structure P = Params

signature CROSSCUT = sig
  exception Crosscut of string
  val xcut : P.t -> unit
end

structure Crosscut : CROSSCUT = struct
  exception Crosscut of string
  infix |>
  fun x |> f = f x

  fun lt  (x: int)  y = x < y
  fun gt  (x: int)  y = x > y
  fun zlt (x: real) y = x < y
  fun zgt (x: real) y = x > y

  fun fill p c img =
    Pgon.appXY (fn xy => Img.set img xy c) p

  fun composite img [] = img
    | composite img (r :: rs) = (
        fill (#pgon r) (#mean r) img;
        composite img rs)

  type region =
    { pgon  : Pgon.t
    , mean  : Img.pixel
    , error : int
    , size  : int
    }

  fun regionString (r: region) = let
    val sp = Pgon.toString (#pgon r)
    val sm = Img.pixelString (#mean r)
    val se = Int.toString (#error r)
    val sz = Int.toString (#size r)
  in
    "{ pgon  = " ^ sp ^ "\n" ^
    ", mean  = " ^ sm ^ "\n" ^
    ", error = " ^ se ^ "\n" ^
    ", size  = " ^ sz ^ "\n" ^
    "}"
  end

  fun regLt (r1: region) (r2: region) =
    #size r1 < #size r2

  fun regGt (r1: region) (r2: region) =
    #size r1 > #size r2

  fun errorGt (r1: region) (r2: region) =
    #error r1 > #error r2

  fun sumErrLt (rp1: region * region) (rp2: region * region) = let
    val e1 = #error (#1 rp1) + #error (#2 rp1)
    val e2 = #error (#1 rp2) + #error (#2 rp2)
  in
    e1 < e2
  end

  fun pgonMeanN img pgon = let
    fun aux (x, y) (sr, sg, sb, n) = let
      val (r, g, b) = Img.get img (x, y)
    in
      (sr + r, sg + g, sb + b, n + 1)
    end
    val (sr, sg, sb, n) =
      Pgon.fold aux (0, 0, 0, 0) pgon
    val rn = Real.fromInt n
  in
    ( ( Util.round (Real.fromInt sr / rn)
      , Util.round (Real.fromInt sg / rn)
      , Util.round (Real.fromInt sb / rn)
      )
    , n
    )
  end

  fun pgonError img pgon (mr, mg, mb) = let
    fun aux (x, y) esum = let
      val (r, g, b) = Img.get img (x, y)
      val dr = Int.abs (mr - r)
      val dg = Int.abs (mg - g)
      val db = Int.abs (mb - b)
    in
      esum + dr + dg + db
    end
  in
    Pgon.fold aux 0 pgon
  end

  fun mkRegion img pgon = let
    val (mean, size) = pgonMeanN img pgon
    val error = pgonError img pgon mean
  in
    { pgon  = pgon
    , mean  = mean
    , error = error
    , size  = size
    }
  end

  fun padRegMid r = let
    fun pad p =
      if List.length p < 8
      then pad (Pgon.midLongest p)
      else p
  in
    { pgon  = pad (#pgon r)
    , mean  = #mean r
    , error = #error r
    , size  = #size r
    }
  end

  fun regionSplits img minReg oldSize (r: region) = let
    val p = #pgon r
    fun hulls (p1, p2) =
      (Pgon.convexHull p1, Pgon.convexHull p2)
    fun filterBogus (p1, p2) acc = let
      val s1 = Pgon.size p1
      val s2 = Pgon.size p2
    in
      if s1 > minReg  andalso s2 > minReg andalso
         s1 < oldSize andalso s2 < oldSize
      then (p1, p2) :: acc
      else acc
    end
    fun mkRegions (p1, p2) =
      (mkRegion img p1, mkRegion img p2)
  in
    p |> Pgon.splits
      |> List.map hulls
      |> Util.foldl filterBogus []
      |> List.map mkRegions
  end

  fun xcutStep img minReg ([], dones, seen) = ([], dones, seen)
    | xcutStep img minReg (r::rs, dones, seen) =
        if Util.mem r seen then (
          Log.log ("previously seen region:\n" ^ regionString r);
          if Util.mem r dones then (
            Log.log "repeat already in dones";
            (rs, dones, seen)
          ) else (
            Log.log "adding repeat to dones";
            (rs, r::dones, seen)
          )
        ) else let
            val paddedR = padRegMid r
          in
            Log.log ("splitting region:\n" ^ regionString r);

            if r = paddedR
            then Log.log "no padding added"
            else Log.log ("adding padding:\n" ^ regionString paddedR);

            case regionSplits img minReg (#size r) paddedR
              of [] => (
                   Log.log "no splits, adding to dones";
                   (rs, r::dones, r::seen)
                 )
               | splits => let
                   val (r1, r2) = Util.extreme sumErrLt splits
                   val rs' = rs |> Util.insert errorGt r1
                                |> Util.insert errorGt r2
                 in
                   Log.log ("split region into:\n" ^
                              regionString r1 ^
                              "\n\nand:\n" ^
                              regionString r2);
                   (rs', dones, r::seen)
                 end
          end

  fun initRegion img = let
    val (w, h) = Img.dim img
    val p0 = [(0, 0), (w, 0), (w, h), (0, h)]
  in
    mkRegion img p0
  end

  val logPts = let
    fun loop i [] =
          loop (i + 1) [i]
      | loop i (x :: xs) =
          if x > 10000
          then List.rev xs
          else loop (i + 1) (x + i :: x :: xs)
    val pts = loop 0 []
    val ramp = 10
  in
    Util.range ramp @
    (List.map (Util.curry op+ ramp) pts)
  end

  fun readImg (params: P.t) = let
    val tmp =
      OS.Path.joinDirFile
        { dir = #tmpDir params
        , file = "xcut-" ^ Rand.name 10 ^ ".ppm"
        }
    val (w, h) = #maxDim params
    val dim = Int.toString w ^ "x" ^ Int.toString h
    val cmd =
      "convert"
      ^ " -strip"
      ^ " -depth 8"
      ^ " -resize " ^ dim
      (* single quote path *)
      ^ " '" ^ #path params ^ "'"
      ^ " " ^ tmp
    val status =
      cmd |> OS.Process.system
          |> Posix.Process.fromStatus
    val success =
      Posix.Process.fromStatus (OS.Process.success)
  in
    if status = success then
      let
        val img = PPM.read tmp
      in
        OS.FileSys.remove tmp;
        Log.log ("read image, size = " ^ Util.i2str (Img.dim img));
        img
      end
    else
      raise (Crosscut ("readImg command failed:\n" ^ cmd))
  end

  fun animate frames rate out = let
    val fs =
      (* single quote frames *)
      "'" ^ String.concatWith "' '" frames ^ "'"
    val cmd =
      "convert"
      ^ " -delay " ^ Int.toString rate
      ^ " -loop 0"
      ^ " " ^ fs
      (* single quote gif *)
      ^ " '" ^ out ^ ".gif'"
    val status =
      cmd |> OS.Process.system
          |> Posix.Process.fromStatus
    val success =
      Posix.Process.fromStatus (OS.Process.success)
  in
    if status = success then
      ()
    else
      raise (Crosscut ("animate command failed:\n" ^ cmd))
  end

  fun xcut (params: P.t) = let
    val img = readImg params

    fun writeFrame regs path = let
      val canvas =
        case #bg params
          of SOME color => Img.mkimg (Img.dim img) color
           | NONE => Img.copy img
    in
      composite canvas regs;
      PPM.write canvas path
    end

    val tmpPrefix =
      "xcut-" ^ Rand.name 10 ^ "-"
    fun tmpName i = let
      val pi = StringCvt.padLeft #"0" 5 (Int.toString i)
    in
      OS.Path.joinDirFile
        { dir = #tmpDir params
        , file = tmpPrefix ^ pi ^ ".ppm"
        }
    end

    val animFrames : string list ref =
      ref []
    fun writeAnimFrame regs dones i = let
      val af = tmpName i
    in
      if #anim params andalso Util.mem i logPts then (
        Log.log ("writing animation frame: " ^ af);
        writeFrame (dones @ regs) af;
        animFrames := af :: !animFrames
      ) else ()
    end

    fun loop i (regs, dones, seen) = (
      Log.log ("xcut loop i = " ^ Int.toString i);
      writeAnimFrame regs dones i;
      if i >= #ncuts params orelse regs = [] then
        regs @ dones
      else
        loop (i + 1) (xcutStep img (#minReg params) (regs, dones, seen))
    )

    val regs = loop 0 ([initRegion img], [], [])
    val _ = Log.log ("end xcut loop, total regions: " ^
                       Int.toString (List.length regs))

    val outName = let
      val s = Int.toString (#ncuts params)
      val p = StringCvt.padLeft #"0" 5 s
    in
      P.outPrefix params ^ "-" ^ p
    end
  in
    if #anim params then let
      val frames =
        if #mirror params
        then List.rev (!animFrames) @ (!animFrames)
        else List.rev (!animFrames)
    in
      Log.log "animate frames";
      animate
        frames
        (#rate params)
        outName;

      Log.log "remove animation frames";
      Util.iterl OS.FileSys.remove (!animFrames)
    end
    else (
      Log.log "write final output frame";
      (* single quote outname *)
      writeFrame regs ("'" ^ outName ^ ".ppm'")
    )
  end
end
