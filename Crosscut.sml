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

  fun readImg path (maxW, maxH) = let
    val tmp = Rand.name 100 ^ ".ppm"
    val dim = Int.toString maxW ^ "x" ^ Int.toString maxH
    val cmd =
      "convert"
      ^ " -depth 8"
      ^ " -resize " ^ dim
      ^ " " ^ path
      ^ " " ^ tmp
    val _ = OS.Process.system cmd
    val img = PPM.read tmp
  in
    OS.FileSys.remove tmp;
    img
  end

  fun animate frames rate out = let
    val cmd =
      "convert"
      ^ " -delay " ^ Int.toString rate
      ^ " -loop 0"
      ^ " " ^ String.concatWith " " frames
      ^ " " ^ out ^ ".gif"
  in
    OS.Process.system cmd
  end

  fun filename path =
    path |> OS.Path.splitDirFile
         |> #file
         |> OS.Path.splitBaseExt
         |> #base

  fun padI i =
    StringCvt.padLeft #"0" 5 (Int.toString i)

  fun xcut (params: P.t) = let
    val img = readImg (#path params) (#maxDim params)
    val (w, h) = Img.dim img
    val _ =
      Log.log ("read image, size = (" ^
                 Int.toString w ^ ", " ^ Int.toString h ^ ")")

    val logged : string list ref = ref []
    val outPref =
      OS.Path.joinDirFile
       { dir = #outDir params
       , file = filename (#path params) ^ "-xcut-"
       }
    fun log regs dones i = let
      val canvas =
        case #bg params
          of SOME color => Img.mkimg (w, h) color
           | NONE => Img.copy img
      val rs = Util.sort regGt (dones @ regs)
      val out = outPref ^ padI i ^ ".ppm"
    in
      composite canvas rs;
      PPM.write canvas out;
      logged := out :: !logged
    end

    fun loop i (regs, dones, seen) = (
      Log.log ("xcut loop iter " ^ Int.toString i);
      if i >= #ncuts params then (
        Log.log "reached ncuts iterations";
        log regs dones i
      ) else if regs = [] then (
        Log.log "reached empty regs";
        log regs dones i
      ) else (
        if #anim params andalso Util.mem i logPts
        then log regs dones i else ();

        loop (i + 1)
          (xcutStep img (#minReg params) (regs, dones, seen))
      )
    )
  in
    Log.log "begin xcut loop";
    loop 0 ([initRegion img], [], []);

    if #anim params then (
      Log.log "animate frames";
      animate
        (List.rev (!logged))
        (#rate params)
        (outPref ^ padI (#ncuts params));

      Log.log "remove animation frames";
      Util.iterl OS.FileSys.remove (!logged)
    ) else ()
  end
end
