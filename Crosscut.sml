signature CROSSCUT = sig
  exception Crosscut of string
  val xcut : string -> int -> unit
  val xcutAnimate : string -> int -> unit
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

  type params =
    { minRegion : int
    , minMids   : int
    }

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

  fun regionSplits img (r: region) = let
    val n = 25
    fun hulls (p1, p2) =
      (Pgon.convexHull p1, Pgon.convexHull p2)
    fun filterSmall (p1, p2) acc =
      if Pgon.size p1 > n andalso Pgon.size p2 > n
      then (p1, p2) :: acc
      else acc
    fun mkRegions (p1, p2) =
      (mkRegion img p1, mkRegion img p2)
  in
    r |> #pgon
      |> Pgon.splits
      |> List.map hulls
      |> Util.foldl filterSmall []
      |> List.map mkRegions
  end


  fun xcutStep img ([], dones, seen) = ([], dones, seen)
    | xcutStep img (r::rs, dones, seen) =
        if Util.mem r seen then
          if Util.mem r dones
          then (rs, dones, seen)
          else (rs, r::dones, seen)
        else
          case regionSplits img (padRegMid r)
            of [] => (rs, r::dones, r::seen)
             | splits => let
                 val (r1, r2) = Util.extreme sumErrLt splits
                 val rs' = rs |> Util.insert errorGt r1
                              |> Util.insert errorGt r2
               in
                 (rs', dones, r::seen)
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
    val ramp = 20
  in
    Util.range ramp @
    (List.map (Util.curry op+ ramp) pts)
  end

  fun xcut p n = let
    val img = PPM.read p
    val be = OS.Path.splitBaseExt p
    fun log regs dones i = let
      val p' = OS.Path.joinBaseExt
                {base = #base be ^ "-xcut", ext = #ext be}
      val ic = Img.copy img
      val rs = Util.sort regGt (dones @ regs)
    in
      composite ic rs;
      PPM.write ic p'
    end
    fun loop (regs, dones, seen) i = (
      if i >= n orelse regs = [] then (
        log regs dones i;
        regs @ dones
      ) else (
        loop (xcutStep img (regs, dones, seen)) (i + 1)
      )
    )
    val rSplits = loop ([initRegion img], [], []) 0
  in
    ()
  end
  handle (Util.Util msg) => print ("Util: " ^ msg)

  fun xcutAnimate p n = let
    val img = PPM.read p
    val be = OS.Path.splitBaseExt p
    fun log regs dones i = let
      val pi = StringCvt.padLeft #"0" 5 (Int.toString i)
      val b' = #base be ^ "-xcut-" ^ pi
      val p' = OS.Path.joinBaseExt {base = b', ext = #ext be}
      val ic = Img.copy img
      val rs = Util.sort regGt (dones @ regs)
    in
      composite ic rs;
      PPM.write ic p'
    end
    fun loop (regs, dones, seen) i = (
      if i >= n orelse regs = [] then (
        log regs dones i;
        regs @ dones
      ) else (
        if Util.mem i logPts then log regs dones i else ();
        loop (xcutStep img (regs, dones, seen)) (i + 1)
      )
    )
    val rSplits = loop ([initRegion img], [], []) 0
  in
    ()
  end
  handle (Util.Util msg) => print ("Util: " ^ msg)
end
