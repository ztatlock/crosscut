structure P = Params

signature XCUTRAW = sig
  exception XCutRaw of string
  val xcut : BinIO.instream * BinIO.outstream * P.t -> unit
end

structure XCutRaw : XCUTRAW = struct
  exception XCutRaw of string
  infix |>
  fun x |> f = f x

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

  fun orderLineY ((x1, y1), (x2, y2)) =
    if y1 < y2
    then ((x1, y1), (x2, y2))
    else ((x2, y2), (x1, y1))

  fun lineEdgeY l = let
    val ((x0, y0), (xN, yN)) = orderLineY l
    val dx = xN - x0
    val dy = yN - y0
    val mi = Real32.fromInt dx / Real32.fromInt dy
    val x0_r = Real32.fromInt x0
    fun mkPt i =
      ( Real32.round (x0_r + Real32.fromInt i * mi)
      , y0 + i
      )
  in
    List.map mkPt (Util.range dy)
  end

  fun edgesY p =
    p |> Util.rotate
      |> Util.zip p
      |> List.map lineEdgeY
      |> List.concat

  fun fold f init p = let
    fun groupSortedY [] = []
      | groupSortedY pts = let
          fun aux ((x, y), (y0, yN)) =
            if y < y0 then
              (y, yN)
            else if y > yN then
              (y0, y)
            else
              (y0, yN)
          val yStart = #2 (List.hd pts)
          val (y0, yN) = List.foldl aux (yStart, yStart) pts
          val t = Array.array (yN - y0 + 1, [])
          fun insX x [] = [x]
            | insX x (h::t) =
                if x < h
                then x :: h :: t
                else h :: insX x t
          fun aux (x, y) = let
            val i = y - y0
            val xs = Array.sub (t, i)
          in
            Array.update (t, i, insX x xs)
          end
        in
          Util.iterl aux pts;
          Array.foldli
            (fn (i, xs, acc) => (y0 + i, xs) :: acc)
            []
            t
        end
    fun scanX y ((x0, xN), acc) = let
      fun loop (i, acc) =
        if i >= xN
        then acc
        else loop (i + 1, f (i, y) acc)
    in
      loop (x0, acc)
    end
    fun aux ((y, xs), acc) =
      List.foldl (scanX y) acc (Util.pairUp xs)
  in
    p |> edgesY
      |> groupSortedY
      |> List.foldl aux init
  end


  fun pgonMeanN img pgon = let
    fun aux (x, y) (sr, sg, sb, n) = let
      val (r, g, b) = Img.get img (x, y)
    in
      (sr + r, sg + g, sb + b, n + 1)
    end
    val (sr, sg, sb, n) =
      fold aux (0, 0, 0, 0) pgon
  in
    if n = 0 then
      ((0, 0, 0), 0)
    else
      ((sr div n, sg div n, sb div n), n)
  end

  fun pgonError img pgon (mr, mg, mb) n = let
    val skip = (n div 16) + 2
    fun aux (x, y) (i, esum) =
      if i mod skip = 0 then
        let
          val (r, g, b) = Img.get img (x, y)
          val dr = Int.abs (mr - r)
          val dg = Int.abs (mg - g)
          val db = Int.abs (mb - b)
        in
          (i + 1, esum + dr + dg + db)
        end
      else
        (i + 1, esum)
    val (_, esum) =
      fold aux (0, 0) pgon
  in
    skip * esum
  end

  fun mkRegion img pgon = let
    val (mean, size) = pgonMeanN img pgon
    val error = pgonError img pgon mean size
  in
    { pgon  = pgon
    , mean  = mean
    , error = error
    , size  = size
    }
  end

  fun padRegMid r = let
    fun pad n p =
      if n <= 0
      then p
      else pad (n - 1) (Pgon.midLongest p)
    val p = #pgon r
    val n = List.length p
    val upper =
      if #size r > 400 then
        4
      else if #size r > 300 then
        5
      else if #size r > 200 then
        6
      else
        7
  in
    { pgon  = pad (upper - n) p
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

  fun xcutStep img minReg ([], dones) = ([], dones)
    | xcutStep img minReg (r::rs, dones) = let
        val paddedR = padRegMid r
      in
        case regionSplits img minReg (#size r) paddedR
          of [] => (rs, r::dones)
           | splits => let
               val (r1, r2) = Util.extreme sumErrLt splits
               val rs' = rs |> Util.insert errorGt r1
                            |> Util.insert errorGt r2
             in
               (rs', dones)
             end
      end

  fun initRegion img = let
    val (w, h) = Img.dim img
    val p0 = [(0, 0), (w, 0), (w, h), (0, h)]
  in
    mkRegion img p0
  end

  fun xcut (fIn, fOut, params: P.t) =
    case Img.rawRead fIn (#maxDim params)
      of NONE => ()
       | SOME img => let
            fun loop i (regs, dones) = (
              Log.log ("xcutRaw loop i = " ^ Int.toString i);
              if i >= #ncuts params then
                dones @ regs
              else
                loop (i + 1) (xcutStep img (#minReg params) (regs, dones))
            )
            val regs = loop 0 ([initRegion img], [])
          in
            Img.rawWrite fOut (composite img regs);
            xcut (fIn, fOut, params)
         end
end
