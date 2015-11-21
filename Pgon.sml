signature PGON = sig
  exception Pgon of string

  type point = int * int
  type line = point * point
  type t = point list

  val toString : t -> string
  val canon : t -> t
  val fold : (point -> 'a -> 'a) -> 'a -> t -> 'a
  val points : t -> point list
  val size : t -> int
  val appXY : (point -> unit) -> t -> unit
  val addMids : t -> t
  val midLongest : t -> t
  val splits : t -> (t * t) list

  datatype turn = CCW | COL | CW
  val turn : line -> point -> turn
  val convexHull : t -> t
  val isConvexCW : t -> bool

  val neighbor : t -> t -> bool
  val join : t -> t -> t
end

structure Pgon : PGON = struct
  exception Pgon of string

  type point = int * int
  type line = point * point
  type t = point list

  fun xLt (x1, y1) (x2, y2) =
    x1 < x2 orelse (x1 = x2 andalso y1 < y2)

  fun xGt (x1, y1) (x2, y2) =
    x1 > x2 orelse (x1 = x2 andalso y1 > y2)

  fun yLt (x1, y1) (x2, y2) =
    y1 < y2 orelse (y1 = y2 andalso x1 < x2)

  fun yGt (x1, y1) (x2, y2) =
    y1 > y2 orelse (y1 = y2 andalso x1 > x2)

  fun length ((x1, y1), (x2, y2)) = let
    val dx = x2 - x1
    val dy = y2 - y1
    val dx2 = Real.fromInt (dx * dx)
    val dy2 = Real.fromInt (dy * dy)
  in
    Math.sqrt (dx2 + dy2)
  end

  fun longer l1 l2 =
    length l1 > length l2

  fun ptSub (x1, y1) (x2, y2) =
    (x1 - x2, y1 - y2)

  fun ptAdd (x1, y1) (x2, y2) =
    (x1 + x2, y1 + y2)

  fun ptCross (x1, y1) (x2, y2) =
    (x1 * y2) - (y1 * x2)

  fun ptDot (x1, y1) (x2, y2) =
    (x1 * x2) + (y1 * y2)

  datatype turn = CCW | COL | CW

  fun turn (pt1, pt2) pt3 = let
    val t = ptCross (ptSub pt2 pt1) (ptSub pt3 pt1)
  in
    if t > 0 then
      CCW
    else if t = 0 then
      COL
    else
      CW
  end

  fun inTri (v1, v2, v3) pt = let
    val t1 = turn (v1, v2) pt
    val t2 = turn (v2, v3) pt
    val t3 = turn (v3, v1) pt
  in
    t1 = t2 andalso t2 = t3 orelse
    t1 = COL orelse t2 = COL orelse t3 = COL
  end

  fun lnPtDist ((x1, y1), (x2, y2)) (x, y) = let
    val dy = y2 - y1
    val dx = x2 - x1
    val n = Real.fromInt
      (dy * x - dx * y + x2 * y1 - y2 * x1)
    val d = Real.fromInt
      (dy * dy + dx * dx)
  in
    (Real.abs n) / (Math.sqrt d)
  end

  fun sides l [] = ([], [], [])
    | sides l (pt :: pts) = let
        val (ccws, cols, cws) = sides l pts
      in
        case turn l pt
          of CCW => (pt :: ccws, cols, cws)
           | COL => (ccws, pt :: cols, cws)
           | CW  => (ccws, cols, pt :: cws)
      end

  fun convexHull p = let
    fun further l pt1 pt2 =
      lnPtDist l pt1 > lnPtDist l pt2
    fun aux (v1, v2) [] = []
      | aux (v1, v2) cs = let
          val (piv, cs) = Util.extract (further (v1, v2)) cs
          val cs = List.filter (fn pt => not (inTri (v1, piv, v2) pt)) cs
          val (ccws, _, cws) = sides (v1, piv) cs
        in
          aux (v1, piv) ccws @ piv :: aux (piv, v2) cws
        end
    val (ptL, p) = Util.extract xLt p
    val (ptR, p) = Util.extract xGt p
    val (ccws, _, cws) = sides (ptL, ptR) p
  in
    ptL :: aux (ptL, ptR) ccws @ ptR :: aux (ptR, ptL) cws
  end

  fun isConvexCW p = let
    fun loop (v1 :: v2 :: v3 :: vs) = let
          val t = turn (v1, v2) v3
        in
          (t = CW orelse t = COL) andalso
          loop (v2 :: v3 :: vs)
        end
      | loop _ = true
  in
    loop (p @ p)
  end

  fun ptString (x, y) = let
    val sx = Int.toString x
    val sy = Int.toString y
  in
    "(" ^ sx ^ ", " ^ sy ^ ")"
  end

  fun toString p = let
    val sp = String.concatWith ", " (List.map ptString p)
  in
    "[" ^ sp ^ "]"
  end

  infix |>
  fun x |> f = f x

  (* may not be uniq if points are repeated *)
  fun canon p = let
    fun rotN n = Util.ntimes Util.rotate n p
    val nPts = List.length p
    val rots = List.map rotN (Util.range nPts)
    fun startsHigher ((xA, yA) :: _) ((xB, yB) :: _) =
          yA > yB orelse (yA = yB andalso xA < xB)
      | startsHigher _ _ =
          false
    val highest = Util.extreme startsHigher rots
    val clockwise = let
      val (xNext, _) = Util.nth highest 1
      val (xLast, _) = Util.nth highest (nPts - 1)
    in
      if xNext > xLast
      then highest
      else List.rev (Util.rotate highest)
    end
  in
    clockwise
  end

  fun orderLineY ((x1, y1), (x2, y2)) =
    if y1 < y2
    then ((x1, y1), (x2, y2))
    else ((x2, y2), (x1, y1))

  fun lineEdgeY l = let
    val ((x0, y0), (xN, yN)) = orderLineY l
    val dx = xN - x0
    val dy = yN - y0
    val mi = Real.fromInt dx / Real.fromInt dy
    val x0_r = Real.fromInt x0
    fun mkPt i =
      ( Real.round (x0_r + Real.fromInt i * mi)
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
          val ys = List.map #2 pts
          val y0 = Util.extreme (Util.curry op<) ys
          val yN = Util.extreme (Util.curry op>) ys
          val t = Array.array (yN - y0 + 1, [])
          fun aux (x, y) = let
            val i = y - y0
            val xs = Array.sub (t, i)
          in
            Array.update (t, i, Util.insert (Util.curry op<) x xs)
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

  fun points p =
    fold (Util.curry op::) [] p

  fun size p =
    List.length (points p)

  fun appXY f p =
    fold (fn pt => fn _ => f pt) () p

  fun midPt (x1, y1) (x2, y2) =
    ( Util.meanI [x1, x2]
    , Util.meanI [y1, y2]
    )

  fun edges p =
    Util.zip p (Util.rotate p)

  fun addMids p = let
    fun aux (xy1, xy2) p' =
      midPt xy1 xy2 :: xy1 :: p'
  in
    List.rev
      (Util.foldl aux [] (edges p))
  end

  fun midLongest p = let
    val es = edges p
    val l = Util.extreme longer es
    fun aux (xy1, xy2) p' =
      if (xy1, xy2) = l then
        midPt xy1 xy2 :: xy1 :: p'
      else
        xy1 :: p'
  in
    List.rev
      (Util.foldl aux [] es)
  end

  fun lSubArr a i j =
    if i < j
    then Array.sub (a, i) :: lSubArr a (i + 1) j
    else []

  fun splits p = let
    val n = List.length p
    fun splits i j =
      if j < n + i - 1 andalso j < n
      then (i, j) :: splits i (j + 1)
      else []
    fun loop i =
      if i < n - 2
      then splits i (i + 2) @ loop (i + 1)
      else []
    val a = Array.fromList (p @ p)
    fun pieces (i, j) =
      ( lSubArr a i (j + 1)
      , lSubArr a j (n + i + 1))
  in
    List.map pieces (loop 0)
  end

  (* not reflexive! *)
  fun neighbor p1 p2 = let
    val common = Util.intersect p1 p2
  in
    p1 <> p2 andalso List.length common >= 2
  end

  (* buggy! *)
  (* maybe works for non-overlapping pgons that share 1 set of contiguous points? *)
  fun join p1 p2 = let
    val _ = (
      print "join:\n";
      print "p1:\n";
      print (toString p1);
      print "\n";
      print "p2:\n";
      print (toString p2);
      print "\n";
      print "\n\n";
      TextIO.flushOut TextIO.stdOut
    )
    val p1 = Util.until
              (fn x => not ((Util.mem (List.hd x)) p2))
              Util.rotate
              (canon p1)
    val p2 = Util.until
              (fn x => not ((Util.mem (List.hd x)) p1))
              Util.rotate
              (canon p2)
    val common = Util.intersect p1 p2
    val p1' = Util.until
                (Util.startsWith common)
                Util.rotate
                p1
    val p2' = Util.until
                (Util.startsWith (List.rev common))
                Util.rotate
                p2
    val nc = List.length common
    val part1 = Util.drop (nc - 1) p1'
    val part2 = Util.drop (nc - 1) p2'
  in
    part1 @ part2
  end

end
