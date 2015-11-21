exception Assert of string

fun assert msg b : unit =
  if b
  then ()
  else raise (Assert msg)

fun assert_eq msg a b : unit =
  if a = b
  then ()
  else raise (Assert msg)

fun assert_n msg (f: int -> bool) : unit = let
  fun aux () = let
    val i = Rand.upto 1000
  in
    if f i then () else
      raise (Assert (msg ^ " @ i = " ^ Int.toString i))
  end
in
  Util.ntimes aux 100 ()
end

fun randPt n =
  (Rand.upto n, Rand.upto n)

fun randPgon nPts maxPt =
  List.map (fn i => randPt maxPt) (Util.range nPts)

fun randPgonUniq nPts maxPt =
  Util.uniq (randPgon nPts maxPt)

fun randConvex nPts maxPt =
  Pgon.convexHull (randPgonUniq nPts maxPt)

fun test _ = let

  (* PGON TESTS *)

  val t1 =
    [ (1, 1)
    , (2, 0)
    , (0, 0)
    ]

  val _ =
    assert_eq "canon 001"
      t1
      (Pgon.canon t1)

  val _ =
    assert_n "canon 002"
    (fn i => t1 = (Pgon.canon (Util.ntimes Util.rotate i t1)))

  val s1 =
    [ (0, 1)
    , (1, 1)
    , (1, 0)
    , (0, 0)
    ]

  val _ =
    assert_eq "canon 003"
      s1
      (Pgon.canon s1)

  val _ =
    assert_n "canon 004"
    (fn i => s1 = (Pgon.canon (Util.ntimes Util.rotate i s1)))

  val p1 =
    [ (17, 23)
    , (19, 20)
    , (17, 15)
    , (21, 14)
    , (19, 7)
    , (19, 5)
    , (16, 4)
    , (10, 3)
    , (9, 12)
    , (7, 17)
    , (2, 20)
    ]

  val _ =
    assert_eq "canon 005"
      p1
      (Pgon.canon p1)

  val _ =
    assert_n "canon 006"
    (fn i => p1 = (Pgon.canon (Util.ntimes Util.rotate i p1)))

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgonUniq 10 10
        in
          assert_eq ("canon 007.0 @ " ^ Pgon.toString p)
            (Pgon.canon p)
            (Pgon.canon (Util.ntimes Util.rotate (Rand.upto 10) p))
        end
      )
      100
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgonUniq 100 10
        in
          assert_eq ("canon 007.1 @ " ^ Pgon.toString p)
            (Pgon.canon p)
            (Pgon.canon (Util.ntimes Util.rotate (Rand.upto 100) p))
        end
      )
      10
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgonUniq 10 100
        in
          assert_eq ("canon 007.2 @ " ^ Pgon.toString p)
            (Pgon.canon p)
            (Pgon.canon (Util.ntimes Util.rotate (Rand.upto 10) p))
        end
      )
      10
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 10 10
          val pC = Pgon.canon p
          val (x0, y0) = List.hd pC
          val (x1, y1) = List.hd (List.tl pC)
          val (xN, yN) = List.hd (List.rev pC)
        in
          assert ("canon 008.0 @ " ^ Pgon.toString p)
            (List.all (fn (x, y) => y0 >= y) (List.tl pC) andalso
             x1 >= xN andalso
             Util.intersect p pC = p)
        end
      )
      100
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 100 100
          val pC = Pgon.canon p
          val (x0, y0) = List.hd pC
          val (x1, y1) = List.hd (List.tl pC)
          val (xN, yN) = List.hd (List.rev pC)
        in
          assert ("canon 008.1 @ " ^ Pgon.toString p)
            (List.all (fn (x, y) => y0 >= y) (List.tl pC) andalso
             x1 >= xN andalso
             Util.intersect p pC = p)
        end
      )
      10
      ()

  val _ =
    assert_eq "fold 001"
      (Pgon.points t1)
      (Pgon.points (Util.rotate t1))

  val _ =
    assert_eq "fold 002"
      (Pgon.points s1)
      (Pgon.points (Util.rotate s1))

  val _ =
    assert_eq "fold 003"
      (Pgon.points p1)
      (Pgon.points (Util.rotate p1))

  val _ =
    assert_n "fold 004"
    (fn i => Pgon.points t1 = Pgon.points (Util.ntimes Util.rotate i t1))

  val _ =
    assert_n "fold 005"
    (fn i => Pgon.points s1 = Pgon.points (Util.ntimes Util.rotate i s1))

  val _ =
    assert_n "fold 006"
    (fn i => Pgon.points p1 = Pgon.points (Util.ntimes Util.rotate i p1))

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 10 10
          val pts1 = Pgon.points p
          val pts2 = Pgon.points (Util.ntimes Util.rotate (Rand.upto 10) p)
        in
          assert_eq ( "fold 007.0 @ " ^ Pgon.toString p
                    ^ "\n\n" ^ Pgon.toString pts1
                    ^ "\n\n" ^ Pgon.toString pts2)
            pts1
            pts2
        end
      )
      100
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 100 10
          val pts1 = Pgon.points p
          val pts2 = Pgon.points (Util.ntimes Util.rotate (Rand.upto 100) p)
        in
          assert_eq ( "fold 007.1 @ " ^ Pgon.toString p
                    ^ "\n\n" ^ Pgon.toString pts1
                    ^ "\n\n" ^ Pgon.toString pts2)
            pts1
            pts2
        end
      )
      10
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 10 100
          val pts1 = Pgon.points p
          val pts2 = Pgon.points (Util.ntimes Util.rotate (Rand.upto 10) p)
        in
          assert_eq ( "fold 007.2 @ " ^ Pgon.toString p
                    ^ "\n\n" ^ Pgon.toString pts1
                    ^ "\n\n" ^ Pgon.toString pts2)
            pts1
            pts2
        end
      )
      10
      ()

  val s2 =
    [ (0, 0)
    , (0, 4)
    , (4, 4)
    , (4, 0)
    ]

  val s2' =
    [ (0, 0), (0, 2)
    , (0, 4), (2, 4)
    , (4, 4), (4, 2)
    , (4, 0), (2, 0)
    ]

  val _ =
    assert_eq "mids 001"
      (Pgon.addMids s2)
      s2'

  val p1 =
    [ (0, 0)
    , (0, 4)
    , (2, 10)
    , (2, 0)
    ]

  val p1' =
    [ (0, 0)
    , (0, 4)
    , (2, 10)
    , (2, 5)
    , (2, 0)
    ]

  val _ =
    assert_eq "midLongest 001"
      (Pgon.midLongest p1)
      p1'

  val s1_splits =
    [([(0, 1), (1, 1), (1, 0)]
     ,[(1, 0), (0, 0), (0, 1)])
    ,([(1, 1), (1, 0), (0, 0)]
     ,[(0, 0), (0, 1), (1, 1)])
    ]

  val _ = let
    val splits = Pgon.splits s1
    val s =
      Util.foldl (fn s => fn acc => acc ^ "\n" ^ s)
        ""
        (List.map
          (fn (p1, p2) =>
             "(" ^ Pgon.toString p1 ^ ", " ^
                   Pgon.toString p2 ^ ")")
          splits)
  in
    assert_eq ( "splits 001 @ "
              ^ Pgon.toString s1
              ^ "\n\n"
              ^ s)
      splits
      s1_splits
  end

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randConvex 100 1000
          val splits = Pgon.splits p
        in
          assert ( "splits 002 @ " ^ Pgon.toString p)
            (List.all (fn (p1, p2) => Pgon.convexHull (p1 @ p2) = p) splits)
        end
      )
      10
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randConvex 100 1000
          val splits = Pgon.splits p
        in
          assert ( "convex splits 001 @ " ^ Pgon.toString p)
            (List.all
              (fn (p1, p2) => Pgon.isConvexCW p1 andalso
                              Pgon.isConvexCW p2) splits)
        end
      )
      10
      ()

  val _ =
    assert_eq "turn 001"
      (Pgon.turn ((0, 0), (1, 1)) (0, 1))
      Pgon.CCW

  val _ =
    assert_eq "turn 002"
      (Pgon.turn ((0, 0), (1, 1)) (2, 2))
      Pgon.COL

  val _ =
    assert_eq "turn 003"
      (Pgon.turn ((0, 0), (1, 1)) (1, 0))
      Pgon.CW

  val bigbox =
    [ (0, 0)
    , (0, 100)
    , (100, 100)
    , (100, 0)
    ]

  val _ = let
    val pts = List.map
                (fn (x, y) => (x + 1, y + 1))
                (randPgon 1000 98)
    val mess = Rand.shuffle (bigbox @ pts)
    val ch = Pgon.convexHull mess
  in
    assert_eq ("convexHull 001 @ " ^ Pgon.toString ch)
      ch
      bigbox
  end

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 10 10
          val ch = Pgon.convexHull p
        in
          assert ( "convexHull 002.1 @ " ^ Pgon.toString p
                 ^ "\n\n" ^ Pgon.toString ch)
            (Pgon.isConvexCW ch)
        end
      )
      1000
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 100 100
          val ch = Pgon.convexHull p
        in
          assert ( "convexHull 002.2 @ " ^ Pgon.toString p
                 ^ "\n\n" ^ Pgon.toString ch)
            (Pgon.isConvexCW ch)
        end
      )
      100
      ()


  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 1000 1000
          val ch = Pgon.convexHull p
        in
          assert ( "convexHull 002.3 @ " ^ Pgon.toString p
                 ^ "\n\n" ^ Pgon.toString ch)
            (Pgon.isConvexCW ch)
        end
      )
      10
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 10 10
          val ch = Pgon.convexHull p
          val pts1 = Pgon.points p
          val pts2 = Pgon.points ch
        in
          assert ( "convexHull 003.1 @ " ^ Pgon.toString p
                 ^ "\n\n" ^ Pgon.toString ch)
            (Util.subset pts1 pts2)
        end
      )
      1000
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 10 100
          val ch = Pgon.convexHull p
          val pts1 = Pgon.points p
          val pts2 = Pgon.points ch
        in
          assert ( "convexHull 003.2 @ " ^ Pgon.toString p
                 ^ "\n\n" ^ Pgon.toString ch)
            (Util.subset pts1 pts2)
        end
      )
      10
      ()

  val _ =
    Util.ntimes (fn () =>
        let
          val p = randPgon 100 100
          val ch = Pgon.convexHull p
          val pts1 = Pgon.points p
          val pts2 = Pgon.points ch
        in
          assert ( "convexHull 003.3 @ " ^ Pgon.toString p
                 ^ "\n\n" ^ Pgon.toString ch)
            (Util.subset pts1 pts2)
        end
      )
      10
      ()

in
  assert_eq "All tests pass and 0 <> 1."
    0
    1
end

val _ = test ()
  handle Assert msg => print (msg ^ "\n")

