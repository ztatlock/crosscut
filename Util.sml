signature UTIL = sig
  exception Util of string
  val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  val nth : 'a list -> int -> 'a
  val ntimes : ('a -> 'a) -> int -> 'a -> 'a
  val until : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
  val extreme : ('a -> 'a -> bool) -> 'a list -> 'a
  val extract : ('a -> 'a -> bool) -> 'a list -> 'a * 'a list
  val insert : ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
  val sort : ('a -> 'a -> bool) -> 'a list -> 'a list
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val unzip : ('a * 'b) list -> 'a list * 'b list
  val zip3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
  val unzip3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
  val pairUp : 'a list -> ('a * 'a) list
  val rotate : 'a list -> 'a list
  val unrotate : 'a list -> 'a list
  val part : ('a -> bool) -> 'a list -> 'a list * 'a list
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
  val mem : ''a -> ''a list -> bool
  val rmSeqRep : ''a list -> ''a list
  val uniq : ''a list -> ''a list
  val startsWith: ''a list -> ''a list -> bool
  val intersect : ''a list -> ''a list -> ''a list
  val subset : ''a list -> ''a list -> bool
  val range : int -> int list
  val square : real -> real
  val round : real -> int
  val cmpR : real -> real -> bool
  val meanR : real list -> real
  val meanI : int list -> int
  val foldl : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
  val iterl : ('a -> unit) -> 'a list -> unit
  val flatMap : ('a -> 'b list) -> 'a list -> 'b list
  val fold2i : (int * 'a * 'b * 'c -> 'c) -> 'c ->
               'a array -> 'b array -> 'c
  val fold2xy : ((int * int) * 'a * 'b * 'c -> 'c) -> 'c ->
                'a array array -> 'b array array -> 'c
  val app2xy : ((int * int) * 'a * 'b -> unit) ->
               'a array array -> 'b array array -> unit
  val bench : (unit -> 'a) -> int
end

structure Util : UTIL = struct
  exception Util of string

  fun curry f a b = f (a, b)
  fun uncurry f (a, b) = f a b
  fun flip f b a = f a b

  fun nth (x::xs) 0 = x
    | nth (x::xs) n = nth xs (n - 1)
    | nth _       _ = raise (Util "nth")

  fun ntimes f 0 x = x
    | ntimes f n x = ntimes f (n - 1) (f x)

  fun until cond f x =
    if cond x
    then x
    else until cond f (f x)

  fun extreme cmp [] = raise (Util "extreme empty list")
    | extreme cmp [x] = x
    | extreme cmp (x1 :: x2 :: xs) =
        if cmp x1 x2
        then extreme cmp (x1 :: xs)
        else extreme cmp (x2 :: xs)

  fun extract cmp [] = raise (Util "extract empty list")
    | extract cmp [x] = (x, [])
    | extract cmp (x :: xs) =
      let
        val (y, ys) = extract cmp xs
      in
        if cmp x y
        then (x, y :: ys)
        else (y, x :: ys)
      end

  fun insert lt x ys = let
    fun loop [] = [x]
      | loop (y :: ys) =
          if lt x y
          then x :: y :: ys
          else y :: loop ys
  in
    loop ys
  end

  fun sort cmp [] = []
    | sort cmp (x::xs) = insert cmp x (sort cmp xs)

  fun zip (x :: xs) (y :: ys) =
        (x, y) :: zip xs ys
    | zip _ _ = []

  fun unzip [] = ([], [])
    | unzip ((x, y) :: xys) = let
        val (xs, ys) = unzip xys
      in
        (x::xs, y::ys)
      end

  fun zip3 (x :: xs) (y :: ys) (z :: zs) =
        (x, y, z) :: zip3 xs ys zs
    | zip3 _ _ _ = []

  fun unzip3 [] = ([], [], [])
    | unzip3 ((x, y, z) :: xyzs) = let
        val (xs, ys, zs) = unzip3 xyzs
      in
        (x::xs, y::ys, z::zs)
      end

  fun pairUp (x :: y :: t) = (x, y) :: pairUp t
    | pairUp _ = []

  fun rotate [] = []
    | rotate (x :: xs) = xs @ [x]

  fun unrotate l = List.rev (rotate (List.rev l))

  fun part pred [] = ([], [])
    | part pred (x :: xs) = let
        val (a, b) = part pred xs
      in
        if pred x then
          (x :: a, b)
        else
          (a, x :: b)
      end

  fun take 0 xs = []
    | take n (x::xs) = x :: take (n - 1) xs
    | take _ _ = raise (Util "bogus take")

  fun drop 0 xs = xs
    | drop n (x::xs) = drop (n - 1) xs
    | drop _ _ = raise (Util "bogus drop")

  fun mem x [] = false
    | mem x (y :: ys) = x = y orelse mem x ys

  fun rmSeqRep [] = []
    | rmSeqRep [x] = [x]
    | rmSeqRep (x1 :: x2 :: xs) =
        if x1 = x2 then
          rmSeqRep (x2 :: xs)
        else
          x1 :: rmSeqRep (x2 :: xs)

  fun uniq [] = []
    | uniq (x :: xs) =
        if mem x xs then
          uniq xs
        else
          x :: uniq xs

  fun startsWith [] ys = true
    | startsWith _ [] = false
    | startsWith (x::xs) (y::ys) =
        x = y andalso startsWith xs ys

  fun intersect xs ys =
    List.filter (flip mem ys) xs

  fun subset xs ys =
    List.all (flip mem ys) xs

  fun range n = let
    fun loop acc 0 = 0 :: acc
      | loop acc n = loop (n :: acc) (n - 1)
  in
    if n <= 0
    then []
    else loop [] (n - 1)
  end

  fun square (r: real) = r * r

  fun round r =
    Real.round r
    (*
    handle Domain => 0 (* TODO bogus *)
    *)

  fun cmpR r1 r2 =
    case Real.compare (r1, r2)
      of LESS => true
       | _ => false

  fun cascadeSum rs = let
    fun loop [] = []
      | loop [x] = [x]
      | loop (x1 :: x2 :: xs) =
          x1 + x2 :: loop xs
  in
    case rs
      of [] => 0.0
       | [x] => x
       | _ => cascadeSum (loop rs)
  end

  fun meanR rs = let
    val s = cascadeSum rs
    val n = Real.fromInt (List.length rs)
  in
    s / n
  end

  fun meanI l =
    round (meanR (List.map Real.fromInt l))

  fun foldl f acc [] = acc
    | foldl f acc (x::xs) = foldl f (f x acc) xs

  fun iterl f [] = ()
    | iterl f (x::xs) = (f x; iterl f xs)

  fun flatMap f [] = []
    | flatMap f (x::xs) = f x @ flatMap f xs

  fun fold2i f init a1 a2 =
    Array.foldli (fn (i, e1, acc) =>
      f (i, e1, Array.sub (a2, i), acc))
    init a1

  fun fold2xy f init a1 a2 =
    fold2i (fn (y, row1, row2, acc) =>
      fold2i (fn (x, e1, e2, acc) =>
        f ((x, y), e1, e2, acc))
      acc row1 row2)
    init a1 a2

  fun app2xy f a1 a2 =
    fold2xy (fn (xy, e1, e2, _) =>
      f (xy, e1, e2))
    () a1 a2

  fun bench f = let
    val t0 = Time.now ()
    val _  = f ()
    val t1 = Time.now ()
    val m  = Time.toMilliseconds (Time.- (t1, t0))
  in
    LargeInt.toInt m
  end
end
