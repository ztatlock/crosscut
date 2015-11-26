signature IMG = sig
  exception Img of string

  type coord = int * int
  type pixel = int * int * int
  type t = pixel array array

  val white : pixel
  val black : pixel
  val red   : pixel
  val green : pixel
  val blue  : pixel

  val pixelString : pixel -> string

  val dim : t -> int * int
  val get : t -> coord -> pixel
  val set : t -> coord -> pixel -> unit
  val mkimg : int * int -> pixel -> t
  val copy : t -> t

  val appXY : (coord -> pixel -> unit) -> t -> unit
  val mixPix : pixel list -> pixel
  val merge : t list -> t

  val rawRead : BinIO.instream -> (int * int) -> t option
  val rawWrite : BinIO.outstream -> t -> unit
end

structure Img : IMG = struct
  exception Img of string

  type coord = int * int
  type pixel = int * int * int
  type t = pixel array array

  val white = (255, 255, 255)
  val black = (  0,   0,   0)
  val red   = (255,   0,   0)
  val green = (  0, 255,   0)
  val blue  = (  0,   0, 255)

  fun pixelString (r, g, b) = let
    val sr = Int.toString r
    val sg = Int.toString g
    val sb = Int.toString b
  in
    "(" ^ sr ^ ", " ^ sg ^ ", " ^ sb ^ ")"
  end

  fun dim img = let
    val h = Array.length img
    val w = if h = 0 then 0
            else Array.length (Array.sub (img, 0))
  in
    (w, h)
  end

  fun get img (x, y) =
    Array.sub (Array.sub (img, y), x)

  fun set img (x, y) p =
    Array.update (Array.sub (img, y), x, p)

  fun mkimg (w, h) p = let
    fun mkrow _ = Array.array (w, p)
  in
    Array.tabulate (h, mkrow)
  end

  fun copy img = let
    val (w, h) = dim img
    val img' = mkimg (w, h) black
    fun copyRow (y, imgY) =
      Array.copy { src = imgY
                 , dst = Array.sub (img', y)
                 , di = 0
                 }
    val _ = Array.appi copyRow img
  in
    img'
  end

  fun appXY f img = let
    fun aux (y, imgY) =
      Array.appi (fn (x, p) => f (x, y) p) imgY
  in
    Array.appi aux img
  end

  fun mixPix ps = let
    val (rs, gs, bs) = Util.unzip3 ps
  in
    ( Util.meanI rs
    , Util.meanI gs
    , Util.meanI bs
    )
  end

  fun merge [] = mkimg (0, 0) black
    | merge imgs = let
        val (w, h) = dim (List.hd imgs)
        val img' = mkimg (w, h) black
        fun mpixels xy _ = let
          val p' = mixPix (List.map (Util.flip get xy) imgs)
        in
          set img' xy p'
        end
        val _ = appXY mpixels img'
      in
        img'
      end

  fun rawRead f (w, h) = let
    val n = w * h * 3
    val d = BinIO.inputN (f, n)
    fun pxl (i, j) = let
      val y = i * w + j
      val x = y * 3
      val r = Word8Vector.sub (d, x + 0)
      val g = Word8Vector.sub (d, x + 1)
      val b = Word8Vector.sub (d, x + 2)
    in
      ( Word8.toInt r
      , Word8.toInt g
      , Word8.toInt b
      )
    end
  in
    if Word8Vector.length d < n
    then NONE
    else SOME (
      Array.tabulate (h, fn i =>
        Array.tabulate (w, fn j =>
          pxl (i, j))))
  end

  fun rawWrite f img = let
    val (w, h) = dim img
    val n = w * h * 3
    fun byt x = let
      val y = x div 3
      val (i, j) = (y mod w, y div w)
      val (r, g, b) = get img (i, j)
    in
      case x mod 3
        of 0 => Word8.fromInt r
         | 1 => Word8.fromInt g
         | 2 => Word8.fromInt b
         | _ => raise (Img "rawWrite: impossible")
    end
    val d = Word8Vector.tabulate (n, byt)
  in
    BinIO.output (f, d)
  end
end
