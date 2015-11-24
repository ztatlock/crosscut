signature PPM = sig
  exception PPM of string
  val read : string -> Img.t
  val write : Img.t -> string -> unit
end

structure PPM : PPM = struct
  exception PPM of string

  (* scanExpect : 'a -> ('a, 's) reader -> ('a, 's) reader *)
  fun scanExpect expect rdr s =
    case rdr s
      of NONE => NONE
       | SOME (e, s) =>
    if e = expect
    then SOME (e, s)
    else NONE

  (* scan2 : ('a, 's) reader -> ('a * 'a, 's) reader *)
  fun scan2 rdr s =
    case rdr s
      of NONE => NONE
       | SOME (e1, s) =>
    case rdr s
      of NONE => NONE
       | SOME (e2, s) =>
    SOME ((e1, e2), s)

  (* scan3 : ('a, 's) reader -> ('a * 'a * 'a, 's) reader *)
  fun scan3 rdr s =
    case rdr s
      of NONE => NONE
       | SOME (e1, s) =>
    case rdr s
      of NONE => NONE
       | SOME (e2, s) =>
    case rdr s
      of NONE => NONE
       | SOME (e3, s) =>
    SOME ((e1, e2, e3), s)

  (* scanN : int -> ('a, 's) reader -> ('a list, 's) reader *)
  fun scanN n rdr s = let
    fun loop 0 acc s = SOME (List.rev acc, s)
      | loop i acc s =
        case rdr s
          of NONE => NONE
           | SOME (e, s') => loop (i-1) (e::acc) s'
  in
    if n < 0
    then NONE
    else loop n [] s
  end

  (* scanPixel : (char, 's) reader -> (pixel, 's) reader *)
  fun scanPixel getc s =
    case scan3 getc s
      of NONE => NONE
       | SOME ((c1, c2, c3), s) =>
    SOME ((Char.ord c1, Char.ord c2, Char.ord c3), s)

  (* scanPPM : (char, 's) reader -> (img, 's) reader *)
  fun scanPPM getc s =
    case scanExpect (#"P", #"6") (scan2 getc) s
      of NONE => raise (PPM "scanPPM: bad magic number")
       | SOME (_, s) =>
    case StringCvt.skipWS getc s
      of s =>
    case Int.scan StringCvt.DEC getc s
      of NONE => raise (PPM "scanPPM: bad width")
       | SOME (w, s) =>
    case StringCvt.skipWS getc s
      of s =>
    case Int.scan StringCvt.DEC getc s
      of NONE => raise (PPM "scanPPM: bad height")
       | SOME (h, s) =>
    case StringCvt.skipWS getc s
      of s =>
    case scanExpect 255 (Int.scan StringCvt.DEC getc) s
      of NONE => raise (PPM "scanPPM: color max not 255")
       | SOME (_, s) =>
    case scanExpect #"\n" getc s
      of NONE => raise (PPM "scanPPM: non '\n' after color max")
       | SOME (_, s) =>
    case scanN h (scanN w (scanPixel getc)) s
      of NONE => raise (PPM "scanPPM: bad raster")
       | SOME (r, s) =>
    case Array.fromList (List.map Array.fromList r)
      of i =>
    SOME (i, s)

  fun read p = let
    val f = TextIO.openIn p
    val x = TextIO.scanStream scanPPM f
    val _ = TextIO.closeIn f
  in
    case x
      of NONE => raise (PPM ("read: couldn't read: " ^ p))
       | SOME i => i
  end
  handle IO.Io ioe =>
    raise (PPM ("read: exception from " ^ (#function ioe)
                ^ " while trying to read: " ^ p))


  fun write (i: Img.t) p = let
    val f = TextIO.openOut p
    fun write s = TextIO.output (f, s)
    fun write1 c = TextIO.output1 (f, c)
  in
    write "P6\n";
    write (Int.toString (Array.length (Array.sub (i, 0))));
    write " ";
    write (Int.toString (Array.length i));
    write "\n255\n";
    Array.app (Array.app (fn (r, g, b) => (
      write1 (Char.chr r);
      write1 (Char.chr g);
      write1 (Char.chr b)))) i;
    TextIO.closeOut f
  end
  handle IO.Io ioe =>
    raise (PPM ("write: exception from " ^ (#function ioe)
                ^ " while trying to write: " ^ p))
end
