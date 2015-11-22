structure P = Params

exception Driver of string

fun intify x =
  case Int.fromString x
    of SOME i => i
     | NONE => raise (Driver ("bogus int: " ^ x))

fun parseCmdLine () = let
  fun loop [] ps = ps
    | loop ("--anim" :: xs) ps =
        loop xs (P.setAnim true ps)
    | loop ("--ncuts" :: x :: xs) ps =
        loop xs (P.setNCuts (intify x) ps)
    | loop ("--path" :: x :: xs) ps =
        loop xs (P.setPath x ps)
    | loop ("--outDir" :: x :: xs) ps =
        loop xs (P.setOutDir x ps)
    | loop ("--maxDim" :: w :: h :: xs) ps =
        loop xs (P.setMaxDim (intify w, intify h) ps)
    | loop ("--rate" :: x :: xs) ps =
        loop xs (P.setRate (intify x) ps)
    | loop ("--minReg" :: x :: xs) ps =
        loop xs (P.setMinReg (intify x) ps)
    | loop (x :: xs) ps =
        raise (Driver ("bogus arg: " ^ x))
in
  loop (CommandLine.arguments ()) P.init
end

fun main () = (
  Crosscut.xcut (parseCmdLine ());
  OS.Process.exit OS.Process.success
)

fun println x =
  print (x ^ "\n")

val _ =
  main ()
  handle Util.Util msg =>
          println ("[Util] " ^ msg)
       | PPM.PPM msg =>
           println ("[PPM] " ^ msg)
       | Crosscut.Crosscut msg =>
           println ("[Crosscut] " ^ msg)
       | Driver msg =>
           println ("[Driver] " ^ msg)
