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
    | loop ("--tmpDir" :: x :: xs) ps =
        loop xs (P.setTmpDir x ps)
    | loop ("--maxDim" :: w :: h :: xs) ps =
        loop xs (P.setMaxDim (intify w, intify h) ps)
    | loop ("--maxDim2" :: x :: xs) ps =
        loop xs (P.setMaxDim (intify x, intify x) ps)
    | loop ("--rate" :: x :: xs) ps =
        loop xs (P.setRate (intify x) ps)
    | loop ("--minReg" :: x :: xs) ps =
        loop xs (P.setMinReg (intify x) ps)
    | loop ("--bg" :: r :: g :: b :: xs) ps =
        loop xs (P.setBG
          (intify r, intify g, intify b) ps)
    | loop ("--log" :: xs) ps =
        loop xs (P.setLog true ps)
    | loop ("--noMirror" :: xs) ps =
        loop xs (P.setMirror false ps)
    | loop ("--rawVid" :: p1 :: p2 :: xs) ps =
        loop xs (P.setRawVid (p1, p2) ps)
    | loop ("--markov" :: x :: xs) ps =
        loop xs (P.setMarkov (intify x) ps)
    | loop ("--vrni" :: xs) ps =
        loop xs (P.setVrni true ps)
    | loop (x :: xs) ps =
        raise (Driver ("bogus arg: " ^ x))
in
  loop (CommandLine.arguments ()) P.init
end

fun main () = let
  val ps = parseCmdLine ()
in
  if #log ps
  then Log.init (P.outPrefix ps ^ ".log")
  else ();
  Log.log ("params = \n" ^ P.toString ps);

  case #rawVid ps
    of SOME (p1, p2) => let
         val f1 = BinIO.openIn p1
         val f2 = BinIO.openOut p2
       in
         XCutRaw.xcut (f1, f2, ps)
       end
     | NONE =>
  case #markov ps
    of SOME keySize => let
         val model = Markov.mkModel (#path ps) keySize
         val gen = Markov.gen model keySize 1000
       in
         print gen
       end
     | NONE =>
  case #vrni ps
    of true => (
         Log.log "begin voronoi";
         Voronoi.voronoi ps;
         Log.log "end voronoi"
       )
     | false => (
         Log.log "begin xcut";
         Crosscut.xcut ps;
         Log.log "end xcut"
       );

  Log.close ();
  OS.Process.exit OS.Process.success
end

fun println x =
  print (x ^ "\n")

val _ =
  (main () handle e => (Log.close (); raise e))
  handle Util.Util msg =>
          println ("[Util] " ^ msg)
       | Img.Img msg =>
           println ("[Img] " ^ msg)
       | PPM.PPM msg =>
           println ("[PPM] " ^ msg)
       | Log.Log msg =>
           println ("[Log] " ^ msg)
       | Crosscut.Crosscut msg =>
           println ("[Crosscut] " ^ msg)
       | XCutRaw.XCutRaw msg =>
           println ("[XCutRaw] " ^ msg)
       | Driver msg =>
           println ("[Driver] " ^ msg)
