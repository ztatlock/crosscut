structure P = Params

signature VORONOI = sig
  exception Voronoi of string
  val voronoi : P.t -> unit
end

structure Voronoi : VORONOI = struct
  exception Voronoi of string
  infix |>
  fun x |> f = f x

  fun voronoiStep img sites =
    sites

  fun voronoi (params: P.t) = let
    val img = Crosscut.readImg params

    fun writeFrame regs path = let
      val canvas =
        case #bg params
          of SOME color => Img.mkimg (Img.dim img) color
           | NONE => Img.copy img
    in
      composite canvas regs;
      PPM.write canvas path
    end

    val tmpPrefix =
      "voronoi-" ^ Rand.name 10 ^ "-"
    fun tmpName i = let
      val pi = StringCvt.padLeft #"0" 5 (Int.toString i)
    in
      OS.Path.joinDirFile
        { dir = #tmpDir params
        , file = tmpPrefix ^ pi ^ ".ppm"
        }
    end

    val animFrames : string list ref =
      ref []
    fun writeAnimFrame regs dones i = let
      val af = tmpName i
    in
      if #anim params andalso Util.mem i Crosscut.logPts then (
        Log.log ("writing animation frame: " ^ af);
        writeFrame (dones @ regs) af;
        animFrames := af :: !animFrames
      ) else ()
    end

    fun loop i sites = (
      Log.log ("voronoi loop i = " ^ Int.toString i);
      writeAnimFrame regs dones i;
      if i >= #ncuts params then
        sites
      else
        loop (i + 1) (voronoiStep img sites)
    )

    val regs = loop 0 []
    val _ = Log.log ("end voronoi loop, total regions: " ^
                       Int.toString (List.length regs))

    val outName = let
      val s = Int.toString (#ncuts params)
      val p = StringCvt.padLeft #"0" 5 s
    in
      P.outPrefix params ^ "-" ^ p
    end
  in
    if #anim params then let
      val frames =
        if #mirror params
        then List.rev (!animFrames) @ (!animFrames)
        else List.rev (!animFrames)
    in
      Log.log "animate frames";
      Crosscut.animate
        frames
        (#rate params)
        outName;

      Log.log "remove animation frames";
      Util.iterl OS.FileSys.remove (!animFrames)
    end
    else (
      Log.log "write final output frame";
      (* single quote outname *)
      writeFrame regs ("'" ^ outName ^ ".ppm'")
    )
  end
end
