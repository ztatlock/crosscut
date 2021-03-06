structure P = Params

signature VORONOI = sig
  exception Voronoi of string
  val voronoi : P.t -> unit
end

structure Voronoi : VORONOI = struct
  exception Voronoi of string
  infix |>
  fun x |> f = f x

  fun manhattan (x1, y1) (x2, y2) = let
    val dx = x1 - x2
    val dy = y1 - y2
  in
    Int.abs dx + Int.abs dy
  end

  fun mcloser pt pt1 pt2 = let
    val d1 = manhattan pt pt1
    val d2 = manhattan pt pt2
  in
    d1 < d2
  end

  fun distance (x1, y1) (x2, y2) = let
    val dx = x1 - x2
    val dy = y1 - y2
    val dx2 = Real.fromInt (dx * dx)
    val dy2 = Real.fromInt (dy * dy)
  in
    Math.sqrt (dx2 + dy2)
  end

  fun closer pt pt1 pt2 = let
    val d1 = distance pt pt1
    val d2 = distance pt pt2
  in
    d1 < d2
  end

  fun voronoiRegs img sites = let
    val sr = Array.fromList
              (List.map (fn s => (s, [])) sites)
    val n = Array.length sr
    fun addPt site pt = let
      fun loop i =
        if i >= n then () else let
          val (s, pts) = Array.sub (sr, i)
        in
          if site = s then
            Array.update (sr, i, (s, pt :: pts))
          else
            loop (i + 1)
        end
    in
      loop 0
    end
    val (w, h) = Img.dim img
    val (s, ss) =
      case sites
        of s :: ss => (s, ss)
         | _ => raise (Voronoi "empty sites")
    val _ = let
      fun loop (x, y) =
        if x >= w then
          ()
        else if y >= h then
          loop (x + 1, 0)
        else let
          fun aux (s, (sMin, dMin)) = let
            val d = manhattan (x, y) s
          in
            if d < dMin
            then (s, d)
            else (sMin, dMin)
          end
          val (s, _) =
            List.foldl aux (s, manhattan (x, y) s) ss
        in
          addPt s (x, y);
          loop (x, y + 1)
        end
    in
      loop (0, 0)
    end
  in
    Array.foldl (fn ((_, pts), acc) => pts :: acc) [] sr
  end

  fun avgPix img pts = let
    fun aux ((x, y), (sr, sg, sb, n)) = let
      val (r, g, b) = Img.get img (x, y)
    in
      (sr + r, sg + g, sb + b, n + 1)
    end
    val (sr, sg, sb, n) =
      List.foldl aux (0, 0, 0, 0) pts
    val rn = Real.fromInt n
  in
    ( Util.round (Real.fromInt sr / rn)
    , Util.round (Real.fromInt sg / rn)
    , Util.round (Real.fromInt sb / rn)
    )
  end

  fun render img canvas sites = let
    val regs = voronoiRegs img sites
    fun renderReg (pts, ()) = let
      val color = avgPix img pts
      fun aux (pt, ()) =
        Img.set canvas pt color
    in
      List.foldl aux () pts
    end
  in
    List.foldl renderReg () (voronoiRegs img sites)
  end

  fun voronoiStep img sites = let
    val (w, h) = Img.dim img
    val pt = (Rand.upto w, Rand.upto h)
  in
    pt :: sites
  end

  fun voronoi (params: P.t) = let
    val img = Crosscut.readImg params

    fun writeFrame sites path = let
      val canvas =
        case #bg params
          of SOME color => Img.mkimg (Img.dim img) color
           | NONE => Img.copy img
    in
      render img canvas sites;
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
    fun writeAnimFrame sites i = let
      val af = tmpName i
    in
      if #anim params andalso Util.mem i Crosscut.logPts then (
        Log.log ("writing animation frame: " ^ af);
        writeFrame sites af;
        animFrames := af :: !animFrames
      ) else ()
    end

    fun loop i sites = (
      Log.log ("voronoi loop i = " ^ Int.toString i);
      writeAnimFrame sites i;
      if i >= #ncuts params then
        sites
      else
        loop (i + 1) (voronoiStep img sites)
    )

    val (w, h) = Img.dim img
    val sites = loop 0 [(w div 2, h div 2)]
    val _ = Log.log ("end voronoi loop, total sites: " ^
                       Int.toString (List.length sites))

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
      writeFrame sites (outName ^ ".ppm")
    )
  end
end
