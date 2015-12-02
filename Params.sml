signature PARAMS = sig
  type pipe =
    string * string

  type t =
    { anim   : bool
    , ncuts  : int
    , path   : string
    , outDir : string
    , tmpDir : string
    , maxDim : int * int
    , rate   : int
    , minReg : int
    , bg     : Img.pixel option
    , log    : bool
    , mirror : bool
    , rawVid : pipe option
    , markov : int option
    , vrni   : bool
    }

  val init : t

  val toString  : t -> string
  val outPrefix : t -> string

  val setAnim   : bool      -> t -> t
  val setNCuts  : int       -> t -> t
  val setPath   : string    -> t -> t
  val setOutDir : string    -> t -> t
  val setTmpDir : string    -> t -> t
  val setMaxDim : int * int -> t -> t
  val setRate   : int       -> t -> t
  val setMinReg : int       -> t -> t
  val setBG     : Img.pixel -> t -> t
  val setLog    : bool      -> t -> t
  val setMirror : bool      -> t -> t
  val setRawVid : pipe      -> t -> t
  val setMarkov : int       -> t -> t
  val setVrni   : bool      -> t -> t
end

structure Params : PARAMS = struct
  infix |>
  fun x |> f = f x

  type pipe =
    string * string

  type t =
    { anim   : bool
    , ncuts  : int
    , path   : string
    , outDir : string
    , tmpDir : string
    , maxDim : int * int
    , rate   : int
    , minReg : int
    , bg     : Img.pixel option
    , log    : bool
    , mirror : bool
    , rawVid : pipe option
    , markov : int option
    , vrni   : bool
    }

  val init =
    { anim   = false
    , ncuts  = 1000
    , path   = ""
    , outDir = "."
    , tmpDir = "/tmp"
    , maxDim = (500, 500)
    , rate   = 40
    , minReg = 25
    , bg     = NONE
    , log    = false
    , mirror = true
    , rawVid = NONE
    , markov = NONE
    , vrni   = false
    }

  fun toString (ps: t) : string = let
    val bg =
      case #bg ps
        of NONE => "NONE"
         | SOME pxl => Img.pixelString pxl
    val rv =
      case #rawVid ps
        of NONE => "NONE"
         | SOME (p1, p2) => "(" ^ p1 ^ ", " ^ p2 ^ ")"
    val markov =
      case #markov ps
        of NONE => "NONE"
         | SOME i => Int.toString i
    val flds =
      [ "anim   = " ^ Bool.toString (#anim ps)
      , "ncuts  = " ^ Int.toString (#ncuts ps)
      , "path   = " ^ #path ps
      , "outDir = " ^ #outDir ps
      , "tmpDir = " ^ #tmpDir ps
      , "maxDim = " ^ Util.i2str (#maxDim ps)
      , "rate   = " ^ Int.toString (#rate ps)
      , "minReg = " ^ Int.toString (#minReg ps)
      , "bg     = " ^ bg
      , "log    = " ^ Bool.toString (#log ps)
      , "mirror = " ^ Bool.toString (#mirror ps)
      , "rawVid = " ^ rv
      , "markov = " ^ markov
      , "vrni   = " ^ Bool.toString (#vrni ps)
      ]
  in
    "{ " ^ String.concatWith "\n, " flds ^ "\n}"
  end

  fun outPrefix (ps: t) : string = let
    val x =
      ps |> #path
         |> OS.Path.splitDirFile
         |> #file
         |> OS.Path.splitBaseExt
         |> #base
    val name =
      if x = ""
      then "anon"
      else x
  in
    OS.Path.joinDirFile
      { dir = #outDir ps
      , file = name ^ "-xcut"
      }
  end

  fun setAnim x (ps: t) : t =
    { anim   = x
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setNCuts x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = x
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setPath x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = x
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setOutDir x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = x
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setTmpDir x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #tmpDir ps
    , tmpDir = x
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }


  fun setMaxDim x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = x
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setRate x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = x
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setMinReg x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = x
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setBG x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = SOME x
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setLog x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = x
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setMirror x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = x
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setRawVid x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = SOME x
    , markov = #markov ps
    , vrni   = #vrni ps
    }

  fun setMarkov x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = SOME x
    , vrni   = #vrni ps
    }

  fun setVrni x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , tmpDir = #tmpDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    , mirror = #mirror ps
    , rawVid = #rawVid ps
    , markov = #markov ps
    , vrni   = x
    }


end
