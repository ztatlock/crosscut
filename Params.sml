signature PARAMS = sig
  type t =
    { anim   : bool
    , ncuts  : int
    , path   : string
    , outDir : string
    , maxDim : int * int
    , rate   : int
    , minReg : int
    , bg     : Img.pixel option
    , log    : bool
    }

  val init : t

  val toString  : t -> string
  val outPrefix : t -> string

  val setAnim   : bool      -> t -> t
  val setNCuts  : int       -> t -> t
  val setPath   : string    -> t -> t
  val setOutDir : string    -> t -> t
  val setMaxDim : int * int -> t -> t
  val setRate   : int       -> t -> t
  val setMinReg : int       -> t -> t
  val setBG     : Img.pixel -> t -> t
  val setLog    : bool      -> t -> t
end

structure Params : PARAMS = struct
  infix |>
  fun x |> f = f x

  type t =
    { anim   : bool
    , ncuts  : int
    , path   : string
    , outDir : string
    , maxDim : int * int
    , rate   : int
    , minReg : int
    , bg     : Img.pixel option
    , log    : bool
    }

  val init =
    { anim   = false
    , ncuts  = 1000
    , path   = ""
    , outDir = "."
    , maxDim = (500, 500)
    , rate   = 30
    , minReg = 25
    , bg     = NONE
    , log    = false
    }

  fun toString (ps: t) : string = let
    val maxDim =
      "("  ^ Int.toString (#1 (#maxDim ps)) ^
      ", " ^ Int.toString (#2 (#maxDim ps)) ^
      ")"
    val bg =
      case #bg ps
        of NONE => "NONE"
         | SOME pxl => Img.pixelString pxl
    val flds =
      [ "anim   = " ^ Bool.toString (#anim ps)
      , "ncuts  = " ^ Int.toString (#ncuts ps)
      , "path   = " ^ #path ps
      , "outDir = " ^ #outDir ps
      , "maxDim = " ^ maxDim
      , "rate   = " ^ Int.toString (#rate ps)
      , "minReg = " ^ Int.toString (#minReg ps)
      , "bg     = " ^ bg
      , "log    = " ^ Bool.toString (#log ps)
      ]
  in
    "{ " ^ String.concatWith "\n, " flds ^ "\n}"
  end

  fun outPrefix (ps: t) : string = let
    val name =
      ps |> #path
         |> OS.Path.splitDirFile
         |> #file
         |> OS.Path.splitBaseExt
         |> #base
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
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    }

  fun setNCuts x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = x
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    }

  fun setPath x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = x
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    }

  fun setOutDir x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = x
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    }

  fun setMaxDim x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = x
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    }

  fun setRate x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = x
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = #log ps
    }

  fun setMinReg x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = x
    , bg     = #bg ps
    , log    = #log ps
    }

  fun setBG x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = SOME x
    , log    = #log ps
    }

  fun setLog x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    , bg     = #bg ps
    , log    = x
    }
end
