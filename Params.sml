signature PARAMS = sig
  type t =
    { anim   : bool
    , ncuts  : int
    , path   : string
    , outDir : string
    , maxDim : int * int
    , rate   : int
    , minReg : int
    }

  val init : t

  val setAnim   : bool      -> t -> t
  val setNCuts  : int       -> t -> t
  val setPath   : string    -> t -> t
  val setOutDir : string    -> t -> t
  val setMaxDim : int * int -> t -> t
  val setRate   : int       -> t -> t
  val setMinReg : int       -> t -> t
end

structure Params : PARAMS = struct
  type t =
    { anim   : bool
    , ncuts  : int
    , path   : string
    , outDir : string
    , maxDim : int * int
    , rate   : int
    , minReg : int
    }

  val init =
    { anim   = false
    , ncuts  = 1000
    , path   = ""
    , outDir = "."
    , maxDim = (500, 500)
    , rate   = 30
    , minReg = 25
    }

  fun setAnim x (ps: t) : t =
    { anim   = x
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    }

  fun setNCuts x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = x
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    }

  fun setPath x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = x
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    }

  fun setOutDir x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = x
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = #minReg ps
    }

  fun setMaxDim x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = x
    , rate   = #rate ps
    , minReg = #minReg ps
    }

  fun setRate x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = x
    , minReg = #minReg ps
    }

  fun setMinReg x (ps: t) : t =
    { anim   = #anim ps
    , ncuts  = #ncuts ps
    , path   = #path ps
    , outDir = #outDir ps
    , maxDim = #maxDim ps
    , rate   = #rate ps
    , minReg = x
    }
end
