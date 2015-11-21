exception Driver of string

type params =
  { anim  : bool
  , ncuts : int
  , path  : string
  }

fun setAnim ps x =
  { anim  = x
  , ncuts = #ncuts ps
  , path  = #path ps
  }

fun setNCuts ps x =
  { anim  = #anim ps
  , ncuts = x
  , path  = #path ps
  }

fun setPath ps x =
  { anim  = #anim ps
  , ncuts = #ncuts ps
  , path  = x
  }

val initParams =
  { anim  = false
  , ncuts = 0
  , path  = ""
  }

fun parseCL ps [] =
      ps
  | parseCL ps ("--path" :: x :: args) =
      parseCL (setPath ps x) args
  | parseCL ps ("--ncuts" :: x :: args) =
      let val i = valOf (Int.fromString x) in
        parseCL (setNCuts ps i) args
      end
  | parseCL ps ("--anim" :: args) =
      parseCL (setAnim ps true) args
  | parseCL ps (x :: args) =
      raise (Driver ("bogus cmd line: " ^ x))

fun main () = let
  val ps = parseCL initParams (CommandLine.arguments ())
  fun f () =
    if #anim ps
    then Crosscut.xcutAnimate (#path ps) (#ncuts ps)
    else Crosscut.xcut (#path ps) (#ncuts ps)
in
  f ();
  OS.Process.exit OS.Process.success
end

val _ =
  main ()
