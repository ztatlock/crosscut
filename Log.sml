signature LOG = sig
  exception Log of string
  val init  : string option -> unit
  val log   : string -> unit
  val close : unit -> unit
end

structure Log : LOG = struct
  exception Log of string

  val logFile : TextIO.outstream option ref = ref NONE

  fun init NONE = ()
    | init (SOME path) =
        logFile := SOME (TextIO.openOut path)

  fun log msg =
    case !logFile
      of NONE => ()
       | SOME f => (
           TextIO.output (f, "@ " ^ Time.toString (Time.now ()) ^ ": \n");
           TextIO.output (f, msg);
           TextIO.output (f, "\n\n")
         )

  fun close () =
    case !logFile
      of NONE => ()
       | SOME f => TextIO.closeOut f
end
