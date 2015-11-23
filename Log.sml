signature LOG = sig
  val init  : string -> unit
  val log   : string -> unit
  val close : unit -> unit
end

structure Log : LOG = struct
  val logFile : TextIO.outstream option ref =
    ref NONE

  val t0 : Time.time ref =
    ref Time.zeroTime

  fun init path = (
    logFile := SOME (TextIO.openOut path);
    t0 := Time.now ()
  )

  fun log msg =
    case !logFile
      of NONE => ()
       | SOME f => let
           val dt = Time.- (Time.now (), !t0)
         in
           TextIO.output (f, "@ " ^ Time.toString dt ^ ": \n");
           TextIO.output (f, msg);
           TextIO.output (f, "\n\n")
         end

  fun close () =
    case !logFile
      of NONE => ()
       | SOME f => (
           TextIO.closeOut f;
           logFile := NONE;
           t0 := Time.zeroTime
         )
end
