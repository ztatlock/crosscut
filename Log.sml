signature LOG = sig
  exception Log of string
  val init  : string -> unit
  val log   : string -> unit
  val close : unit -> unit
end

structure Log : LOG = struct
  exception Log of string

  val logFile : TextIO.outstream option ref =
    ref NONE

  val t0 : Time.time ref =
    ref Time.zeroTime

  fun init path = (
    logFile := SOME (TextIO.openOut path);
    t0 := Time.now ()
  )
  handle IO.Io ioe =>
    raise (Log ("init: exception from " ^ (#function ioe)
                ^ " while trying open log: " ^ path))

  fun log msg =
    case !logFile
      of NONE => ()
       | SOME f => let
           val dt = Time.- (Time.now (), !t0)
         in
           TextIO.output (f, "@ " ^ Time.toString dt ^ ":\n");
           TextIO.output (f, msg);
           TextIO.output (f, "\n\n");
           TextIO.flushOut f
         end
  handle IO.Io ioe =>
    raise (Log ("log: exception from " ^ (#function ioe)
                ^ " while trying to log entry: " ^ msg))

  fun close () =
    case !logFile
      of NONE => ()
       | SOME f => (
           TextIO.closeOut f;
           logFile := NONE;
           t0 := Time.zeroTime
         )
  handle IO.Io ioe =>
    raise (Log ("close: exception from " ^ (#function ioe)
                ^ " while trying to write close log"))
end
