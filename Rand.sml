signature RAND = sig
  val upto : int -> int
  val elt : 'a list -> 'a
  val shuffle : 'a list -> 'a list
end

structure Rand :> RAND = struct
  structure LI = LargeInt

  val rand__ = let
    fun imod (i, m) = LI.toInt (LI.mod (i, LI.toLarge m))
    val us = Time.toMicroseconds (Time.now ())
  in
    Random.rand (imod(us, 137), imod(us, 139))
  end

  fun upto i =
    Random.randRange (0, i-1) rand__

  fun elt l =
    Util.nth l (upto (List.length l))

  fun shuffle l =
    Util.sort (fn _ => fn _ => upto 2 = 0) l
end
