signature RAND = sig
  val upto    : int -> int
  val elt     : 'a list -> 'a
  val cmp     : 'a -> 'a -> bool
  val shuffle : 'a list -> 'a list
  val name    : int -> string
end

structure Rand :> RAND = struct
  structure LI = LargeInt

  infix |>
  fun x |> f = f x

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

  fun cmp a b =
    upto 2 = 0

  fun shuffle l =
    Util.sort cmp l

  val nameChars =
    String.explode
      ( "abcdefghijklmnopqrstuvwxyz"
      ^ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      ^ "0123456789_"
      )

  fun name n =
    n |> Util.range
      |> List.map (fn _ => elt nameChars)
      |> String.implode
end
