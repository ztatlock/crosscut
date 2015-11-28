structure P = Params

signature MARKOV = sig
  exception Markov of string

  type t

  val mkModel : string -> int -> t
  val gen : t -> int -> int -> string
end

structure Markov : MARKOV = struct
  exception Markov of string
  infix |>
  fun x |> f = f x

  structure StringListKey : ORD_KEY = struct
    type ord_key = string list
    val compare = List.collate String.compare
  end

  structure StringListMap : ORD_MAP =
    BinaryMapFn(StringListKey)

  type t =
    string list StringListMap.map

  structure Map = StringListMap

  fun mkModel path keySize = let
    fun loop model ws =
      case Util.split keySize ws
        of SOME (key, x::xs) => let
             val model' =
               case Map.find (model, key)
                 of NONE => Map.insert (model, key, [x])
                  | SOME l => Map.insert (model, key, x :: l)
           in
             loop model' (List.tl ws)
           end
         | _ => model
  in
    path
      |> Util.read
      |> String.translate (fn c =>
          if Char.isPunct c orelse c = #"\n" then
            " "
          else
            Char.toString c)
      |> String.map Char.toLower
      |> String.tokens Char.isSpace
      |> List.filter (fn w =>
          String.size w > 1 orelse w = "a" orelse w = "i")
      |> loop Map.empty
  end

  fun gen model keySize n = let
    val dump =
      List.concat (Map.listItems model)
    fun loop acc 0 _ = List.rev acc
      | loop acc i seed = let
          val w =
            case Map.find (model, seed)
              of NONE => Rand.elt dump
               | SOME l => Rand.elt l
        in
          loop (w::acc) (i - 1) (List.tl seed @ [w])
        end
  in
    keySize
      |> Util.range
      |> List.map (fn _ => Rand.elt dump)
      |> loop [] n
      |> String.concatWith " "
  end
end
