module Environment : sig
  include (module type of Map.Make (String))

  val extend : 'a t -> string list -> 'a list -> 'a t
end

type t =
  | Int of int
  | Bool of bool
  | Closure of (t Environment.t * string list * Ast.t * string option)
  | Primitive of (t list -> t)

exception Unbound_variable of string
exception Expected_function

val to_string : t -> string
val eval : ?env:(t Environment.t) -> Ast.t -> t
