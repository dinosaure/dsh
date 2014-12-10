module Environment : sig
  include (module type of Map.Make (String))

  val extend : 'a t -> string list -> 'a list -> 'a t
end

type t =
  | Int of int
  | Bool of bool
  | Char of char
  | Tuple of t list
  | Unit
  | List of t list
  | Closure of (t Environment.t * string list * Ast.t * string option)
  | Primitive of (t list -> t)
  | Variant of (string * t)
  | Record of ((string * t) list)

exception Unbound_variable of string
exception Expected_function
exception Expected_boolean
exception Error of (Loc.t * exn)

val to_string : t -> string

val eval : t Environment.t -> Ast.t -> t
