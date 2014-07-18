module Environment : sig
  include (module type of Map.Make (String))

  val extend : 'a t -> string list -> 'a list -> 'a t
end

type t =
  | Int of int
  | Bool of bool
  | Char of char
  | Unit
  | Closure of (t Environment.t * string list * Ast.t * string option)
  | Primitive of (t list -> t)

exception Unbound_variable of string
exception Expected_function
exception Expected_boolean
exception Error of (Location.t * exn)

val string_of_exn : exn -> string

val to_string : t -> string

val eval : t Environment.t -> Ast.t -> t
val top : t Environment.t -> Ast.i list -> unit
