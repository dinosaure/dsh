type t =
  | Var of string
  | App of t * t list
  | Abs of string list * t
  | Let of string * t * t

val to_string : t -> string
