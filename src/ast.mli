type t =
  | Var of (Location.t * string)
  | App of (Location.t * t * t list)
  | Abs of (Location.t * string list * t)
  | Let of (Location.t * string * t * t)

val to_string : t -> string
