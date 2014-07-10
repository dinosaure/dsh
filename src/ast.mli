type t =
  | Var of (Location.t * string)
  | App of (Location.t * t * t list)
  | Abs of (Location.t * (string * annotation option) list * t)
  | Let of (Location.t * string * t * t)
  | Ann of (Location.t * t * annotation)
and annotation = (int list * Type.t)

val is_annotated : t -> bool

val to_string : t -> string
