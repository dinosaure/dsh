type t =
  | Var of (Location.t * string)
  | Variant of (Location.t * string * t)
  | Tuple of (Location.t * t list)
  | Int of (Location.t * int)
  | Char of (Location.t * char)
  | Bool of (Location.t * bool)
  | Unit of Location.t

val to_string : t -> string
