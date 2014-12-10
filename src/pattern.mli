type t =
  | Var of (Loc.t * string)
  | Any of Loc.t
  | Variant of (Loc.t * string * t)
  | Tuple of (Loc.t * t list)
  | Int of (Loc.t * int)
  | Char of (Loc.t * char)
  | Bool of (Loc.t * bool)
  | Unit of Loc.t

val to_string : t -> string
