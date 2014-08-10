type t =
  | Var of (Location.t * string)
  | App of (Location.t * t * t list)
  | Abs of (Location.t * (string * annotation option) list * t)
  | Let of (Location.t * string * t * t)
  | Rec of (Location.t * string * t * t)
  | Ann of (Location.t * t * annotation)
  | If of (Location.t * t * t * t)
  | Seq of (Location.t * t * t)
  | Unit of Location.t
  | Int of (Location.t * int)
  | Bool of (Location.t * bool)
  | Char of (Location.t * char)
  | Alias of (Location.t * string * Type.t * t)
  | Variant of (Location.t * string * t)
and annotation = (int list * Type.t)

val loc : t -> Location.t
val is_annotated : t -> bool

val to_string : t -> string
