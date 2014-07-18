type t =
  | Var of (Location.t * string)
  | App of (Location.t * t * t list)
  | Abs of (Location.t * (string * annotation option) list * t)
  | Let of (Location.t * string * t * t)
  | Rec of (Location.t * string * t * t)
  | Ann of (Location.t * t * annotation)
  | If of (Location.t * t * t * t)
  | Int of (Location.t * int)
  | Bool of (Location.t * bool)
  | Char of (Location.t * char)
and annotation = (int list * Type.t)

type i =
  | Def of (Location.t * string * t)
  | Expr of (Location.t * t)

val is_annotated : t -> bool

val to_string : t -> string
