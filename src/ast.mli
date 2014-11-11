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
  | Tuple of (Location.t * t list)
  | Case of (Location.t * t * (Pattern.t * t) list)
  | Variant of (Location.t * string * t)
  | RecordSelect of (Location.t * t * string)
  | RecordExtend of (Location.t * t list Type.Set.t * t)
  | RecordRestrict of (Location.t * t * string)
  | RecordEmpty of Location.t
and annotation = (int list * Type.t)

val loc : t -> Location.t
val is_annotated : t -> bool

val to_string : t -> string
