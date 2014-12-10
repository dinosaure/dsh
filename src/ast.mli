type t =
  | Var of (Loc.t * string)
  | App of (Loc.t * t * t list)
  | Abs of (Loc.t * (string * annotation option) list * t)
  | Let of (Loc.t * string * t * t)
  | Rec of (Loc.t * string * t * t)
  | Ann of (Loc.t * t * annotation)
  | If of (Loc.t * t * t * t)
  | Seq of (Loc.t * t * t)
  | Unit of Loc.t
  | Int of (Loc.t * int)
  | Bool of (Loc.t * bool)
  | Char of (Loc.t * char)
  | Alias of (Loc.t * string * Type.t * t)
  | Tuple of (Loc.t * t list)
  | Case of (Loc.t * t * (Pattern.t * t) list)
  | Variant of (Loc.t * string * t)
  | RecordSelect of (Loc.t * t * string)
  | RecordExtend of (Loc.t * t list Type.Set.t * t)
  | RecordRestrict of (Loc.t * t * string)
  | RecordEmpty of Loc.t
and annotation = (int list * Type.t)

val loc : t -> Loc.t
val is_annotated : t -> bool

val to_string : t -> string
