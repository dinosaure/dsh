type t

val add : string -> Type.t -> t -> t
val lookup : string -> t -> Type.t
val empty : t
