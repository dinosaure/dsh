type t

module Alias : sig
  val lookup : string -> t -> Type.t
end

module Datatype : sig
  val exists : string -> t -> bool
  val lookup : string -> t -> (string * Type.t)
end

exception Constructor_already_exists of string

val add : string -> Type.t -> t -> t
val lookup : string -> t -> Type.t
val empty : t
