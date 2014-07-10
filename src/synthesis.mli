module Environment : sig
  include (module type of Map.Make (String))

  val extend : Type.t t -> string -> Type.t -> Type.t t
  val lookup : Type.t t -> string -> Type.t
end

module Variable : sig
  val next : unit -> int
  val reset : unit -> unit

  val make : int -> Type.t
  val generic : unit -> Type.t
  val bound : unit -> (int * Type.t)
end

exception Recursive_type of Type.t
exception Conflict of (Type.t * Type.t)
exception Circularity of (Type.t * Type.t)
exception Expected_argument of (Type.t * int)
exception Expected_function of Type.t
exception Unbound_variable of string
exception Variable_no_instantiated
exception Polymorphic_parameter_inferred of Type.t list
exception Is_not_instance of (Type.t * Type.t)

val string_of_exn : exn -> string

val unification : Type.t -> Type.t -> unit
val generalization : int -> Type.t -> Type.t
val specialization : int -> Type.t -> Type.t

val eval : Type.t Environment.t -> int -> Ast.t -> Type.t
