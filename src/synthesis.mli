module Environment : sig
  include (module type of Map.Make (String))

  val extend : Type.t t -> string -> Type.t -> Type.t t
  val lookup : Type.t t -> string -> Type.t
end

module TPattern : sig
  type t

  val of_pattern : Type.t -> Pattern.t -> t
  val to_string : t -> string

  val pressure_variants : ?def:bool -> t list list -> bool
end

exception Recursive_type of Type.t
exception Conflict of (Type.t * Type.t)
exception Circularity of (Type.t * Type.t)
exception Mismatch_arguments of Type.t
exception Expected_function of Type.t
exception Unbound_variable of string
exception Variable_no_instantiated
exception Polymorphic_argument_inferred of Type.t list
exception No_instance of (Type.t * Type.t)
exception Unknown_type of string
exception Unbound_constructor of string
exception Error of (Loc.t * exn)

val unification : Type.t -> Type.t -> unit
val generalization : int -> Type.t -> Type.t
val specialization : int -> Type.t -> Type.t

val eval :
  ?gamma:Gamma.t ->
  ?env:Type.t Environment.t ->
  ?level:int -> Ast.t -> Type.t

val compute_pattern :
  Gamma.t ->
  (Environment.key * Type.t) list ->
  int ->
  Pattern.t ->
  Type.t * (Environment.key * Type.t) list
