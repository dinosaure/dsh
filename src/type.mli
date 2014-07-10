type t =
  | Const of string
  | App of (t * t list)
  | Arrow of (t list * t)
  | Var of var ref
  | Forall of (int list * t)
and var =
  | Unbound of int * int
  | Bound of int
  | Link of t
  | Generic of int

module Map : sig
  include Map.S with type key = int

  val extend : string t -> int list -> (string list * string t)
end

val unlink : t -> t
val is_monomorphic : t -> bool

val to_string : ?env:string Map.t -> t -> string
