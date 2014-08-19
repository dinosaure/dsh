module Set : sig
  include (module type of Map.Make(String))

  val of_list : (string * 'a) list -> 'a t
  val to_list : 'a t -> (string * 'a) list

  val iter2 : ((string * 'a) -> (string * 'b) -> unit) -> 'a t -> 'b t -> unit
end

type t =
  | Const of string
  | Primitive of string
  | App of (t * t list)
  | Arrow of (t list * t)
  | Var of var ref
  | Forall of (int list * t)
  | Alias of (string * t)
  | Set of t Set.t
  | Constr of (int list * t)
and var =
  | Unbound of int * int
  | Bound of int
  | Link of t
  | Generic of int

module Primitive : sig
  val add : string -> unit
  val exists : string -> bool

  val int : t
  val char : t
  val bool : t
  val unit : t
end

module Map : sig
  include Map.S with type key = int

  val generate : int -> string
  val extend : string t -> int list -> (string list * string t)
end

val unlink : t -> t
val is_monomorphic : t -> bool

val to_string : ?env:string Map.t -> t -> string
val copy : t -> t
