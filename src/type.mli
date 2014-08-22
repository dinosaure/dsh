module Environment : sig
  include Map.S with type key = int

  val generate : int -> string
  val extend : string t -> int list -> (string list * string t)
end

module Set : sig
  include (module type of Map.Make(String))

  val of_list : (string * 'a) list -> 'a t
  val to_list : 'a t -> (string * 'a) list

  val iter2 : ((string * 'a) -> (string * 'b) -> unit) -> 'a t -> 'b t -> unit
end

type t =
  | Const of string
  | App of (t * t list)
  | Arrow of (t list * t)
  | Var of var ref
  | Forall of (int list * t)
  | Alias of (string * t)
  | Set of t Set.t
  | Abs of (string list * t)
and var =
  | Unbound of int * int
  | Bound of int
  | Link of t
  | Generic of int

module Variable : sig
  val next : unit -> int
  val reset : unit -> unit

  val make : int -> t
  val generic : unit -> t
  val bound : unit -> (int * t)
end

val int : t
val char : t
val bool : t
val unit : t
val tuple : t

val unlink : t -> t
val is_monomorphic : t -> bool

val to_string : ?env:string Environment.t -> t -> string
val copy : t -> t
val normalize : t -> t
