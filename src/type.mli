module Environment : sig
  include Map.S with type key = int

  val generate : int -> string
  val extend : string t -> int list -> (string list * string t)
end

module Set : sig
  include (module type of Map.Make(String))

  val add_list : 'a t -> (string * 'a) list -> 'a t

  val of_list : (string * 'a) list -> 'a t
  val to_list : 'a t -> (string * 'a) list

  val iter2 : ((string * 'a) -> (string * 'b) -> unit) -> 'a t -> 'b t -> unit
  val merge : ('a list) t -> ('a list) t -> ('a list) t
end

type t =
  | Const of string                       (* like `int` *)
  | App of (t * t list)                   (* like `(list int)` *)
  | Arrow of (t list * t)                 (* like `int -> int` *)
  | Var of var ref                        (* variable of type *)
  | Forall of (int list * t)              (* like `(forall (l) t)` *)
  | Alias of (string * t)                 (* alias of type *)
  | Variant of row                        (* like `[ A | B ]` *)
  | Record of row                         (* like `{ a; b }` *)
  | RowEmpty
  | RowExtend of ((t list) Set.t * row)
  | Abs of (string list * t)
and var =
  | Unbound of int * int
  | Bound of int
  | Link of t
  | Generic of int
and row = t

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
val tuple : t list -> t

val compact : row -> row list Set.t * row
val is_close : t -> bool
val close_row : t -> unit
val bound : t -> t -> t * t
val unlink : t -> t
val is_monomorphic : t -> bool

val to_string : ?env:string Environment.t -> t -> string
val copy : t -> t
val normalize : t -> t
