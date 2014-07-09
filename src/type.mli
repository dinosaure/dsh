type t =
  | Const of string
  | App of (t * t list)
  | Arrow of (t list * t)
  | Var of var ref
and var =
  | Unbound of int * int
  | Link of t
  | Generic of int

val to_string : t -> string
