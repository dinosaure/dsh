type t
type position

val make : Lexing.position -> Lexing.position -> t
val compose : position -> position -> t
val dummy : t

val start : t -> position
val stop : t -> position

val to_string_of_file : t -> string -> string
val to_string_of_line : t -> string -> string
val to_string : t -> string
