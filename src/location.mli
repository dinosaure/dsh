type t

val make : Lexing.position -> Lexing.position -> t
val dummy : t

val to_string_of_file : t -> string -> string
val to_string_of_line : t -> string -> string
val to_string : t -> string
