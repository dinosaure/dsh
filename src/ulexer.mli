exception Lexical_error

val parse : unit ->
  ((Sedlexing.lexbuf -> Uparser.token * Lexing.position * Lexing.position) *
   (Sedlexing.lexbuf -> Lexing.position) *
   (Sedlexing.lexbuf -> Lexing.position))

val to_string : Uparser.token -> string
