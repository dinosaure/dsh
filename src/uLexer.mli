exception Lexical_error

val parse : unit ->
  ((Sedlexing.lexbuf -> UParser.token * Lexing.position * Lexing.position) *
   (Sedlexing.lexbuf -> Lexing.position) *
   (Sedlexing.lexbuf -> Lexing.position))

val to_string : UParser.token -> string
