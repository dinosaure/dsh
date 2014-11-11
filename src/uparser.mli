include (module type of Parser
         with type token = Parser.token)

val single_ty :
  ('a -> token * Lexing.position * Lexing.position) -> 'a -> Type.t

val single_expr :
  ('a -> token * Lexing.position * Lexing.position) -> 'a -> Ast.t

val exprs :
  ('a -> token * Lexing.position * Lexing.position) -> 'a -> Ast.t
