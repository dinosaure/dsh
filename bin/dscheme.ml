let () =
  let a = Parser.single_expr
      Lexer.token
      (Lexing.from_string "(fst [one, true])")
  in
  let t = W.eval Core.core 0 a in
  let t = W.generalization (-1) t in
  Printf.printf "%s\n" (Type.to_string t);
  Printf.printf "%s\n" (Ast.to_string a)
