let interpret expr =
  let lexbuf = Lexing.from_string expr in
  try
    let tree = Parser.single_expr Lexer.token lexbuf in
    let ty = Synthesis.eval Core.core 0 tree
             |> Synthesis.generalization (-1) in
    Printf.printf "=> %s\n%!" (Ast.to_string tree);
    Printf.printf "=> %s\n%!" (Type.to_string ty);
  with
  | Parser.Error ->
    Printf.printf "Parsing error at: %s\n%!"
      (Location.to_string (Location.make
                             (Lexing.lexeme_start_p lexbuf)
                             (Lexing.lexeme_end_p lexbuf)))

let () =
  let rec repl () =
    try interpret (read_line ()); repl ()
    with End_of_file -> ()
  in repl ()
