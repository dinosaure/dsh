let interpret expr =
  let lexbuf = Lexing.from_string expr in
  try
    let tree = Parser.single_expr Lexer.token lexbuf in
    let ty = Synthesis.eval Core.core 0 tree in
    let rt = Interpreter.eval ~env:Core.runtime tree in
    Printf.printf ": %s\n%!" (Type.to_string ty);
    Printf.printf "> %s\n%!" (Interpreter.to_string rt);
  with
  | Parser.Error ->
    Printf.printf "Parsing error at:\n%s%!"
      (Location.to_string_of_line
         (Location.make
            (Lexing.lexeme_start_p lexbuf)
            (Lexing.lexeme_end_p lexbuf))
         expr)
  | Synthesis.Error (loc, exn) ->
    Printf.printf "Typing error - %s at:\n%s%!"
      (Synthesis.string_of_exn exn)
      (Location.to_string_of_line loc expr)
  | Lexer.Lexical_error ->
    Printf.printf "Lexical error at:\n%s%!"
      (Location.to_string_of_line
         (Location.make
            (Lexing.lexeme_start_p lexbuf)
            (Lexing.lexeme_end_p lexbuf))
         expr)
  | Interpreter.Error (loc, exn) ->
    Printf.printf "Interpreter error - %s at:\n%s%!"
      (Interpreter.string_of_exn exn)
      (Location.to_string_of_line loc expr)

let () =
  let rec repl () =
    Printf.printf "> %!";
    try interpret (read_line ()); repl ()
    with End_of_file -> ()
  in repl ()
