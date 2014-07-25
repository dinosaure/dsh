let interpret expr =
  let lexbuf = Lexing.from_string expr in
  try
    let tree = Parser.single_expr Lexer.token lexbuf in
    let ty = Synthesis.eval ~env:Core.core tree in
    let rt = Interpreter.eval Core.runtime tree in
    Printf.printf "%s : %s\n%!" (Interpreter.to_string rt) (Type.to_string ty);
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

let rec repl () =
  begin
    Printf.printf "> %!";
    try interpret (read_line ()); repl ()
    with End_of_file -> ()
  end

let rec file inch filename =
  let lexbuf = Lexing.from_channel inch in
  try
    let tree = Parser.exprs Lexer.token lexbuf in
    let _ = Interpreter.eval Core.runtime tree in
    ()
  with
  | Parser.Error ->
    let loc = Location.make
        (Lexing.lexeme_start_p lexbuf)
        (Lexing.lexeme_end_p lexbuf)
    in
    Printf.printf "Parsing error at %s:\n> %s\n%!"
      (Location.to_string loc)
      (Location.to_string_of_file loc filename)
  | Synthesis.Error (loc, exn) ->
    Printf.printf "Typing error - %s at %s:\n> %s\n%!"
      (Synthesis.string_of_exn exn)
      (Location.to_string loc)
      (Location.to_string_of_file loc filename)
  | Lexer.Lexical_error ->
    let loc = Location.make
        (Lexing.lexeme_start_p lexbuf)
        (Lexing.lexeme_end_p lexbuf)
    in
    Printf.printf "Lexical error at %s:\n> %s\n%!"
      (Location.to_string loc)
      (Location.to_string_of_file loc filename)
  | Interpreter.Error (loc, exn) ->
    Printf.printf "Interpreter error - %s at %s:\n> %s\n%!"
      (Interpreter.string_of_exn exn)
      (Location.to_string loc)
      (Location.to_string_of_file loc filename)

let () =
  Printexc.record_backtrace true;
  try
    if Array.length Sys.argv = 2
    then file (open_in Sys.argv.(1)) Sys.argv.(1)
    else repl ()
  with exn ->
    Printf.fprintf stderr "E: %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stderr
