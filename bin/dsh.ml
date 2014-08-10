let interpret expr =
  let lexbuf = Lexing.from_string expr in
  try
    let tree = Parser.single_expr Lexer.token lexbuf in
    let ty = Synthesis.eval ~env:Core.core tree in
    let rt = Interpreter.eval Core.runtime tree in
    Printf.printf "%s : %s\n%!" (Interpreter.to_string rt) (Type.to_string ty);
  with
  | Parser.Error ->
    let loc = Location.make
        (Lexing.lexeme_start_p lexbuf)
        (Lexing.lexeme_end_p lexbuf)
    in Printf.printf "Parsing error at: %s\n%!"
      (Location.to_string_of_line loc expr)
  | Lexer.Lexical_error ->
    let loc = Location.make
        (Lexing.lexeme_start_p lexbuf)
        (Lexing.lexeme_end_p lexbuf)
    in Printf.printf "Lexical error at:\n%s%!"
      (Location.to_string_of_line loc expr)
  | Synthesis.Error (loc, _)
  | Interpreter.Error (loc, _) as exn ->
    Printf.printf "%s at:\n%s%!"
      (Printexc.to_string exn)
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
    Printf.printf "parsing: ok!\n%!";
    let _ = Synthesis.eval ~env:Core.core tree in
    Printf.printf "typing: ok!\n%!";
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
  | Synthesis.Error (loc, _)
  | Interpreter.Error (loc, _) as exn ->
    Printf.printf "%s at %s:\n> %s\n%!"
      (Printexc.to_string exn)
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

let () =
  Printexc.record_backtrace true;
  try
    if Array.length Sys.argv = 2
    then file (open_in Sys.argv.(1)) Sys.argv.(1)
    else repl ()
  with exn ->
    Printf.printf "%s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stderr
