let interpret expr =
  let lexbuf = Sedlexing.Utf8.from_string expr in
  let token, start, stop = Ulexer.parse () in
  try
    let tree = Uparser.single_expr token lexbuf in
    let ty = Synthesis.eval ~env:Core.core tree in
    (* let rt = Interpreter.eval Core.runtime tree in *)
    Printf.printf "%s : %s\n%!" expr (Type.to_string ty);
  with
  | Uparser.Error ->
    let loc = Location.make
        (start lexbuf)
        (stop lexbuf)
    in Printf.printf "Parsing error at:\n%s\n%!"
      (Location.to_string_of_line loc expr)
  | Ulexer.Lexical_error ->
    let loc = Location.make
        (start lexbuf)
        (stop lexbuf)
    in Printf.printf "Lexical error at:\n%s%!"
      (Location.to_string_of_line loc expr)
  | Synthesis.Error (loc, _) as exn ->
    Printf.printf "Typing error: %s at:\n%s%!"
      (Printexc.to_string exn)
      (Location.to_string_of_line loc expr)
  | Interpreter.Error (loc, _) as exn ->
    Printf.printf "Interpreter error: %s at:\n%s%!"
      (Printexc.to_string exn)
      (Location.to_string_of_line loc expr)

let rec repl () =
  begin
    Printf.printf "> %!";
    try interpret (read_line ()); repl ()
    with End_of_file -> ()
  end

let rec file inch filename =
  let lexbuf = Sedlexing.Utf8.from_channel inch in
  let token, start, stop = Ulexer.parse () in
  try
    let tree = Uparser.exprs token lexbuf in
    let _ = Synthesis.eval ~env:Core.core tree in
    (* let _ = Interpreter.eval Core.runtime tree in *)
    ()
  with
  | Uparser.Error ->
    let loc = Location.make
        (start lexbuf)
        (stop lexbuf)
    in
    Printf.printf "Parsing error at %s:\n> %s\n%!"
      (Location.to_string loc)
      (Location.to_string_of_file loc filename)
  | Ulexer.Lexical_error ->
    let loc = Location.make
        (start lexbuf)
        (stop lexbuf)
    in
    Printf.printf "Lexical error at %s:\n> %s\n%!"
      (Location.to_string loc)
      (Location.to_string_of_file loc filename)
  | Synthesis.Error (loc, _) as exn ->
    Printf.printf "Typing error: %s at %s:\n> %s\n%!"
      (Printexc.to_string exn)
      (Location.to_string loc)
      (Location.to_string_of_file loc filename)
  | Interpreter.Error (loc, _) as exn ->
    Printf.printf "Interpreter error: %s at %s:\n> %s\n%!"
      (Printexc.to_string exn)
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
