let interpret expr =
  let lexbuf = Sedlexing.Utf8.from_string expr in
  let token, start, stop = ULexer.parse () in
  try
    let tree = UParser.single_expr token lexbuf in
    let ty = Synthesis.eval ~env:Core.core tree in
    (Type.to_string ty)
  with
  | UParser.Error ->
    let loc = Loc.make
      (start lexbuf)
      (stop lexbuf)
    in Printf.sprintf "Parsing error at:\n%s\n%!"
      (Loc.to_string_of_line loc expr)
  | ULexer.Lexical_error ->
    let loc = Loc.make
      (start lexbuf)
      (stop lexbuf)
    in Printf.sprintf "Lexical error at:\n%s\n%!"
      (Loc.to_string_of_line loc expr)
  | Synthesis.Error (loc, exn) ->
    Printf.sprintf "Typing error: %s at:\n%s%!"
      (Printexc.to_string exn)
      (Loc.to_string_of_line loc expr)
  | Interpreter.Error (loc, exn) ->
    Printf.sprintf "Interpreter error: %s at:\n%s%!"
      (Printexc.to_string exn)
      (Loc.to_string_of_line loc expr)


let () =
  Js.Unsafe.global##plop <- (Js.wrap_callback interpret);
