open CamomileLibraryDyn.Camomile

let () = Random.self_init ()
let () = Printexc.record_backtrace true

let interpret expr =
  let lexbuf = Sedlexing.Utf8.from_string expr in
  let token, start, stop = ULexer.parse () in
  try
    let tree = UParser.single_expr token lexbuf in
    let ty = Synthesis.eval ~env:Core.core tree in
    (* let rt = Interpreter.eval Core.runtime tree in *)
    Printf.sprintf "# : %s" (* Interpreter.to_string rt *) (Type.to_string ty)
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
    in Printf.sprintf "Lexical error at:\n%s%!"
      (Loc.to_string_of_line loc expr)
  | Synthesis.Error (loc, _) as exn ->
    Printf.sprintf "Typing error: %s at:\n%s%!"
      (Printexc.to_string exn)
      (Loc.to_string_of_line loc expr)
  | Synthesis.Conflict _ ->
    Printf.sprintf "Backtrace:\n%s%!"
      (Printexc.get_backtrace ())
  | Interpreter.Error (loc, _) as exn ->
    Printf.sprintf "Interpreter error: %s at:\n%s%!"
      (Printexc.to_string exn)
      (Loc.to_string_of_line loc expr)

let rec file inch filename =
  let lexbuf = Sedlexing.Utf8.from_channel inch in
  let token, start, stop = ULexer.parse () in
  try
    let tree = UParser.exprs token lexbuf in
    let _ = Synthesis.eval ~env:Core.core tree in
    (* let _ = Interpreter.eval Core.runtime tree in *)
    ()
  with
  | UParser.Error ->
    let loc = Loc.make
        (start lexbuf)
        (stop lexbuf)
    in
    Printf.printf "Parsing error at %s:\n> %s\n%!"
      (Loc.to_string loc)
      (Loc.to_string_of_file loc filename)
  | ULexer.Lexical_error ->
    let loc = Loc.make
        (start lexbuf)
        (stop lexbuf)
    in
    Printf.printf "Lexical error at %s:\n> %s\n%!"
      (Loc.to_string loc)
      (Loc.to_string_of_file loc filename)
  | Synthesis.Error (loc, _) as exn ->
    Printf.printf "Typing error: %s at %s:\n> %s\n%!"
      (Printexc.to_string exn)
      (Loc.to_string loc)
      (Loc.to_string_of_file loc filename)
  | Interpreter.Error (loc, _) as exn ->
    Printf.printf "Interpreter error: %s at %s:\n> %s\n%!"
      (Printexc.to_string exn)
      (Loc.to_string loc)
      (Loc.to_string_of_file loc filename)

let prompt =
  let open LTerm_text in
  eval [
    B_bold true;
    B_fg LTerm_style.lred; S("dsh"); E_fg;
    B_fg LTerm_style.lblue; S("> "); E_fg;
    E_bold;
  ]

class read_line ~term ~history = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  initializer
    self#set_prompt (React.S.const prompt)
end

(* Binding like Vim *)

let () =
  let make_key ch1 ch2 =
    [{ LTerm_key.control = true;
       meta = false;
       shift = false;
       code = LTerm_key.Char (UChar.of_char 'k') };
     { LTerm_key.control = false;
       meta = false;
       shift = false;
       code = LTerm_key.Char (UChar.of_char ch1) };
     { LTerm_key.control = false;
       meta = false;
       shift = false;
       code = LTerm_key.Char (UChar.of_char ch2) }]
  in
  LTerm_edit.bind
    (make_key 'l' '*') (* λ *)
    [LTerm_edit.Zed (Zed_edit.Insert (UChar.chr 0x03bb))];
  LTerm_edit.bind
    (make_key 'F' 'A') (* ∀ *)
    [LTerm_edit.Zed (Zed_edit.Insert (UChar.chr 0x2200))];
  LTerm_edit.bind
    (make_key '-' '>') (* → *)
    [LTerm_edit.Zed (Zed_edit.Insert (UChar.chr 0x2192))];
  LTerm_edit.bind
    (make_key 'T' 'E') (* ∃ *)
    [LTerm_edit.Zed (Zed_edit.Insert (UChar.chr 0x2203))];
  LTerm_edit.bind
    (make_key 'o' '/') (* ø *)
    [LTerm_edit.Zed (Zed_edit.Insert (UChar.chr 0x00f8))]


let rec loop term history =
  match%lwt
    try%lwt
      let%lwt command =
        (new read_line
            ~term
            ~history:(LTerm_history.contents history))#run
      in
      Lwt.return (Some command)
    with Sys.Break -> Lwt.return None
  with
    | Some command ->
      let result =  interpret command in
      let%lwt () = LTerm.fprintls term
        (let open LTerm_text in eval [ S(result) ])
      in LTerm_history.add history command; loop term history
    | None -> loop term history

let%lwt () =
  let%lwt () = LTerm_inputrc.load () in
  try%lwt
    let%lwt term = Lazy.force LTerm.stdout in
    loop term (LTerm_history.create [])
  with LTerm_read_line.Interrupt ->
    Lwt.return ()

(*
let () =
  Printexc.record_backtrace true;
  try
    if Array.length Sys.argv = 2
    then file (open_in Sys.argv.(1)) Sys.argv.(1)
    else repl ()
  with exn ->
    Printf.printf "%s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stderr
*)
