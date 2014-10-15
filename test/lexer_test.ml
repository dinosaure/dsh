open OUnit2

type t =
  | OK of Parser.token list
  | Fail

let tests =
  let open Parser in
  [
    ("", OK []);
    ("  \t\n\r  \r\n\t", OK []);
    (*
    ("())lambda let_ _1Ma",
     OK [LPAR; RPAR; RPAR; LAMBDA; NAME "let_"; NAME "_1Ma"]);
    ("lambda let forall", OK [LAMBDA; LET; FORALL]);
    *)
    ("λ", Fail);
    ("x$", Fail);
  ]

module String = struct
  include String

  let of_list ?(sep="") to_string lst =
    let buffer = Buffer.create 16 in
    let rec aux = function
      | [] -> ()
      | [ x ] -> Printf.bprintf buffer "%s" (to_string x)
      | x :: r -> Printf.bprintf buffer "%s%s" (to_string x) sep; aux r
    in aux lst; Buffer.contents buffer
end

let to_string = function
  | Fail -> "Fail"
  | OK l -> ("OK: " ^ (String.of_list ~sep:", " Lexer.string_of_token l))

let parse code =
  let lexbuf = Lexing.from_string code in
  let rec aux acc =
    match Lexer.token lexbuf with
    | Parser.EOF -> acc
    | token -> aux (token :: acc)
  in List.rev (aux [])

let make_test (expr, result) =
  String.escaped expr >:: fun _ ->
    let re =
      try OK (parse expr)
      with Lexer.Lexical_error -> Fail
    in
    assert_equal ~printer:to_string re result

let suite = "Lexer test" >::: List.map make_test tests

let () = run_test_tt_main suite
