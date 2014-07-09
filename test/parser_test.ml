open OUnit2

type t =
  | OK of Ast.t
  | Fail

let tests =
  let open Ast in
  [
    ("", Fail);
    ("x", OK (Var "x"));
    ("(lambda (x) x)", OK (Abs (["x"], Var "x")));
    ("(f x y)", OK (App (Var "f", [Var "x"; Var "y"])));
    ("((f x) y)", OK (App (App (Var "f", [Var "x"]), [Var "y"])));
    ("(let (f (lambda (x y) (g x y))) (f a b))",
     OK (Let ("f", Abs (["x"; "y"], App (Var "g", [Var "x"; Var "y"])),
              App (Var "f", [Var "a"; Var "b"]))));
    ("(let (x a) (let (y b) (f x y)))",
     OK (Let ("x", Var "a",
              (Let ("y", Var "b",
                    App (Var "f", [Var "x"; Var "y"]))))));
    ("f x", Fail);
    ("(let (a one))", Fail);
    ("()", Fail)
  ]

let to_string = function
  | Fail -> "Fail"
  | OK a -> ("OK: " ^ (Ast.to_string a))

let make_test (expr, result) =
  String.escaped expr >:: fun _ ->
    let re =
      try OK (Parser.single_expr Lexer.token (Lexing.from_string expr))
      with Parser.Error -> Fail
    in
    assert_equal ~printer:to_string re result

let suite = "Parser test" >::: List.map make_test tests

let () = run_test_tt_main suite
