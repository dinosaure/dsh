open OUnit2

module Ast_without_loc = struct
  type t =
    | Var of string
    | App of (t * t list)
    | Abs of ((string * annotation option) list * t)
    | Let of (string * t * t)
    | Rec of (string * t * t)
    | Ann of (t * annotation)
    | If of (t * t * t)
    | Seq of (t * t)
    | Int of int
    | Bool of bool
    | Char of char
    | Unit
  and annotation = (int list * Type.t)

  let rec to_ast = function
    | Var name -> Ast.Var (Location.dummy, name)
    | App (f, a) -> Ast.App (Location.dummy, to_ast f, List.map to_ast a)
    | Abs (a, r) -> Ast.Abs (Location.dummy, a, to_ast r)
    | Let (name, e, c) -> Ast.Let (Location.dummy, name, to_ast e, to_ast c)
    | Rec (name, e, c) -> Ast.Rec (Location.dummy, name, to_ast e, to_ast c)
    | Ann (e, ann) -> Ast.Ann (Location.dummy, to_ast e, ann)
    | If (i, a, b) -> Ast.If (Location.dummy, to_ast i, to_ast a, to_ast b)
    | Seq (a, b) -> Ast.Seq (Location.dummy, to_ast a, to_ast b)
    | Int i -> Ast.Int (Location.dummy, i)
    | Bool b -> Ast.Bool (Location.dummy, b)
    | Char c -> Ast.Char (Location.dummy, c)
    | Unit -> Ast.Unit (Location.dummy)

  let rec of_ast = function
    | Ast.Var (_, name) -> Var name
    | Ast.App (_, f, a) -> App (of_ast f, List.map of_ast a)
    | Ast.Abs (_, a, r) -> Abs (a, of_ast r)
    | Ast.Let (_, name, e, c) -> Let (name, of_ast e, of_ast c)
    | Ast.Rec (_, name, e, c) -> Rec (name, of_ast e, of_ast c)
    | Ast.Ann (_, e, ann) -> Ann (of_ast e, ann)
    | Ast.If (_, i, a, b) -> If (of_ast i, of_ast a, of_ast b)
    | Ast.Seq (_, a, b) -> Seq (of_ast a, of_ast b)
    | Ast.Int (_, i) -> Int i
    | Ast.Bool (_, b) -> Bool b
    | Ast.Char (_, c) -> Char c
    | Ast.Unit _ -> Unit
end

type t =
  | OK of Ast_without_loc.t
  | Fail

let tests =
  let open Ast_without_loc in
  [
    ("", Fail);
    ("(", Fail);
    (")", Fail);
    ("())", Fail);
    ("x", OK (Var "x"));
    ("(lambda (x) x)", OK (Abs ([("x", None)], Var "x")));
    ("(lambda x x)", Fail);
    ("(f x y)", OK (App (Var "f", [Var "x"; Var "y"])));
    ("((f x) y)", OK (App (App (Var "f", [Var "x"]), [Var "y"])));
    ("(f x) y", Fail);
    ("(let (f (lambda (x y) (g x y))) (f a b))",
     OK (Let ("f",
              Abs ([("x", None); ("y", None)],
                   App (Var "g", [Var "x"; Var "y"])),
              App (Var "f", [Var "a"; Var "b"]))));
    ("(let (x a) (let (y b) (f x y)))",
     OK (Let ("x", Var "a",
              (Let ("y", Var "b",
                    App (Var "f", [Var "x"; Var "y"]))))));
    ("f x", Fail);
    ("(let (a one))", Fail);
    ("((let) (a one) a)", Fail);
    ("(let (a one) a)",
     OK (Let ("a", Var "one", Var "a")));
    ("()", Fail);
    ("(id x))", Fail)
  ]

let to_string = function
  | Fail -> "Fail"
  | OK a -> ("OK: " ^ (Ast_without_loc.to_ast a
                       |> Ast.to_string))

let make_test (expr, result) =
  String.escaped expr >:: fun _ ->
    let re =
      try OK (Parser.single_expr Lexer.token (Lexing.from_string expr)
              |> Ast_without_loc.of_ast)
      with Parser.Error -> Fail
    in
    assert_equal ~printer:to_string re result

let suite = "Parser test" >::: List.map make_test tests

let () = run_test_tt_main suite
