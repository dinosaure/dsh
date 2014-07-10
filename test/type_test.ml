open OUnit2

type t =
  | OK of string
  | Fail of exn

module Variable = struct
  include Synthesis.Variable

  let dummy = Type.Var (ref (Type.Unbound (-1, -1)))
end

module Type = struct
  include Type

  let rec compare t1 t2 =
    match t1, t2 with
    | Const n1, Const n2 -> n1 = n2
    | App (f1, a1), App (f2, a2) ->
      compare f1 f2
      && List.fold_left2 (fun acc t1 t2 -> acc && compare t1 t2) true a1 a2
    | Arrow (a1, r1), Arrow (a2, r2) ->
      List.fold_left2 (fun acc t1 t2 -> acc && compare t1 t2) true a1 a2
      && compare r1 r2
    | Var { contents = Link t1}, Var { contents = Link t2 } -> compare t1 t2
    | Var { contents = Generic id1}, Var { contents = Generic id2 } -> id1 = id2
    | Var { contents = Unbound _ }, Const _ -> true
    | _, ty when ty = Variable.dummy -> true
    | ty, _ when ty = Variable.dummy -> true
    | _, _ -> false
end

let tests =
  [
    ("id", OK ("(forall (a) (a -> a))"));
    ("one", OK ("int"));
    ("x", Fail (Synthesis.Unbound_variable "x"));
    ("(let (x x) x)", Fail (Synthesis.Unbound_variable "x"));
    ("(let (x id) x)", OK ("(forall (a) (a -> a))"));
    ("(let (x (lambda (y) y)) x)", OK ("(forall (a) (a -> a))"));
    ("(lambda (x) x)", OK ("(forall (a) (a -> a))"));
    ("(lambda (x) x)", OK ("(forall (int) (int -> int))"));
    (",", OK ("(forall (a b) (a -> b -> (pair a b)))"));
    (",", OK ("(forall (z x) (x -> z -> (pair x z)))"));
    ("(lambda (x) (let (y (lambda (z) z)) y))",
     OK ("(forall (a b) (a -> (b -> b)))"));
    ("(let (f (lambda (x) x)) (let (id (lambda (y) y)) (= f id)))",
     OK ("bool"));
    ("(let (f (lambda (x) x)) (= f =))",
     Fail (Synthesis.Conflict
             (Type.Arrow ([ Variable.dummy ], Variable.dummy),
              (Type.Arrow ([ Variable.dummy; Variable.dummy ],
                           Type.Const "bool")))));
    ("(let (f (lambda (a b) true)) (= f =))",
     OK ("bool"));
    ("(let (f (lambda (x) x)) (= f succ))",
     OK ("bool"));
    ("(let (f (lambda (x) x)) [(f one), (f true)])",
     OK ("(pair int bool)"));
    ("(lambda (f) [(f one), (f true)])",
     Fail (Synthesis.Conflict (Type.Const "int", Type.Const "bool")));
    ("(let (f (lambda (x y) (let (a (= x y)) [x = y]))) f)",
     OK ("(forall (a) (a -> a -> bool))"));
    ("(id id)", OK ("(forall (a) (a -> a))"));
    ("(choose (lambda (x y) x) (lambda (x y) y))",
     OK ("(forall (a) (a -> a -> a))"));
    ("(let (x id) (let (y (let (z (x id)) z)) y))",
     OK ("(forall (a) (a -> a))"));
    ("(cons id nill)", OK ("(forall (a) (list (a -> a)))"));
    ("(let (l1 (cons id nill)) (let (l2 (cons succ nill)) l2))",
     OK ("(list (int -> int))"));
    ("[one + true]",
     Fail (Synthesis.Conflict (Type.Const "int", Type.Const "bool")));
    ("(+ one)",
     Fail (Synthesis.Expected_argument
             (Type.Arrow ([Type.Const "int"; Type.Const "int"],
                          Type.Const "int"), 1)));
    ("(lambda (x) (let (y x) x))", OK ("(forall (a) (a -> a))"));
    ("(lambda (x) (let (y (let (z (x (lambda (x) x))) z)) y))",
     OK ("(forall (a b) (((a -> a) -> b) -> b))"));
    ("(lambda (x) (lambda (y) (let (x (x y)) (x y))))",
     OK ("(forall (a b) ((a -> a -> b) -> a -> b))"));
    ("(lambda (x) (let (y (lambda (z) (x z))) y))",
     OK ("(forall (a b) ((a -> b) -> a -> b))"));
    ("(lambda (x) (let (y (lambda (z) x)) y))",
     OK ("(forall (a b) (a -> b -> a))"));
    ("(lambda (x) (lambda (y) (let (x (x y)) (lambda (x) (y x)))))",
     OK ("(forall (a b c) (((a -> b) -> c) -> (a -> b) -> a -> b))"));
    ("(lambda (x) (let (y x) (y y)))",
     Fail (Synthesis.Recursive_type
             (Type.Arrow ([ Variable.dummy ], Variable.dummy))));
    ("(lambda (x) (let (y (lambda (z) z)) (y y)))",
     OK ("(forall (a b) (a -> b -> b))"));
    ("(lambda (x) (x x))",
     Fail (Synthesis.Recursive_type
             (Type.Arrow ([ Variable.dummy ], Variable.dummy))));
    ("(one id)",
     Fail (Synthesis.Expected_function (Type.Const "int")));
    ("(lambda (f) (let (x (lambda (g y) (let (_ (g y)) (= f g)))) x))",
     OK ("(forall (a b) ((a -> b) -> (a -> b) -> a -> bool))"));
    ("(let (const (lambda (x) (lambda (y) x))) const)",
     OK ("(forall (a b) (a -> b -> a))"));
    ("(let (|> (lambda (f x) (f x))) |>)",
     OK ("(forall (a b) ((a -> b) -> a -> b))"));
    ("(let (apply (lambda (f) (lambda (x) (f x)))) apply)",
     OK ("(forall (a b) ((a -> b) -> a -> b))"));
  ]

let to_string = function
  | Fail e -> "Fail: " ^ (Synthesis.string_of_exn e)
  | OK ty -> "OK: " ^ ty

let normalize ty =
  Type.to_string (Parser.single_ty Lexer.token (Lexing.from_string ty))

let compare r1 r2 =
  let open Synthesis in
  match r1, r2 with
  | OK ty1, OK ty2 -> (normalize ty1) = (normalize ty2)
  | Fail (Recursive_type a), Fail (Recursive_type b) -> Type.compare a b
  | Fail (Conflict (a1, b1)), Fail (Conflict (a2, b2)) ->
    Type.compare a1 a2 && Type.compare b1 b2
  | Fail (Circularity (a1, b1)), Fail (Circularity (a2, b2)) ->
    Type.compare a1 a2 && Type.compare b1 b2
  | Fail (Expected_argument (t1, n1)), Fail (Expected_argument (t2, n2)) ->
    n1 = n2 && Type.compare t1 t2
  | Fail (Expected_function a), Fail (Expected_function b) ->
    Type.compare a b
  | Fail (Unbound_variable n1), Fail (Unbound_variable n2) -> n1 = n2
  | _, _ -> false

let make_test (expr, result) =
  String.escaped expr >:: fun _ ->
    let re =
      try Synthesis.Variable.reset ();
        let ty = Synthesis.eval Core.core 0
            (Parser.single_expr Lexer.token (Lexing.from_string expr)) in
        let ty' = Synthesis.generalization (-1) ty in
        OK (Type.to_string ty')
      with exn -> Fail exn
    in
    assert_equal ~printer:to_string ~cmp:compare re result

let suite = "Synthesis test" >::: List.map make_test tests

let () = run_test_tt_main suite
