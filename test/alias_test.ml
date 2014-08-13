open OUnit2

type t =
  | OK of string
  | Fail of exn
  | Unsafe (* lazy to describe exception *)

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
    | _, ty when ty = Variable.dummy -> true
    | ty, _ when ty = Variable.dummy -> true
    | _, _ -> t1 = t2
end

let tests =
  [
    ("(type (int foo) (lambda (x : foo) x))",
     OK ("(foo -> foo)"));
    ("(type (foo int) (type (bar int) (lambda (x : foo) x : bar)))",
     OK ("(foo -> bar)"));
    ("(type (foo (forall (a) (a -> a))) (lambda (f : foo) (f 1)))",
     OK ("(foo -> int)"));
    ("(type (foo (forall (a) (a -> a))) (lambda (f : foo) [(f 1), (f true)]))",
     OK ("(foo -> (pair int bool))"));
    ("(type (foo int) (type
                       (bar (forall (a) (a -> foo)))
                       (lambda (f : bar) (f true))))"),
     OK ("(bar -> foo)");

  ]

let to_string = function
  | Fail e -> "Fail: " ^ (Printexc.to_string e)
  | OK ty -> "OK: " ^ ty
  | Unsafe -> "Fail"

let normalize ty =
  Type.to_string (Parser.single_ty Lexer.token (Lexing.from_string ty))

let rec compare r1 r2 =
  let open Synthesis in
  match r1, r2 with
  | OK ty1, OK ty2 -> (normalize ty1) = (normalize ty2)
  | Fail (Error (_, exn1)), Fail exn2
  | Fail exn1, Fail (Error (_, exn2)) ->
    compare (Fail exn1) (Fail exn2)
  | Fail (Recursive_type a), Fail (Recursive_type b) -> Type.compare a b
  | Fail (Conflict (a1, b1)), Fail (Conflict (a2, b2)) ->
    Type.compare a1 a2 && Type.compare b1 b2
  | Fail (Circularity (a1, b1)), Fail (Circularity (a2, b2)) ->
    Type.compare a1 a2 && Type.compare b1 b2
  | Fail (Mismatch_arguments t1), Fail (Mismatch_arguments t2) ->
    Type.compare t1 t2
  | Fail (Expected_function a), Fail (Expected_function b) ->
    Type.compare a b
  | Fail (Unbound_variable n1), Fail (Unbound_variable n2) -> n1 = n2
  | Unsafe, Fail _ | Fail _, Unsafe -> true
  | a, b -> a = b

let make_test (expr, result) =
  String.escaped expr >:: fun _ ->
    let re =
      try Synthesis.Variable.reset ();
        let ty = Synthesis.eval ~env:Core.core
            (Parser.single_expr Lexer.token (Lexing.from_string expr)) in
        OK (Type.to_string ty)
      with exn -> Fail exn
    in
    assert_equal ~printer:to_string ~cmp:compare re result

let suite = "Synthesis test" >::: List.map make_test tests

let () = run_test_tt_main suite
