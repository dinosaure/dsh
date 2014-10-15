open OUnit2

type t =
  | OK of string
  | Fail of exn
  | Unsafe (* lazy to describe exception *)

module Variable = struct
  include Type.Variable

  let dummy = Type.Var (ref (Type.Unbound (-1, -1)))
end

module Type = struct

  let rec compare t1 t2 =
    match t1, t2 with
    | Type.Const n1, Type.Const n2 -> n1 = n2
    | Type.App (f1, a1), Type.App (f2, a2) ->
      compare f1 f2
      && List.fold_left2 (fun acc t1 t2 -> acc && compare t1 t2) true a1 a2
    | Type.Arrow (a1, r1), Type.Arrow (a2, r2) ->
      List.fold_left2 (fun acc t1 t2 -> acc && compare t1 t2) true a1 a2
      && compare r1 r2
    | Type.Var { contents = Type.Link t1},
      Type.Var { contents = Type.Link t2 } ->
      compare t1 t2
    | Type.Var { contents = Type.Generic id1},
      Type.Var { contents = Type.Generic id2 } -> id1 = id2
    | _, ty when ty = Variable.dummy -> true
    | ty, _ when ty = Variable.dummy -> true
    | _, _ -> t1 = t2

  include Type
end

let tests =
  [
    ("id", OK ("(V (a) (a > a))"));
    ("1", OK ("int"));
    ("x", Fail (Synthesis.Unbound_variable "x"));
    ("(: (x x) x)", Fail (Synthesis.Unbound_variable "x"));
    ("(: (x id) x)", OK ("(V (a) (a > a))"));
    ("(: (x (\\ (y) y)) x)", OK ("(V (a) (a > a))"));
    ("(\\ (x) x)", OK ("(V (a) (a > a))"));
    ("(\\ (x) x)", OK ("(V (lol) (lol > lol))"));
    ("(\\ (a b) (, a b))", OK ("(V (a b) (a > b > (* a b)))"));
    ("(\\ (a b) (, a b))", OK ("(V (z x) (x > z > (* x z)))"));
    ("(\\ (x) (: (y (\\ (z) z)) y))",
     OK ("(V (a b) (a > (b > b)))"));
    ("(: (f (\\ (x) x)) (: (id (\\ (y) y)) (= f id)))",
     OK ("bool"));
    ("(: (f (\\ (x) x)) (= f =))",
     Fail (Synthesis.Conflict
             (Type.Arrow ([ Variable.dummy ], Variable.dummy),
              (Type.Arrow ([ Variable.dummy; Variable.dummy ],
                           Type.bool)))));
    ("(: (f (\\ (a b) true)) (= f =))",
     OK ("bool"));
    ("(: (f (\\ (x) x)) (= f succ))",
     OK ("bool"));
    ("(: (f (\\ (x) x)) (, (f 1) (f true)))",
     OK ("(* int bool)"));
    ("(\\ (f) (, (f 1) (f true)))",
     Fail (Synthesis.Conflict (Type.int, Type.bool)));
    ("(: (f (\\ (x y) (: (a (= x y)) [x = y]))) f)",
     OK ("(V (a) (a > a > bool))"));
    ("(id id)", OK ("(V (a) (a > a))"));
    ("(choose (\\ (x y) x) (\\ (x y) y))",
     OK ("(V (a) (a > a > a))"));
    ("(: (x id) (: (y (: (z (x id)) z)) y))",
     OK ("(V (a) (a > a))"));
    ("(cons id nill)", OK ("(V (a) (list (a > a)))"));
    ("(: (l1 (cons id nill)) (: (l2 (cons succ nill)) l2))",
     OK ("(list (int > int))"));
    ("[1 + true]",
     Fail (Synthesis.Conflict (Type.int, Type.bool)));
    ("(+ 1)",
     Fail (Synthesis.Mismatch_arguments
             (Type.Arrow ([Type.int; Type.int],
                          Type.int))));
    ("(\\ (x) (: (y x) x))", OK ("(V (a) (a > a))"));
    ("(\\ (x) (: (y (: (z (x (\\ (x) x))) z)) y))",
     OK ("(V (a b) (((a > a) > b) > b))"));
    ("(\\ (x) (\\ (y) (: (x (x y)) (x y))))",
     OK ("(V (a b) ((a > a > b) > a > b))"));
    ("(\\ (x) (: (y (\\ (z) (x z))) y))",
     OK ("(V (a b) ((a > b) > a > b))"));
    ("(\\ (x) (: (y (\\ (z) x)) y))",
     OK ("(V (a b) (a > b > a))"));
    ("(\\ (x) (\\ (y) (: (x (x y)) (\\ (x) (y x)))))",
     OK ("(V (a b c) (((a > b) > c) > (a > b) > a > b))"));
    ("(\\ (x) (: (y x) (y y)))",
     Fail (Synthesis.Recursive_type
             (Type.Arrow ([ Variable.dummy ], Variable.dummy))));
    ("(\\ (x) (: (y (\\ (z) z)) (y y)))",
     OK ("(V (a b) (a > b > b))"));
    ("(\\ (x) (x x))",
     Fail (Synthesis.Recursive_type
             (Type.Arrow ([ Variable.dummy ], Variable.dummy))));
    ("(1 id)",
     Fail (Synthesis.Expected_function (Type.int)));
    (* ("(\\ (f) (: (x (\\ (g y) (: (_ (g y)) (= f g)))) x))",
     OK ("(V (a b) ((a > b) > (a > b) > a > bool))")); XXX: Syntax error *)
    ("(: (const (\\ (x) (\\ (y) x))) const)",
     OK ("(V (a b) (a > b > a))"));
    (* ("(: (|> (\\ (f x) (f x))) |>)",
     OK ("(V (a b) ((a > b) > a > b))")); XXX: Syntax error *)
    ("(: (apply (\\ (f) (\\ (x) (f x)))) apply)",
     OK ("(V (a b) ((a > b) > a > b))"));
    ("ids", OK "(list (V (a) (a > a)))");
    ("(\\ (f) (, (f 1) (f true)))",
     Fail (Synthesis.Conflict (Type.int, Type.bool)));
    ("(\\ (f : (V (a) (a > a))) (, (f true) (f 1)))",
     OK ("(((V (a) (a > a))) > (* bool int))"));
    ("(cons ids nill)", OK ("(list (list (V (a) (a > a))))"));
    ("(choose ids nill)", OK ("(list (V (a) (a > a)))"));
    ("(choose nill ids)", OK ("(list (V (a) (a > a)))"));
    ("(cons (\\ (x) (x)) ids)", OK ("(list (V (a) (a > a)))"));
    ("(: (rcons (\\ (x y) (cons y x))) (rcons ids id))",
     OK ("(list (V (a) (a > a)))"));
    ("(cons id ids)", OK ("(list (V (a) (a > a)))"));
    ("(cons id (cons succ nill))", OK ("(list (int > int))"));
    ("(poly id)", OK ("(* int bool)"));
    ("(poly (\\ (x) x))", OK ("(* int bool)"));
    ("(poly succ)",
     Fail (Synthesis.Conflict (Variable.dummy, Type.int)));
    ("(apply succ 1)", OK ("int"));
    ("(apply poly id)", OK ("(* int bool)"));
    ("(id : (V (a) (a > a))) : (int > int)", OK ("(int > int)"));
    ("(single (id : (V (a) (a > a))))",
     OK ("(list (V (a) (a > a)))"));
    ("((\\ (x) (\\ (y) (: (z (choose x y)) z)))
       (id : (V (a) (a > a))))",
     OK ("((V (a) (a > a)) > (V (a) (a > a)))"));
    ("(\\ (x : (V (a) (a > a))) x)",
     OK ("(V (a) ((V (b) (b > b)) > a > a))"));
    ("(id' id)", OK ("(V (a) (a > a))"));
    ("(id'' id)", OK ("(V (a) (a > a))"));
    ("(\\ (ids) (ids' ids))", Unsafe);
    ("(poly (id id))", OK ("(* int bool)"));
    ("(length (ids))", OK ("int"));
    ("(map head (single ids))", OK ("(list (V (a) (a > a)))"));
    ("(apply id 1)", OK ("int"));
    ("(poly magic)", OK ("(* int bool)"));
    ("(magid magic)", OK ("(V (a b) (a > b))"));
    ("(\\ (f : (V (a b) (a > b))) (f : (V (a) (a > a))))",
     OK ("((V (a b) (a > b)) > (V (a) (a > a)))"));
    ("(\\ (f : (V (a b) (a > b))) (: (a (magid f)) 1))",
     OK ("((V (a b) (a > b)) > int)"));
    ("(: (const (any : (V (a) (a > (V (b) (b > a))))))
       (const any))"), OK ("(V (a b) (a > b))");
    ("(Y (f (\\ (x) (f x))) f)", OK ("(V (a b) (a > b))"));
    ("(Y (foldl (\\ (f a l)
                   (? (empty l) a
                    (foldl f (f a (head l)) (tail l))))) foldl)",
     OK ("(V (a b) ((a > b > a) > a > (list b) > a))"));
    ("(Y (foldr (\\ (f l a)
                   (? (empty l) a
                    (f (head l) (foldr f (tail l) a))))) foldr)",
     OK ("(V (a b) ((a > b > b) > (list a) > b > b))"));
    ("(Y (foldl (\\ (f a l)
                   (? (empty l) a
                    (foldl f (f a (head l)) (tail l)))))
           (\\ (l) (foldl (\\ (a x) [a + 1]) 0 l)))",
     OK ("(V (a) ((list a) > int))"));
    ("(Y (iter (\\ (f l)
                  (? (empty l) ()
                   [(f (head l)); (iter f (tail l))])))
       iter)",
     OK ("(V (a) ((a > unit) > (list a) > unit))"));
    ("(: (iter (Y (foldl (\\ (f a l)
                   (? (empty l) a
                    (foldl f (f a (head l)) (tail l)))))
                 (\\ (f l) (foldl (\\ (a x) (f x)) () l))))
           (\\ (l) (iter num l)))",
     OK ("((list int) > unit)"));
    ("(\\ (f a b) [(f a) = b])",
     OK ("(V (a b) ((a > b) > a > b > bool))"));
    ("(\\ (x : (E (a) (a > a))) x)",
     OK ("(V (a) ((a > a) > a > a))"));
    ("(\\ (x : (V (a) (a > a))) x)",
     OK ("(V (a) ((V (b) (b > b)) > a > a))"));
    ("(\\ (f : (E (a b) (a > b)) x y) [(f x) = y])",
     OK ("(V (a b) ((a > b) > a > b > bool))"));
    ("(: (foo (\\ (x : (E (a) a)) x)) foo)",
     OK ("(V (a) (a > a))"));
    ("(\\ (f : (V (a b) (a > b)) x y) [(f x) = y])",
     OK ("(V (a b) ((V (c d) (c > d)) > a > b > bool))"));
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
      try Type.Variable.reset ();
        let ty = Synthesis.eval ~env:Core.core
            (Parser.single_expr Lexer.token (Lexing.from_string expr)) in
        OK (Type.to_string ty)
      with Synthesis.Error (_, exn) -> Fail exn
         | exn -> Fail exn
    in
    assert_equal ~printer:to_string ~cmp:compare re result

let suite = "Synthesis test" >::: List.map make_test tests

let () =
  try run_test_tt_main suite
  with exn -> Printexc.to_string exn |> print_endline
