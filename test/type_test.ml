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
    ("id", OK ("(forall (a) (a -> a))"));
    ("1", OK ("int"));
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
    ("(let (f (lambda (x) x)) [(f 1), (f true)])",
     OK ("(pair int bool)"));
    ("(lambda (f) [(f 1), (f true)])",
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
    ("[1 + true]",
     Fail (Synthesis.Conflict (Type.Const "int", Type.Const "bool")));
    ("(+ 1)",
     Fail (Synthesis.Mismatch_arguments
             (Type.Arrow ([Type.Const "int"; Type.Const "int"],
                          Type.Const "int"))));
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
    ("(1 id)",
     Fail (Synthesis.Expected_function (Type.Const "int")));
    ("(lambda (f) (let (x (lambda (g y) (let (_ (g y)) (= f g)))) x))",
     OK ("(forall (a b) ((a -> b) -> (a -> b) -> a -> bool))"));
    ("(let (const (lambda (x) (lambda (y) x))) const)",
     OK ("(forall (a b) (a -> b -> a))"));
    ("(let (|> (lambda (f x) (f x))) |>)",
     OK ("(forall (a b) ((a -> b) -> a -> b))"));
    ("(let (apply (lambda (f) (lambda (x) (f x)))) apply)",
     OK ("(forall (a b) ((a -> b) -> a -> b))"));
    ("ids", OK "(list (forall (a) (a -> a)))");
    ("(lambda (f) [(f 1), (f true)])",
     Fail (Synthesis.Conflict (Type.Const "int", Type.Const "bool")));
    ("(lambda (f : (forall (a) (a -> a))) [(f true), (f 1)])",
     OK ("(((forall (a) (a -> a))) -> (pair bool int))"));
    ("(cons ids nill)", OK ("(list (list (forall (a) (a -> a))))"));
    ("(choose ids nill)", OK ("(list (forall (a) (a -> a)))"));
    ("(choose nill ids)", OK ("(list (forall (a) (a -> a)))"));
    ("(cons (lambda (x) (x)) ids)", OK ("(list (forall (a) (a -> a)))"));
    ("(let (rcons (lambda (x y) (cons y x))) (rcons ids id))",
     OK ("(list (forall (a) (a -> a)))"));
    ("(cons id ids)", OK ("(list (forall (a) (a -> a)))"));
    ("(cons id (cons succ nill))", OK ("(list (int -> int))"));
    ("(poly id)", OK ("(pair int bool)"));
    ("(poly (lambda (x) x))", OK ("(pair int bool)"));
    ("(poly succ)",
     Fail (Synthesis.Conflict (Variable.dummy, Type.Const "int")));
    ("(apply succ 1)", OK ("int"));
    ("(apply poly id)", OK ("(pair int bool)"));
    ("(id : (forall (a) (a -> a))) : (int -> int)", OK ("(int -> int)"));
    ("(single (id : (forall (a) (a -> a))))",
     OK ("(list (forall (a) (a -> a)))"));
    ("((lambda (x) (lambda (y) (let (z (choose x y)) z)))
       (id : (forall (a) (a -> a))))",
     OK ("((forall (a) (a -> a)) -> (forall (a) (a -> a)))"));
    ("(lambda (x : (forall (a) (a -> a))) x)",
     OK ("(forall (a) ((forall (b) (b -> b)) -> a -> a))"));
    ("(id' id)", OK ("(forall (a) (a -> a))"));
    ("(id'' id)", OK ("(forall (a) (a -> a))"));
    ("(lambda (ids) (ids' ids))", Unsafe);
    ("(poly (id id))", OK ("(pair int bool)"));
    ("(length (ids))", OK ("int"));
    ("(map head (single ids))", OK ("(list (forall (a) (a -> a)))"));
    ("(apply id 1)", OK ("int"));
    ("(poly magic)", OK ("(pair int bool)"));
    ("(magid magic)", OK ("(forall (a b) (a -> b))"));
    ("(lambda (f : (forall (a b) (a -> b))) (f : (forall (a) (a -> a))))",
     OK ("((forall (a b) (a -> b)) -> (forall (a) (a -> a)))"));
    ("(lambda (f : (forall (a b) (a -> b))) (let (a (magid f)) 1))",
     OK ("((forall (a b) (a -> b)) -> int)"));
    ("(let (const (any : (forall (a) (a -> (forall (b) (b -> a))))))
       (const any))"), OK ("(forall (a b) (a -> b))");
    ("(rec (f (lambda (x) (f x))) f)", OK ("(forall (a b) (a -> b))"));
    ("(rec (foldl (lambda (f a l)
                   (if (empty l) a
                    (foldl f (f a (head l)) (tail l))))) foldl)",
     OK ("(forall (a b) ((a -> b -> a) -> a -> (list b) -> a))"));
    ("(rec (foldr (lambda (f l a)
                   (if (empty l) a
                    (f (head l) (foldr f (tail l) a))))) foldr)",
     OK ("(forall (a b) ((a -> b -> b) -> (list a) -> b -> b))"));
    ("(rec (foldl (lambda (f a l)
                   (if (empty l) a
                    (foldl f (f a (head l)) (tail l)))))
           (lambda (l) (foldl (lambda (a x) [a + 1]) 0 l)))",
     OK ("(forall (a) ((list a) -> int))"));
    ("(rec (iter (lambda (f l)
                  (if (empty l) ()
                   [(f (head l)); (iter f (tail l))])))
       iter)",
     OK ("(forall (a) ((a -> unit) -> (list a) -> unit))"));
    ("(let (iter (rec (foldl (lambda (f a l)
                   (if (empty l) a
                    (foldl f (f a (head l)) (tail l)))))
                 (lambda (f l) (foldl (lambda (a x) (f x)) () l))))
           (lambda (l) (iter #num l)))",
     OK ("((list int) -> unit)"));

  ]

let to_string = function
  | Fail e -> "Fail: " ^ (Synthesis.string_of_exn e)
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
