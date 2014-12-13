open OUnit2

type t =
  | OK of string
  | Output of string
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
    ("X(1)", Output "∀a. [X : int | a]");
    ("choose[choose[A(1), B(true)], choose[C(false), D(nill)]]",
     Output "∀a. [A : int | B : bool | C : bool | D : ∀b. list[b] | a]");
    ("choose[A(true), A(1)]",
     Fail (Synthesis.Conflict ((Type.int), (Type.bool))));
    ("choose[A({ x = 1, y = true }), B({ w = false })]",
     Output "∀a. [A : {x : int | y : bool} | B : {w : bool} | a]");
    ("λx y. match x { A(_) → 1 | B(_) → 0 | C(_) → y }",
     Output "∀a b c. [A : a | B : b | C : c] → int → int");
    ("λa. match a { A(i) → i | _ → 1 }",
     Output "∀a. [A : int | a] → int");
    ("f = λe. match e { A(i) → i | _ → 0 }
      in λe. match e { B(i) → i | C(_) → 0 | e → f[e] }",
     Output "∀a b. [A : int | B : int | C : a | b] → int");
    ("e = choose[choose[A(1), B(true)], choose[C(false), D(nill)]]
      in f = λe. match e { D(_) → 0 | B(_) → 0 | _ → 0 }
      in match e { A(i) → i | C(_) → 0 | _ → f[e] }",
     Output "int");
    ("λe. match e { A(_) → 0 | A(i) → i }",
     Output "∀a. [A : int | a] → int");
    ("f = λg. λe. match e { A(i) → i | B(_) → 1 | e → g[e] }
      in g = λe. match e { C(l) → head[l] | _ → 0 }
      in f[g]",
      Output "∀a b. [A : int | B : a | C : list[int] | b] → int");
    ("λe. match e { A(a) → (a.x + 1) }",
     Output "∀a. [A : {x : int | a}] → int");
    ("Y(count) = λx. match x { Cons(a) → (1 + count[a.tail]) | Nill() → 0 }
      in count", Fail (Synthesis.Recursive_type Variable.dummy));
  ]

let to_string = function
  | Fail e -> "Fail: " ^ (Printexc.to_string e)
  | OK ty -> "OK: " ^ ty
  | Output str -> "Output: " ^ str
  | Unsafe -> "Fail"

let normalize ty =
  let lexbuf = Sedlexing.Utf8.from_string ty in
  let token, start, stop = ULexer.parse () in
  try Type.to_string (UParser.single_ty token (Sedlexing.Utf8.from_string ty))
  with
  | UParser.Error ->
    let loc = Loc.make
        (start lexbuf)
        (stop lexbuf)
    in
    Printf.printf "Parsing error at %s:\n> %s\n%!"
      (Loc.to_string loc)
      (Loc.to_string_of_line loc ty);
    raise (Invalid_argument "normalize")

let rec compare r1 r2 =
  let open Synthesis in
  match r1, r2 with
  | OK ty1, OK ty2 -> (normalize ty1) = (normalize ty2)
  | OK ty1, Output ty2
  | Output ty1, OK ty2
  | Output ty1, Output ty2 -> ty1 = ty2
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
        let token, _, _ = ULexer.parse () in
        let ty = Synthesis.eval ~env:Core.core
            (UParser.single_expr token (Sedlexing.Utf8.from_string expr))
            |> Synthesis.generalization (-1) in
        OK (Type.to_string ty)
      with Synthesis.Error (_, exn) -> Fail exn
         | exn -> Fail exn
    in
    assert_equal ~printer:to_string ~cmp:compare re result

let suite = "Synthesis test" >::: List.map make_test tests

let () =
  try run_test_tt_main suite
  with exn -> Printexc.to_string exn |> print_endline
