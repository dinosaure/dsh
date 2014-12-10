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
    ("id", OK ("∀a. a → a"));
    ("1", OK ("int"));
    ("x", Fail (Synthesis.Unbound_variable "x"));
    ("x = x in x", Fail (Synthesis.Unbound_variable "x"));
    ("x = id in x", OK ("∀a. (a → a)"));
    ("x = λy.y in x", OK ("∀a. (a → a)"));
    ("λx.x", OK ("∀a. (a → a)"));
    ("λx.x", OK ("∀y. (y → y)"));
    ("λa b. (a, b)", OK ("∀a b. (a → b → tuple[a, b])"));
    ("λa b. (b, a)", OK ("∀b a. (a → b → tuple[b, a])"));
    ("λx. y = λz. z in y",
     OK ("∀a b. (a → (b → b))"));
    ("f = λx.x in id = λy. y in equal[f, id]",
     OK ("bool"));
    ("f = λx.x in equal[f, equal]",
     Fail (Synthesis.Conflict
             (Type.Arrow ([ Variable.dummy ], Variable.dummy),
              (Type.Arrow ([ Variable.dummy; Variable.dummy ],
                           Type.bool)))));
    ("f = λa b. true in equal[f, equal]",
     OK ("bool"));
    ("f = λx.x in equal[f, succ]",
     OK ("bool"));
    ("f = λx.x in (f[1], f[true])",
     OK ("tuple[int, bool]"));
    ("λf.(f[1], f[true])",
     Fail (Synthesis.Conflict (Type.int, Type.bool)));
    ("f = λx y. a = equal[x, y] in equal[x, y] in f",
     OK ("∀a. (a → a → bool)"));
    ("id[id]", OK ("∀a. (a → a)"));
    ("choose[λx y. x, λx y. y]",
     OK ("∀a. (a → a → a)"));
    ("x = id in y = z = x[id] in z in y",
     OK ("∀a. (a → a)"));
    ("cons[id, nill]", OK ("∀a. list[a → a]"));
    ("l1 = cons[id, nill] in l2 = cons[succ, nill] in l2",
     OK ("list[int → int]"));
    ("(1 + true)",
     Fail (Synthesis.Conflict (Type.int, Type.bool)));
    ("+ [1]",
     Fail (Synthesis.Mismatch_arguments
             (Type.Arrow ([Type.int; Type.int],
                          Type.int))));
    ("λx. y = x in x", OK ("∀a. (a → a)"));
    ("λx. y = z = x[λx. x] in z in y",
     OK ("∀a b.((a → a) → b) → b"));
    ("λx. λy. x = x[y] in x[y]",
     OK ("∀a b. (a → (a → b)) → (a → b)"));
    ("λx y. x = x[y] in x[y]",
     OK ("∀a b. (a → (a → b)) → a → b"));
    ("λx. y = λz. x[z] in y",
     OK ("∀a b. (a → b) → (a → b)"));
    ("λx. y = λz. x in y",
     OK ("∀a b. a → (b → a)"));
    ("λx. λy. x = x[y] in λx. y[x]",
     OK ("∀a b c. ((a → b) → c) → ((a → b) → (a → b))"));
    ("λx. y = x in y[y]",
     Fail (Synthesis.Recursive_type
             (Type.Arrow ([ Variable.dummy ], Variable.dummy))));
    ("λx. y = λz. z in y[y]",
     OK ("∀a b. a → (b → b)"));
    ("λx. x[x]",
     Fail (Synthesis.Recursive_type
             (Type.Arrow ([ Variable.dummy ], Variable.dummy))));
    ("1 [id]",
     Fail (Synthesis.Expected_function (Type.int)));
    ("const = λx y. x in const",
     OK ("∀a b. a → b → a"));
    ("apply = λf x. f[x] in apply",
     OK ("∀a b. (a → b) → a → b"));
    ("ids", OK "list[∀a. a → a]");
    ("λf. (f[1], f[true])",
     Fail (Synthesis.Conflict (Type.int, Type.bool)));
    ("λf : ∀a. a → a. (f[true], f[1])",
     OK ("(∀a. a → a) → tuple[bool, int]"));
    ("cons[ids, nill]", OK ("list[list[∀a. a → a]]"));
    ("choose[ids, nill]", OK ("list[∀a. a → a]"));
    ("choose[nill, ids]", OK ("list[∀a. a → a]"));
    ("cons[λx. x, ids]", OK ("list[∀a. a → a]"));
    ("rcons = λx y. cons[y, x] in rcons[ids, id]",
     OK ("list[∀a. a → a]"));
    ("cons[id, ids]", OK ("list[∀a. a → a]"));
    ("cons[id, cons[succ, nill]]", OK ("list[int → int]"));
    ("poly[id]", OK ("tuple[int, bool]"));
    ("poly[λx. x]", OK ("tuple[int, bool]"));
    ("poly[succ]",
     Fail (Synthesis.Conflict (Variable.dummy, Type.int)));
    ("apply[succ, 1]", OK ("int"));
    ("apply[poly, id]", OK ("tuple[int, bool]"));
    ("(id : ∀a. a → a) : int → int", OK ("int → int"));
    ("single[id : ∀a. a → a]",
     OK ("list[∀a. a → a]"));
    ("(λx. λy. z = choose[x, y] in z)[id : ∀a. a → a]",
     OK ("(∀a. a → a) → (∀a. a → a)"));
    ("λx : ∀a. a → a. x",
     OK ("∀a. (∀b. b → b) → (a → a)"));
    ("id'[id]", OK ("∀a. a → a"));
    ("id''[id]", OK ("∀a. a → a"));
    ("λids. ids'[ids]", Unsafe);
    ("poly[id[id]]", OK ("tuple[int, bool]"));
    ("length[ids]", OK ("int"));
    ("map[head, single[ids]]", OK ("list[∀a. a → a]"));
    ("apply[id, 1]", OK ("int"));
    ("poly[magic]", OK ("tuple[int, bool]"));
    ("magid[magic]", OK ("∀a b. a → b"));
    ("λf : ∀a b. a → b. f : ∀a. a → a",
     OK ("(∀a b. a → b) → (∀a. a → a)"));
    ("λf : ∀a b. a → b. a = magid[f] in 1",
     OK ("(∀a b. a → b) → int"));
    ("const = (any : ∀a. a → (∀b. b → a)) in const[any]"),
     OK ("∀a b. a → b");
    ("Y(f) = λx. f[x] in f", OK ("∀a b. a → b"));
    ("Y(foldl) = λf a l.
      empty[l] ? a | foldl[f, f[a, head[l]], tail[l]]
      in foldl",
     OK ("∀a b.(a → b → a) → a → list[b] → a"));
    ("Y(foldr) = λf l a.
      empty[l] ? a | f[head[l], foldr[f, tail[l], a]]
      in foldr",
     OK ("∀a b. (a → b → b) → list[a] → b → b"));
    ("Y(foldl) = λf a l.
      empty[l] ? a | foldl[f, f[a, head[l]], tail[l]]
      in λl. foldl[λa b. (a + 1), 0, l]",
     OK ("∀a. list[a] → int"));
    ("Y(iter) = λf l.
      empty[l] ? ø | (f[head[l]]; iter[f, tail[l]])
      in iter",
     OK ("∀a b. (a → b) → list[a] → unit"));
    ("iter =
        Y(foldl) = λf a l. empty[l] ? a | foldl[f, f[a, head[l]], tail[l]]
        in λf l. foldl[λa x. f[x], ø, l]
      in λl. iter[num, l]",
     OK ("list[int] → unit"));
    ("λf a b. (f[a] equal b)",
     OK ("∀a b. (a → b) → a → b → bool"));
    ("λx : ∃a. a → a. x",
     OK ("∀a. (a → a) → (a → a)"));
    ("λx : ∀a. a → a. x",
     OK ("∀a. (∀b. b → b) → (a → a)"));
    ("λf : ∃a b. a → b x y. equal[f[x], y]",
     OK ("∀a b. (a → b) → a → b → bool"));
    ("foo = λx : ∃a. a. x in foo",
     OK ("∀a. a → a"));
    ("λf : ∀a b. a → b x y. equal[f[x], y]",
     OK ("∀a b.(∀c d. c → d) → a → b → bool"));
  ]

let to_string = function
  | Fail e -> "Fail: " ^ (Printexc.to_string e)
  | OK ty -> "OK: " ^ ty
  | Unsafe -> "Fail"

let normalize ty =
  let lexbuf = Sedlexing.Utf8.from_string ty in
  let token, start, stop = Ulexer.parse () in
  try Type.to_string (Uparser.single_ty token (Sedlexing.Utf8.from_string ty))
  with
  | Uparser.Error ->
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
        let token, _, _ = Ulexer.parse () in
        let ty = Synthesis.eval ~env:Core.core
            (Uparser.single_expr token (Sedlexing.Utf8.from_string expr)) in
        OK (Type.to_string ty)
      with Synthesis.Error (_, exn) -> Fail exn
         | exn -> Fail exn
    in
    assert_equal ~printer:to_string ~cmp:compare re result

let suite = "Synthesis test" >::: List.map make_test tests

let () =
  try run_test_tt_main suite
  with exn -> Printexc.to_string exn |> print_endline
