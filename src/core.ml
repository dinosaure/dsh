let add expr env =
  let lexbuf = Sedlexing.Utf8.from_string expr in
  let token, start, stop = ULexer.parse () in
  try
    let expr = UParser.single_expr token lexbuf in
    match expr with
    | Ast.Ann (_, Ast.Var (_, name), ([], ty)) ->
      Synthesis.Environment.add name ty env
    | _ -> raise (Invalid_argument ("Core.add: " ^ (Ast.to_string expr)))
  with
  | UParser.Error ->
    let loc = Loc.make
        (start lexbuf)
        (stop lexbuf)
    in Printf.printf "Parsing error at:\n%s\n%!"
      (Loc.to_string_of_line loc expr); env
  | ULexer.Lexical_error ->
    let loc = Loc.make
        (start lexbuf)
        (stop lexbuf)
    in Printf.printf "Lexical error at:\n%s%!"
      (Loc.to_string_of_line loc expr); env

let core =
  Synthesis.Environment.empty
  |> add "id      : ∀a. (a → a)"
  |> add "choose  : ∀a. (a → a → a)"
  |> add "apply   : ∀a b. ((a → b) → a → b)"
  |> add "const   : ∀a b. (a → b → a)"

  |> add "head    : ∀a. (list[a] → a)"
  |> add "tail    : ∀a. (list[a] → list[a])"
  |> add "nill    : ∀a. list[a]"
  |> add "cons    : ∀a. (a → list[a] → list[a])"
  |> add "single  : ∀a. (a → list[a])"
  |> add "map     : ∀a b. ((a → b) → list[a] → list[b])"
  |> add "length  : ∀a. (list[a] → int)"
  |> add "empty   : ∀a. (list[a] → bool)"

  |> add "succ    : (int → int)"
  |> add "pred    : (int → int)"
  |> add "+       : (int → int → int)"
  |> add "-       : (int → int → int)"
  |> add "*       : (int → int → int)"
  |> add "/       : (int → int → int)"
  |> add "%       : (int → int → int)"

  |> add "equal   : ∀a. (a → a → bool)"
  |> add "<>      : ∀a. (a → a → bool)"
  |> add ">       : (int → int → bool)"
  |> add "<       : (int → int → bool)"
  |> add ">=      : (int → int → bool)"
  |> add "<=      : (int → int → bool)"

  |> add "not     : (bool → bool)"
  |> add "and     : (bool → bool → bool)"
  |> add "or      : (bool → bool → bool)"

  |> add "fst     : ∀a b. (tuple[a, b] → a)"
  |> add "snd     : ∀a b. (tuple[a, b] → b)"

  |> add "num     : (int → unit)"
  |> add "bln     : (bool → unit)"
  |> add "chr     : (char → unit)"

  |> add "any     : ∀a. a"
  |> add "magic   : ∀a b. (a → b)"
  |> add "poly    : ((∀a. (a → a)) → tuple[int, bool])"
  |> add "special : ((∀a. (a → a)) → (∀a. (a → a)) → (∀a. (a → a)))"
  |> add "id'     : ((∀a. (a → a)) → (∀a. (a → a)))"
  |> add "id''    : ∀a. ((∀a. (a → a)) → (a → a))"
  |> add "ids     : list[∀a. (a → a)]"
  |> add "ids'    : (list[∀a. (a → a)] → list[∀a. (a → a)])"
  |> add "magid   : ((∀a b. (a → b)) → (∀a b. (a → b)))"

  |> add "fix     : ∀a v. ((a → b) → (a → b)) → (a → b)"

let add name func env =
  Interpreter.Environment.add name (Interpreter.Primitive func) env

let runtime =
  let raise_error name = raise (Invalid_argument name) in
  let open Interpreter in
  Environment.empty
  |> add "id"     (function [x] -> x | _ -> raise_error "id")
  |> add "choose" (function [x; y] -> if Random.bool () then x else y
                          | _ -> raise_error "choose")
  |> add "apply"
    (function [Closure (ext, [ name ], body, None); a] ->
                Interpreter.eval
                  (Interpreter.Environment.add name a ext) body
            | [(Closure (ext, [ name ], body, Some fix)) as closure; a] ->
                Interpreter.eval
                  (Interpreter.Environment.extend ext [fix; name] [closure; a])
                  body
            | _ -> raise_error "apply")
  |> add "const"  (function [a; b] -> a | _ -> raise_error "const")

  |> add "head"   (function [List l] -> List.hd l | _ -> raise_error "head")
  |> add "tail"   (function [List l] -> List (List.tl l)
                          | _ -> raise_error "tail")
  |> Environment.add "nill" (List [])
  |> add "cons"   (function [e; List l] -> List (e :: l)
                          | _ -> raise_error "cons")
  |> add "single" (function [e] -> List [ e ] | _ -> raise_error "single")
  |> add "empty"  (function [List l] -> Bool (List.length l = 0)
                          | _ -> raise_error "empty")

  |> add "succ"   (function [Int n] -> Int (n + 1) | _ -> raise_error "succ")
  |> add "pred"   (function [Int n] -> Int (n - 1) | _ -> raise_error "pred")

  |> add "+"      (function [Int a; Int b] -> Int (a + b)
                          | _ -> raise_error "+")
  |> add "-"      (function [Int a; Int b] -> Int (a - b)
                          | _ -> raise_error "-")
  |> add "*"      (function [Int a; Int b] -> Int (a * b)
                          | _ -> raise_error "*")
  |> add "/"      (function [Int a; Int b] -> Int (a / b)
                          | _ -> raise_error "/")
  |> add "%"      (function [Int a; Int b] -> Int (a mod b)
                          | _ -> raise_error "%")

  |> add "equal"  (function [a; b] -> Bool (a = b)
                          | _ -> raise_error "equal")
  |> add "<>"     (function [a; b] -> Bool (a <> b)
                          | _ -> raise_error "<>")
  |> add ">"      (function [Int a; Int b] -> Bool (a > b)
                           | _ -> raise_error ">")
  |> add "<"      (function [Int a; Int b] -> Bool (a < b)
                           | _ -> raise_error "<")
  |> add ">="     (function [Int a; Int b] -> Bool (a >= b)
                          | _ -> raise_error ">=")
  |> add "<="     (function [Int a; Int b] -> Bool (a <= b)
                          | _ -> raise_error "<=")
  |> add "not"    (function [Bool a] -> Bool (not a)
                          | _ -> raise_error "not")
  |> add "and"    (function [Bool a; Bool b] -> Bool (a && a)
                          | _ -> raise_error "and")
  |> add "or"     (function [Bool a; Bool b] -> Bool (a || a)
                          | _ -> raise_error "or")

  |> add "fst"    (function [Tuple [a; _]] -> a
                          | _ -> raise_error "fst")
  |> add "snd"    (function [Tuple [_; b]] -> b
                          | _ -> raise_error "snd")

  |> add "num"    (function [Int a] -> print_int a; Unit
                          | _ -> raise_error "num")
  |> add "bln"    (function [Bool a] ->
                            print_string (if a then "true" else "false"); Unit
                          | _ -> raise_error "bln")
  |> add "chr"    (function [Char a] -> print_string (String.make 1 a); Unit
                          | _ -> raise_error "chr")
  |> Environment.add "any"  Unit
