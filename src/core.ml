let add expr env =
  let lexbuf = Sedlexing.Utf8.from_string expr in
  let token, start, stop = Ulexer.parse () in
  try
    let expr = Uparser.single_expr token lexbuf in
    match expr with
    | Ast.Ann (_, Ast.Var (_, name), ([], ty)) ->
      Synthesis.Environment.add name ty env
    | _ -> raise (Invalid_argument ("Core.add: " ^ (Ast.to_string expr)))
  with
  | Uparser.Error ->
    let loc = Location.make
        (start lexbuf)
        (stop lexbuf)
    in Printf.printf "Parsing error at:\n%s\n%!"
      (Location.to_string_of_line loc expr); env
  | Ulexer.Lexical_error ->
    let loc = Location.make
        (start lexbuf)
        (stop lexbuf)
    in Printf.printf "Lexical error at:\n%s%!"
      (Location.to_string_of_line loc expr); env

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

let add name func env =
  Interpreter.Environment.add name (Interpreter.Primitive func) env

let runtime =
  let raise_error name = raise (Invalid_argument name) in
  let open Interpreter in
  Environment.empty
