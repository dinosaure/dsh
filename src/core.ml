let add expr env =
  let expr = Parser.single_expr Lexer.token (Lexing.from_string expr) in
  match expr with
  | Ast.Ann (_, Ast.Var (_, name), ([], ty)) ->
    Synthesis.Environment.add name ty env
  | _ -> raise (Invalid_argument "Core.add")

let core =
  Synthesis.Environment.empty
  |> add "id          : (forall (a) (a -> a))"
  |> add "one         : int"
  |> add "zero        : int"
  |> add "succ        : (int -> int)"
  |> add "+           : (int -> int -> int)"
  |> add ",           : (forall (a b) (a -> b -> (pair a b)))"
  |> add "fst         : (forall (a b) ((pair a b) -> a))"
  |> add "snd         : (forall (a b) ((pair a b) -> b))"
  |> add "=           : (forall (a) (a -> a -> bool))"
  |> add "true        : bool"
  |> add "false       : bool"
  |> add "not         : (bool -> bool)"
  |> add "choose      : (forall (a) (a -> a -> a))"
  |> add "apply       : (forall (a b) ((a -> b) -> a -> b))"

  |> add "head        : (forall (a) ((list a) -> a))"
  |> add "tail        : (forall (a) ((list a) -> (list a)))"
  |> add "nill        : (forall (a) (list a))"
  |> add "cons        : (forall (a) (a -> (list a) -> (list a)))"
  |> add "single      : (forall (a) (a -> (list a)))"
  |> add "map         : (forall (a b) ((a -> b) -> (list a) -> (list b)))"
