let add expr env =
  let expr = Parser.single_expr Lexer.token (Lexing.from_string expr) in
  match expr with
  | Ast.Ann (_, Ast.Var (_, name), ([], ty)) ->
    Synthesis.Environment.add name ty env
  | _ -> raise (Invalid_argument "Core.add")

let core =
  Synthesis.Environment.empty
  |> add "id      : (forall (a) (a -> a))"
  |> add "choose  : (forall (a) (a -> a -> a))"
  |> add "apply   : (forall (a b) ((a -> b) -> a -> b))"
  |> add "const   : (forall (a b) (a -> b -> a))"
  |> add "ids     : (list (forall (a) (a -> a)))"
  |> add "idd     : ((forall (a) (a -> a)) -> (forall (a) (a -> a)))"
  |> add "iddd    : (forall (a) ((forall (a) (a -> a)) -> a -> a))"
  |> add "idds    : ((list (forall (a) (a -> a)))
                     -> (list (forall (a) (a -> a))))"
  |> add "special : (((forall (a) (a -> a)) -> (forall (a) (a -> a)))
                     -> (forall (a) (a -> a)))"

  |> add "head    : (forall (a) ((list a) -> a))"
  |> add "tail    : (forall (a) ((list a) -> (list a)))"
  |> add "nill    : (forall (a) (list a))"
  |> add "cons    : (forall (a) (a -> (list a) -> (list a)))"
  |> add "single  : (forall (a) (a -> (list a)))"
  |> add "map     : (forall (a b) ((a -> b) -> (list a) -> (list b)))"
  |> add "length  : (forall (a) ((list a) -> int))"

  |> add "one     : int"
  |> add "zero    : int"
  |> add "succ    : (int -> int)"
  |> add "+       : (int -> int -> int)"

  |> add "=       : (forall (a) (a -> a -> bool))"
  |> add "true    : bool"
  |> add "false   : bool"
  |> add "not     : (bool -> bool)"

  |> add ",       : (forall (a b) (a -> b -> (pair a b)))"
  |> add "fst     : (forall (a b) ((pair a b) -> a))"
  |> add "snd     : (forall (a b) ((pair a b) -> b))"

  |> add "any     : (forall (a) a)"
  |> add "magic   : (forall (a b) (a -> b))"
  |> add "poly    : ((forall (a) (a -> a)) -> (pair int bool))"
