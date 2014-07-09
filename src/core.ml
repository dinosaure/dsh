let add name ty env =
  let ty = Parser.single_forall Lexer.token (Lexing.from_string ty) in
  W.Environment.add name ty env

let core =
  W.Environment.empty
  |> add "id"     "(forall (a) (a -> a))"
  |> add "one"    "int"
  |> add "zero"   "int"
  |> add "succ"   "(int -> int)"
  |> add "+"      "(int -> int -> int)"
  |> add ","      "(forall (a b) (a -> b -> (pair a b)))"
  |> add "fst"    "(forall (a b) ((pair a b) -> a))"
  |> add "snd"    "(forall (a b) ((pair a b) -> b))"
  |> add "="      "(forall (a) (a -> a -> bool))"
  |> add "true"   "bool"
  |> add "false"  "bool"
  |> add "not"    "(bool -> bool)"
  |> add "choose" "(forall (a) (a -> a -> a))"
  |> add "apply"  "(forall (a b) ((a -> b) -> b -> b))"
  |> add "head"   "(forall (a) ((list a) -> a))"
  |> add "tail"   "(forall (a) ((list a) -> (list a)))"
  |> add "nill"   "(forall (a) (list a))"
  |> add "cons"   "(forall (a) (a -> (list a) -> (list a)))"
