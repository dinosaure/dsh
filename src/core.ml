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

  |> add "head    : (forall (a) ((list a) -> a))"
  |> add "tail    : (forall (a) ((list a) -> (list a)))"
  |> add "nill    : (forall (a) (list a))"
  |> add "cons    : (forall (a) (a -> (list a) -> (list a)))"
  |> add "single  : (forall (a) (a -> (list a)))"
  |> add "map     : (forall (a b) ((a -> b) -> (list a) -> (list b)))"
  |> add "length  : (forall (a) ((list a) -> int))"
  |> add "empty   : (forall (a) ((list a) -> bool))"

  |> add "succ    : (int -> int)"
  |> add "pred    : (int -> int)"
  |> add "+       : (int -> int -> int)"
  |> add "-       : (int -> int -> int)"
  |> add "*       : (int -> int -> int)"
  |> add "/       : (int -> int -> int)"
  |> add "%       : (int -> int -> int)"

  |> add "=       : (forall (a) (a -> a -> bool))"
  |> add "==      : (forall (a) (a -> a -> bool))"
  |> add "<>      : (forall (a) (a -> a -> bool))"
  |> add "!=      : (forall (a) (a -> a -> bool))"
  |> add ">       : (int -> int -> bool)"
  |> add "<       : (int -> int -> bool)"
  |> add ">=      : (int -> int -> bool)"
  |> add "<=      : (int -> int -> bool)"
  |> add "not     : (bool -> bool)"
  |> add "and     : (bool -> bool -> bool)"
  |> add "or      : (bool -> bool -> bool)"

  |> add ",       : (forall (a b) (a -> b -> (pair a b)))"
  |> add "fst     : (forall (a b) ((pair a b) -> a))"
  |> add "snd     : (forall (a b) ((pair a b) -> b))"

  |> add "#num    : (int -> unit)"
  |> add "#bln    : (bool -> unit)"
  |> add "#chr    : (char -> unit)"

  |> add "any     : (forall (a) a)"
  |> add "magic   : (forall (a b) (a -> b))"
  |> add "poly    : ((forall (a) (a -> a)) -> (pair int bool))"
  |> add "special : (((forall (a) (a -> a)) -> (forall (a) (a -> a)))
                     -> (forall (a) (a -> a)))"
  |> add "id'     : ((forall (a) (a -> a)) -> (forall (a) (a -> a)))"
  |> add "id''    : (forall (a) ((forall (a) (a -> a)) -> (a -> a)))"
  |> add "ids     : (list (forall (a) (a -> a)))"
  |> add "ids'    : ((list (forall (a) (a -> a)))
                     -> (list (forall (a) (a -> a))))"
  |> add "magid   : ((forall (a b) (a -> b)) -> (forall (a b) (a -> b)))"

let add name func env =
  Interpreter.Environment.add name (Interpreter.Primitive func) env

let runtime =
  let raise_error name = raise (Invalid_argument name) in
  let open Interpreter in
  Environment.empty
  |> add "+" (function [Int a; Int b] -> Int (a + b) | _ -> raise_error "+")
  |> add "-" (function [Int a; Int b] -> Int (a - b) | _ -> raise_error "-")
  |> add "*" (function [Int a; Int b] -> Int (a * b) | _ -> raise_error "*")
  |> add "/" (function [Int a; Int b] -> Int (a / b) | _ -> raise_error "/")
  |> add "%" (function [Int a; Int b] -> Int (a mod b) | _ -> raise_error "%")
  |> add "succ" (function [Int a] -> Int (a + 1) | _ -> raise_error "succ")
  |> add "pred" (function [Int a] -> Int (a - 1) | _ -> raise_error "pred")

  |> add "=" (function [a; b] -> Bool (a = b) | _ -> raise_error "=")
  |> add "==" (function [a; b] -> Bool (a == b) | _ -> raise_error "=")
  |> add "<>" (function [a; b] -> Bool (a <> b) | _ -> raise_error "=")
  |> add "!=" (function [a; b] -> Bool (a != b) | _ -> raise_error "=")
  |> add ">" (function [Int a; Int b] -> Bool (a > b) | _ -> raise_error ">")
  |> add "<" (function [Int a; Int b] -> Bool (a < b) | _ -> raise_error "<")
  |> add ">=" (function [Int a; Int b] -> Bool (a >= b) | _ -> raise_error ">=")
  |> add "<=" (function [Int a; Int b] -> Bool (a <= b) | _ -> raise_error "<=")
  |> add "not" (function [Bool a] -> Bool (not a) | _ -> raise_error "not")

  |> add "#num"
    (function [Int a] -> print_int a; Unit
            | _ -> raise_error "#num")
  |> add "#bln"
    (function [Bool b] -> print_string (if b then "true" else "false"); Unit
            | _ -> raise_error "#bln")
  |> add "#chr"
    (function [Char c] -> print_string (String.make 1 c); Unit
            | _ -> raise_error "#chr")

  |> add "and" (function [Bool a; Bool b] -> Bool (a && b)
                       | _ -> raise_error "and")
  |> add "or" (function [Bool a; Bool b] -> Bool (a || b)
                      | _ -> raise_error "or")

  |> add "," (function l -> Tuple l)
  |> add "fst" (function [Tuple l] -> List.hd l | _ -> raise_error "fst")
  |> add "snd" (function [Tuple l] -> List.tl l |> List.hd
                       | _ -> raise_error "snd")
