type t =
  | Var of (Location.t * string)
  | App of (Location.t * t * t list)
  | Abs of (Location.t * (string * annotation option) list * t)
  | Let of (Location.t * string * t * t)
  | Rec of (Location.t * string * t * t)
  | Ann of (Location.t * t * annotation)
  | If of (Location.t * t * t * t)
  | Seq of (Location.t * t * t)
  | Unit of Location.t
  | Int of (Location.t * int)
  | Bool of (Location.t * bool)
  | Char of (Location.t * char)
  | Alias of (Location.t * string * Type.t * t)
  | Variant of (Location.t * string * t)
  | Tuple of (Location.t * t list)
  | Match of (Location.t * t * (Pattern.t * t) list)
and annotation = (int list * Type.t)

module Buffer = struct
  include Buffer

  let add_list ?(sep="") add_data buffer lst =
    let rec aux = function
      | [] -> ()
      | [ x ] -> add_data buffer x
      | x :: r ->
        Printf.bprintf buffer "%a%s" add_data x sep; aux r
    in aux lst

  let add_annotation buffer (lst, ty) =
    let lst, map = Type.Environment.extend Type.Environment.empty lst in
    match lst with
    | [] -> Buffer.add_string buffer (Type.to_string ~env:map ty)
    | lst ->
      Printf.bprintf buffer "(some (%a) %s)"
        (add_list ~sep:" " Buffer.add_string) lst
        (Type.to_string ~env:map ty)

  let add_argument buffer (name, opt) =
    match opt with
    | Some ann ->
      Printf.bprintf buffer "%s : %a" name add_annotation ann
    | None -> add_string buffer name
end

module List = struct
  include List

  let rec make f n =
    let rec aux acc = function
      | 0 -> List.rev acc
      | n -> aux (f n :: acc) (n - 1)
    in aux [] n
end

let loc = function
  | Var (loc, _) -> loc
  | App (loc, _, _) -> loc
  | Abs (loc, _, _) -> loc
  | Let (loc, _, _, _) -> loc
  | Rec (loc, _, _, _) -> loc
  | Ann (loc, _, _) -> loc
  | If (loc, _, _, _) -> loc
  | Seq (loc, _, _) -> loc
  | Unit loc -> loc
  | Int (loc, _) -> loc
  | Bool (loc, _) -> loc
  | Char (loc, _) -> loc
  | Alias (loc, _, _, _) -> loc
  | Variant (loc, _, _) -> loc
  | Tuple (loc, _) -> loc
  | Match (loc, _, _) -> loc

let rec is_annotated = function
  | Ann (_, _, _) -> true
  | Let (_, _, _, c) -> is_annotated c
  | _ -> false

let to_string tree =
  let buffer = Buffer.create 16 in
  let rec compute buffer = function
    | Var (_, name) -> Buffer.add_string buffer name
    | App (_, f, a) ->
      Printf.bprintf buffer "(%a %a)"
        compute f
        (Buffer.add_list ~sep:" " compute) a
    | Abs (_, a, c) ->
      Printf.bprintf buffer "(\\ (%a) %a)"
        (Buffer.add_list ~sep:" " Buffer.add_argument) a
        compute c
    | Let (_, name, e, c) ->
      Printf.bprintf buffer "(: (%s %a) %a)"
        name
        compute e
        compute c
    | Rec (_, name, e, c) ->
      Printf.bprintf buffer "(Y (%s %a) %a)"
        name
        compute e
        compute c
    | Ann (_, e, ann) ->
      Printf.bprintf buffer "%a : %a"
        compute e
        Buffer.add_annotation ann
    | If (_, i, a, b) ->
      Printf.bprintf buffer "(? %a %a %a)"
        compute i
        compute a
        compute b
    | Seq (_, a, b) ->
      Printf.bprintf buffer "[%a; %a]"
        compute a
        compute b
    | Int (_, i) ->
      Printf.bprintf buffer "%d" i
    | Bool (_, b) ->
      Printf.bprintf buffer "%s" (if b then "true" else "false")
    | Char (_, c) ->
      Printf.bprintf buffer "%c" c
    | Unit _ -> Buffer.add_string buffer "()"
    | Alias (_, name, ty, expr) ->
      Printf.bprintf buffer "(= %s %s %a)"
        name
        (Type.to_string ty)
        compute expr
    | Variant (_, ctor, Unit _) ->
      Buffer.add_string buffer ctor
    | Variant (_, ctor, expr) ->
      Printf.bprintf buffer "(~ %s %a)"
        ctor
        compute expr
    | Tuple (_, l) ->
      Printf.bprintf buffer "(%a)"
        (Buffer.add_list ~sep:", " compute) l
    | Match (_, expr, l) ->
      let add_branch buffer (pattern, expr) =
        Printf.bprintf buffer "(%s %a)"
          (Pattern.to_string pattern)
          compute expr
      in
      Printf.bprintf buffer "(| %a %a)"
        compute expr
        (Buffer.add_list ~sep:" " add_branch) l

  in compute buffer tree; Buffer.contents buffer
