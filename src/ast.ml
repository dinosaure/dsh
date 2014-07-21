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
and annotation = (int list * Type.t)

type i =
  | Def of (Location.t * string * t)
  | Expr of (Location.t * t)

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
    let lst, map = Type.Map.extend Type.Map.empty lst in
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
      Printf.bprintf buffer "(lambda (%a) %a)"
        (Buffer.add_list ~sep:" " Buffer.add_argument) a
        compute c
    | Let (_, name, e, c) ->
      Printf.bprintf buffer "(let (%s %a) %a)"
        name
        compute e
        compute c
    | Rec (_, name, e, c) ->
      Printf.bprintf buffer "(rec (%s %a) %a)"
        name
        compute e
        compute c
    | Ann (_, e, ann) ->
      Printf.bprintf buffer "%a : %a"
        compute e
        Buffer.add_annotation ann
    | If (_, i, a, b) ->
      Printf.bprintf buffer "(if %a %a %a)"
        compute i
        compute a
        compute b
    | Int (_, i) ->
      Printf.bprintf buffer "%d" i
    | Bool (_, b) ->
      Printf.bprintf buffer "%s" (if b then "true" else "false")
    | Char (_, c) ->
      Printf.bprintf buffer "%c" c
    | Unit _ -> Buffer.add_string buffer "()"
  in compute buffer tree; Buffer.contents buffer
