type t =
  | Var of (Location.t * string)
  | App of (Location.t * t * t list)
  | Abs of (Location.t * (string * annotation option) list * t)
  | Let of (Location.t * string * t * t)
  | Ann of (Location.t * t * annotation)
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
        (Buffer.add_list ~sep:", " Buffer.add_argument) a
        compute c
    | Let (_, name, e, c) ->
      Printf.bprintf buffer "(let (%s %a) %a)"
        name
        compute e
        compute c
    | Ann (_, e, ann) ->
      Printf.bprintf buffer "(%a : %a)"
        compute e
        Buffer.add_annotation ann
  in compute buffer tree; Buffer.contents buffer
