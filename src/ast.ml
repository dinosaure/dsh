type t =
  | Var of (Loc.t * string)
  | App of (Loc.t * t * t list)
  | Abs of (Loc.t * (string * annotation option) list * t)
  | Let of (Loc.t * string * t * t)
  | Rec of (Loc.t * string * t * t)
  | Ann of (Loc.t * t * annotation)
  | If of (Loc.t * t * t * t)
  | Seq of (Loc.t * t * t)
  | Unit of Loc.t
  | Int of (Loc.t * int)
  | Bool of (Loc.t * bool)
  | Char of (Loc.t * char)
  | Alias of (Loc.t * string * Type.t * t)
  | Tuple of (Loc.t * t list)
  | Case of (Loc.t * t * (Pattern.t * t) list)
  | Variant of (Loc.t * string * t)
  | RecordSelect of (Loc.t * t * string)
  | RecordExtend of (Loc.t * t list Type.Set.t * t)
  | RecordRestrict of (Loc.t * t * string)
  | RecordEmpty of Loc.t
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

  let add_set ?(sep="") add_data buffer set =
    add_list ~sep add_data buffer (Type.Set.bindings set)
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
  | Tuple (loc, _) -> loc
  | Case (loc, _, _) -> loc
  | Variant (loc, _, _) -> loc
  | RecordSelect (loc, _, _) -> loc
  | RecordExtend (loc, _, _) -> loc
  | RecordRestrict (loc, _, _) -> loc
  | RecordEmpty loc -> loc

let rec is_annotated = function
  | Ann (_, _, _) -> true
  | Let (_, _, _, c) -> is_annotated c
  | _ -> false

let to_string tree =
  let buffer = Buffer.create 16 in
  let rec compute buffer = function
    | Var (_, name) -> Buffer.add_string buffer name
    | App (_, f, a) ->
      Printf.bprintf buffer "%a[%a]"
        compute f
        (Buffer.add_list ~sep:" " compute) a
    | Abs (_, a, c) ->
      Printf.bprintf buffer "λ%a. %a"
        (Buffer.add_list ~sep:" " Buffer.add_argument) a
        compute c
    | Let (_, name, e, c) ->
      Printf.bprintf buffer "%s = %a in %a"
        name
        compute e
        compute c
    | Rec (_, name, e, c) ->
      Printf.bprintf buffer "Y(%s) = %a in %a)"
        name
        compute e
        compute c
    | Ann (_, e, ann) ->
      Printf.bprintf buffer "%a : %a"
        compute e
        Buffer.add_annotation ann
    | If (_, i, a, b) ->
      Printf.bprintf buffer "%a ? %a | %a"
        compute i
        compute a
        compute b
    | Seq (_, a, b) ->
      Printf.bprintf buffer "%a %a"
        compute a
        compute b
    | Int (_, i) ->
      Printf.bprintf buffer "%d" i
    | Bool (_, b) ->
      Printf.bprintf buffer "%s" (if b then "true" else "false")
    | Char (_, c) ->
      Printf.bprintf buffer "%c" c
    | Unit _ -> Buffer.add_string buffer "ø"
    | Alias (_, name, ty, expr) ->
      raise (Failure "Not implemented")
    | Variant (_, ctor, Unit _) ->
      Printf.bprintf buffer "%s()" ctor
    | Variant (_, ctor, expr) ->
      Printf.bprintf buffer "%s(%a)"
        ctor
        compute expr
    | Tuple (_, l) ->
      raise (Failure "Not implemented")
    | Case (_, expr, l) ->
      let add_branch buffer (patterns, expr) =
        Printf.bprintf buffer "%s → %a"
          (Pattern.to_string patterns)
          compute expr
      in
      Printf.bprintf buffer "match %a { %a }"
        compute expr
        (Buffer.add_list ~sep:" | " add_branch) l
    | RecordSelect (_, r, n) ->
      Printf.bprintf buffer "%a.%s" compute r n
    | RecordExtend (_, map, rest) ->
      let add_label label buffer expr =
        Printf.bprintf buffer "%s = %a" label compute expr in
      let add_map buffer map =
        Printf.bprintf buffer "%a"
          (Buffer.add_set ~sep:", "
          (fun buffer (label, lst) ->
            Printf.bprintf buffer "%a"
              (Buffer.add_list ~sep:", " (add_label label))
              lst))
        map
      in
      begin
        match rest with
        | RecordEmpty _ ->
          Printf.bprintf buffer "{ %a }" add_map map
        | expr ->
          Printf.bprintf buffer "{ %a | %a }"
            add_map map
            compute expr
      end
    | RecordRestrict (_, expr, label) ->
      Printf.bprintf buffer "{ %a - %s }"
        compute expr
        label
    | RecordEmpty _ -> Buffer.add_string buffer "{}"

  in compute buffer tree; Buffer.contents buffer
