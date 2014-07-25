type t =
  | Const of string
  | App of (t * t list)
  | Arrow of (t list * t)
  | Var of var ref
  | Forall of (int list * t)
and var =
  | Unbound of int * int
  | Bound of int
  | Link of t
  | Generic of int

module Buffer = struct
  include Buffer

  let add_list ?(sep="") add_data buffer lst =
    let rec aux = function
      | [] -> ()
      | [ x ] -> add_data buffer x
      | x :: r ->
        Printf.bprintf buffer "%a%s" add_data x sep; aux r
    in aux lst
end

module String = struct
  include String

  let of_list ?(sep="") to_string lst =
    let buffer = Buffer.create 16 in
    let rec aux = function
      | [] -> ()
      | [ x ] -> Printf.bprintf buffer "%s" (to_string x)
      | x :: r -> Printf.bprintf buffer "%s%s" (to_string x) sep; aux r
    in aux lst; Buffer.contents buffer
end

module Map = struct
  include Map.Make (struct type t = int let compare = compare end)

  let generate i =
    (String.make 1 (char_of_int (int_of_char 'a' + (i mod 26))))
    ^ (if i >= 26 then (string_of_int (i / 26)) else "")

  let extend env ids =
    let lst, env = List.fold_left
        (fun (lst, env) id ->
           let name = generate (cardinal env) in
           (name :: lst, add id name env))
        ([], env) ids
    in
    (List.rev lst, env)
end

let rec unlink = function
  | Var ({ contents = Link ty } as t) ->
    let ty = unlink ty in
    t := Link ty; ty
  | ty -> ty

let rec is_monomorphic = function
  | Forall _ -> false
  | Const _ -> true
  | Var { contents = Link ty } -> is_monomorphic ty
  | Var _ -> true
  | App (f, a) ->
    is_monomorphic f && List.for_all is_monomorphic a
  | Arrow (a, r) ->
    List.for_all is_monomorphic a && is_monomorphic r

let memoize f =
  let cache = Hashtbl.create 16 in
  let rec aux x =
    try Hashtbl.find cache x
    with Not_found ->
      let y = f aux x in
      Hashtbl.add cache x y; y
  in aux

let to_string ?(env = Map.empty) ty =
  let count_unbound = ref 0 in
  let name_of_unbound =
    memoize (fun self id ->
        let i = !count_unbound in
        incr count_unbound;
        let name = String.make 1 (Char.chr (97 + i mod 26)) ^
                   if i >= 26 then string_of_int (i / 26) else ""
        in "_" ^ name)
  in
  let rec atom ?(first = false) env buffer = function
    | Const name ->
      Buffer.add_string buffer name
    | App (f, a) ->
      Printf.bprintf buffer "(%a %a)"
        (atom ~first env) f
        (Buffer.add_list ~sep:" " (expr ~first env)) a
    | Forall (ids, ty) ->
      let lst, env = Map.extend env ids in
      Printf.bprintf buffer "(forall (%a) %a)"
        (Buffer.add_list ~sep:" " Buffer.add_string) lst
        (expr ~first:true env) ty
    | Var { contents = Unbound (id, _) } ->
      Printf.bprintf buffer "%s" (name_of_unbound id)
    | Var { contents = Bound id } -> Buffer.add_string buffer (Map.find id env)
    | Var { contents = Generic id } ->
      Printf.bprintf buffer "%s" (name_of_unbound id)
    | Var { contents = Link ty } -> atom ~first env buffer ty
    | ty -> Printf.bprintf buffer "(%a)" (expr env) ty
  and expr ?(first = false) env buffer = function
    | Arrow (a, r) ->
      let func = if List.length a = 0 then expr env else atom env in
      Printf.bprintf buffer "%s%a -> %a%s"
        (if first then "(" else "")
        (Buffer.add_list ~sep:" -> " func) a
        (expr env) r
        (if first then ")" else "")
    | Var { contents = Link ty } -> expr ~first env buffer ty
    | ty -> atom ~first env buffer ty
  in
  let buffer = Buffer.create 16 in
  expr ~first:true env buffer ty; Buffer.contents buffer

let rec copy = function
  | Const name -> Const name
  | App (f, a) -> App (copy f, List.map copy a)
  | Arrow (a, r) -> Arrow (List.map copy a, copy r)
  | Var { contents = Link a } -> Var { contents = Link (copy a) }
  | Var ref -> Var (BatRef.copy ref)
  | Forall (lst, ty) -> Forall (lst, copy ty)
