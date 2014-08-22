module Environment = struct
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

module Set = struct
  include Map.Make(String)

  let of_list lst =
    List.fold_left
      (fun acc (key, value) -> add key value acc)
      empty lst

  let to_list set =
    fold (fun key value acc -> (key, value) :: acc) set []

  let iter2 func set1 set2 =
    try List.iter2 func (to_list set1) (to_list set2)
    with Invalid_argument _ -> raise (Invalid_argument ("Type.Set.iter2"))
end

type t =
  | Const of string
  | Primitive of string
  | App of (t * t list)
  | Arrow of (t list * t)
  | Var of var ref
  | Forall of (int list * t)
  | Alias of (string * t)
  | Set of t Set.t
  | Abs of (string list * t)
and var =
  | Unbound of int * int
  | Bound of int
  | Link of t
  | Generic of int

module Variable = struct
  let id = ref 0

  let next () =
    let i = !id in
    incr id; i

  let reset () =
    id := 0

  let make level = Var (ref (Unbound (next (), level)))
  let generic () = Var (ref (Generic (next ())))
  let bound () = let id = next () in (id, Var (ref (Bound id)))
end

module Primitive = struct
  type t = (string, unit) Hashtbl.t

  exception Primitive_already_exists of string

  let () = Printexc.register_printer
    (function
      | Primitive_already_exists name ->
        Some (Printf.sprintf "primitive %s already exists" name)
      | _ -> None)

  let set =
    let empty = Hashtbl.create 16 in
    Hashtbl.add empty "int" ();
    Hashtbl.add empty "char" ();
    Hashtbl.add empty "bool" ();
    Hashtbl.add empty "unit" ();
    Hashtbl.add empty "*" ();
    empty

  let add name =
    try let _ = Hashtbl.find set name in
        raise (Primitive_already_exists name)
    with Not_found -> Hashtbl.add set name ()

  let exists name =
    try let _ = Hashtbl.find set name in true
    with Not_found -> false

  let int = Primitive "int"
  let char = Primitive "char"
  let bool = Primitive "bool"
  let unit = Primitive "unit"
  let pair = Primitive "*"
end

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

let rec unlink = function
  | Var ({ contents = Link ty } as var) ->
    let ty = unlink ty in
    var := Link ty; ty
  | Alias (_, ty) -> unlink ty
  | ty -> ty

let rec is_monomorphic = function
  | Forall _ -> false
  | Const _ -> true
  | Primitive _ -> true
  | Var { contents = Link ty } -> is_monomorphic ty
  | Var _ -> true
  | App (f, a) ->
    is_monomorphic f && List.for_all is_monomorphic a
  | Arrow (a, r) ->
    List.for_all is_monomorphic a && is_monomorphic r
  | Alias (_, t) -> is_monomorphic t
  | Set l ->
    Set.for_all (fun _ -> is_monomorphic) l
  | Abs _ -> false

let memoize f =
  let cache = Hashtbl.create 16 in
  let rec aux x =
    try Hashtbl.find cache x
    with Not_found ->
      let y = f aux x in
      Hashtbl.add cache x y; y
  in aux

let to_string ?(env = Environment.empty) ty =
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
    | Primitive name ->
      Buffer.add_string buffer name
    | Alias (name, ty) ->
      Buffer.add_string buffer name
    | App (f, a) ->
      Printf.bprintf buffer "(%a %a)"
        (atom ~first env) f
        (Buffer.add_list ~sep:" " (expr ~first env)) a
    | Forall (ids, ty) ->
      let lst, env = Environment.extend env ids in
      Printf.bprintf buffer "(forall (%a) %a)"
        (Buffer.add_list ~sep:" " Buffer.add_string) lst
        (expr ~first:true env) ty
    | Abs (ids, ty) ->
      Printf.bprintf buffer "(lambda (%a) %a)"
        (Buffer.add_list ~sep:" " Buffer.add_string) ids
        (expr ~first:true env) ty
    | Var { contents = Unbound (id, _) } ->
      Printf.bprintf buffer "%s" (name_of_unbound id)
    | Var { contents = Bound id } ->
      begin
        try Buffer.add_string buffer (Environment.find id env)
        with Not_found -> Printf.bprintf buffer "#%d" id
      end
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
    | Set l ->
      let of_variant buffer = function
        | ctor, Const "unit" -> Buffer.add_string buffer ctor
        | ctor, ty ->
          Printf.bprintf buffer "(%s %a)"
            ctor
            (expr ~first env) ty
      in
      Printf.bprintf buffer "(%a)"
        (Buffer.add_list ~sep:" | " of_variant) (Set.to_list l)
    | ty -> atom ~first env buffer ty
  in
  let buffer = Buffer.create 16 in
  expr ~first:true env buffer ty; Buffer.contents buffer

let rec copy = function
  | Const name -> Const name
  | Primitive name -> Primitive name
  | App (f, a) -> App (copy f, List.map copy a)
  | Arrow (a, r) -> Arrow (List.map copy a, copy r)
  | Var { contents = Link a } -> Var { contents = Link (copy a) }
  | Var ref -> Var (BatRef.copy ref)
  | Forall (lst, ty) -> Forall (lst, copy ty)
  | Alias (s, t) -> Alias (s, copy t)
  | Set l -> Set (Set.map copy l)
  | Abs (lst, ty) -> Abs (lst, copy ty)

module S = BatSet.Make(String)
let of_list l = List.fold_right S.add l S.empty
let to_list s = S.fold (fun x a -> x :: a) s []

let free ty =
  let rec aux = function
    | Const name -> S.singleton name
    | Primitive name -> S.empty
    | App (f, a) ->
      S.union (aux f) (List.fold_left S.union S.empty (List.map aux a))
    | Arrow (a, r) ->
      S.union (List.fold_left S.union S.empty (List.map aux a)) (aux r)
    | Var { contents = Link a } -> aux a
    | Var ref -> S.empty
    | Forall (lst, ty) -> aux ty
    | Alias (s, t) -> S.empty
    (* XXX: Alias is not a declaration (like let ... in).
            It's just a fix for pretty-print *)
    | Set l -> Set.fold (fun _ x a -> S.union x a) (Set.map aux l) S.empty
    | Abs (lst, ty) -> S.diff (aux ty) (of_list lst)
  in aux ty

let fresh names set =
  let rec aux name =
    if S.mem name set
    then aux (name ^ "'")
    else name
  in List.map aux names

let rec substitute name ty ty' =
  match ty' with
  | Const name' when name = name' -> ty
  | Abs (lst, ty') ->
    let free_of_ty = free ty in
    let free_of_ty' = free ty' in
    (* Stop substitution, [name] is got bound *)
    if List.mem name lst then Abs (lst, ty')
    (* Stop, substitution, [name] ∉ [free_of_ty'] *)
    else if not (S.mem name free_of_ty') then Abs (lst, ty')
    (* Rename bound vars to avoid conflict *)
    else if not (S.inter free_of_ty (of_list lst) |> S.is_empty)
    then
      let lst' = fresh lst (S.union free_of_ty free_of_ty') in
      Abs (lst', substitute name ty ty')
    (* Regular substitution *)
    else Abs (lst, substitute name ty ty')
  | App (f, a) ->
    App (substitute name ty f, List.map (substitute name ty) a)
  | Arrow (a, r) ->
    Arrow (List.map (substitute name ty) a, substitute name ty r)
  | Var ({ contents = Link ty' } as var) ->
    var := Link (substitute name ty ty');
    Var var
  | Forall (lst, ty') -> Forall (lst, substitute name ty ty')
  | Alias (s, ty') -> Alias (s, substitute name ty ty')
  | Set l -> Set (Set.map (substitute name ty) l)
  | ty -> ty

let rec is_abstraction = function
  | Abs _ -> true
  | Alias (_, t) -> is_abstraction t
  | Var { contents = Link t } -> is_abstraction t
  | _ -> false

let rec is_redex = function
  | App (t, _) -> is_abstraction t
  | Alias (_, t) -> is_redex t
  | Var { contents = Link t } -> is_redex t
  | _ -> false

exception Mismatch_arguments of t

let () = Printexc.register_printer
  (function
    | Mismatch_arguments ty ->
      Some (Printf.sprintf "mismatch argument(s) in application of %s"
            (to_string ty))
    | _ -> None)

let rec reduction = function
  | App (ty, args) when is_abstraction ty ->
    begin
      (** TODO: lost Alias (bug of pretty-print) and is not exhaustive
                pattern-matching *)
      let Abs (ids, ty) = unlink ty in
      try let lst = List.combine ids args in
          List.fold_right (fun (x, u) -> substitute x u) lst ty
      with Invalid_argument "List.combine" ->
           raise (Mismatch_arguments (Abs (ids, ty)))
    end
  | ty -> ty

let normalize t =
  let rec step t =
    if is_redex t then reduction t
    else match t with
      | Abs (lst, t) -> Abs (lst, step t)
      | App (f, a) ->
        let f' = step f in
        if f' = f then App (f, List.map step a)
        else App (f', a)
      | Arrow (a, r) -> Arrow (List.map step a, step r)
      | Var ({ contents = Link t } as var) ->
        var := Link (step t);
        Var var
      | Forall (lst, t) -> Forall (lst, step t)
      | Alias (s, t) -> Alias (s, step t)
      | Set l -> Set (Set.map step l)
      | t -> t
  in
  let t' = step t in
  let t' = if t' = t then t else step t' in
  t'
