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

  let add key value map =
    assert (not (mem key map));
    add key value map

  let add_list set lst =
    List.fold_left
      (fun acc (key, value) -> add key value acc)
      set lst

  let of_list lst = add_list empty lst

  let to_list set =
    fold (fun key value acc -> (key, value) :: acc) set []

  let iter2 func set1 set2 =
    try List.iter2 func (to_list set1) (to_list set2)
    with Invalid_argument _ -> raise (Invalid_argument ("Type.Set.iter2"))

  let merge m1 m2 =
    merge
      (fun _ l1 l2 -> match l1, l2 with
       | None, None -> assert false
       | Some l, None
       | None, (Some l) -> Some l
       | Some l1, Some l2 -> Some (l1 @ l2))
    m1 m2
end

type t =
  | Const of string                       (* like `int` *)
  | App of (t * t list)                   (* like `list[int]` *)
  | Arrow of (t list * t)                 (* like `int → int` *)
  | Var of var ref                        (* variable of type *)
  | Forall of (int list * t)              (* like `∀l.t` *)
  | Alias of (string * t)                 (* alias of type *)
  | Variant of row                        (* like `[ A | B ]` *)
  | Record of row                         (* like `{ a; b }` *)
  | RowEmpty                              (* {} *)
  | RowExtend of ((t list) Set.t * row)
  | Abs of (string list * t)              (* like `λl.t` *)
and var =
  | Unbound of int * int
  | Bound of int
  | Link of t
  | Generic of int
and row = t

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

let int = Const "int"
let char = Const "char"
let bool = Const "bool"
let unit = Const "unit"
let tuple l = App (Const "tuple", l)

module Buffer = struct
  include Buffer

  let add_list ?(sep="") add_data buffer lst =
    let rec aux = function
      | [] -> ()
      | [ x ] -> add_data buffer x
      | x :: r ->
        Printf.bprintf buffer "%a%s" add_data x sep; aux r
    in aux lst

  let add_set ?(sep="") add_data buffer set =
    add_list ~sep add_data buffer (Set.bindings set)
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

let rec compact = function
  | RowExtend (map, rest) ->
    begin
      match compact rest with
      | (rest_map, rest) when Set.is_empty rest_map ->
        (map, rest)
      | (rest_map, rest) ->
        (Set.merge map rest_map, rest)
    end
  | Var { contents = Link t } -> compact t
  | Var _ as var -> (Set.empty, var)
  | RowEmpty -> (Set.empty, RowEmpty)
  | ty -> raise (Invalid_argument "Type.compact")

let is_close ty =
  let rec deep_check = function
    | Var ({ contents = Link ty }) -> deep_check ty
    | Alias (_, ty) -> deep_check ty
    | RowEmpty -> true
    | _ -> false
  in match ty with
  | Variant t -> let (_, rest) = compact t in deep_check rest
  | Record t -> let (_, rest) = compact t in deep_check rest
  | _ -> false

let rec unlink = function
  | Var ({ contents = Link ty } as var) ->
    let ty = unlink ty in
    var := Link ty; ty
  | Alias (_, ty) -> unlink ty (* an Alias is a link to a type *)
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
  | Alias (_, t) -> is_monomorphic t
  | Abs _ -> false
  | Variant t -> is_monomorphic t
  | Record t -> is_monomorphic t
  | RowEmpty -> false
  | RowExtend (map, rest) ->
    Set.exists (fun key -> List.exists is_monomorphic) map
    && is_monomorphic rest

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
  let rec atom env buffer = function
    | Const name ->
      Buffer.add_string buffer name
    | Alias (name, ty) ->
      Buffer.add_string buffer name
    | App (f, a) ->
      Printf.bprintf buffer "%a[%a]"
        (atom env) f
        (Buffer.add_list ~sep:", " (expr env)) a
    | Abs (ids, ty) ->
      Printf.bprintf buffer "λ%a. %a"
        (Buffer.add_list ~sep:" " Buffer.add_string) ids
        (expr env) ty
    | Var { contents = Unbound (id, _) } ->
      Printf.bprintf buffer "%s" (name_of_unbound id)
    | Var { contents = Bound id } ->
      begin
        try Buffer.add_string buffer (Environment.find id env)
        with Not_found -> Printf.bprintf buffer "#%d" id
      end
    | Var { contents = Generic id } ->
      Printf.bprintf buffer "%s" (name_of_unbound id)
    | Var { contents = Link ty } -> atom env buffer ty
    | Record t ->
      Printf.bprintf buffer "{%a}" (expr env) t
    | Variant t ->
      Printf.bprintf buffer "[%a]" (expr env) t
    | RowEmpty -> ()
    | RowExtend _ as t ->
      let (map, rest) = compact t in
      let add_label label buffer ty =
        Printf.bprintf buffer "%s : %a" label (expr env) ty in
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
        match unlink rest with
        | RowEmpty -> add_map buffer map
        | RowExtend _ -> assert false
        | ty ->
          Printf.bprintf buffer "%a | %a"
            add_map map
            (expr env) ty
      end
    | ty -> Printf.bprintf buffer "(%a)" (expr env) ty
  and expr env buffer = function
    | Arrow (a, r) ->
      let func = if List.length a = 0 then expr env else atom env in
      Printf.bprintf buffer "%a → %a"
        (Buffer.add_list ~sep:" → " func) a
        (atom env) r
    | Forall (ids, ty) ->
      let lst, env = Environment.extend env ids in
      Printf.bprintf buffer "∀%a. %a"
        (Buffer.add_list ~sep:" " Buffer.add_string) lst
        (expr env) ty
    | Var { contents = Link ty } -> expr env buffer ty
    | ty -> atom env buffer ty
  in
  let buffer = Buffer.create 16 in
  expr env buffer ty; Buffer.contents buffer

let rec bound ty1 ty2 =
  let set = ref [] in
  let add data =
    if List.mem_assq data !set
    then ()
    else set := (data, ()) :: !set in
  let del data = set := List.remove_assq data !set in
  let rec collect = function
    | App (f, a) -> collect f; List.iter collect a
    | Arrow (a, r) -> List.iter collect a; collect r
    | Var { contents = Link a } -> collect a
    | Forall (_, ty) -> collect ty
    | Alias (_, ty) -> collect ty
    | Abs (_, ty) -> collect ty
    | RowExtend (map, Var ({ contents = Unbound _ } as var)) ->
      Set.iter (fun _ -> List.iter collect) map;
      add var
    | RowExtend (map, ty) ->
      Set.iter (fun _ -> List.iter collect) map;
      collect ty
    | Variant ty -> collect ty
    | Record ty -> collect ty
    | ty -> ()
  in
  let rec bound = function
    | App (f, a) -> bound f; List.iter bound a
    | Arrow (a, r) -> List.iter bound a; bound r
    | Var { contents = Link a } -> bound a
    | Forall (_, ty) -> bound ty
    | Alias (_, ty) -> bound ty
    | Abs (_, ty) -> bound ty
    | RowExtend (map, Var ({ contents = Unbound _ } as var))
      when List.mem_assq var !set ->
      Set.iter (fun _ -> List.iter bound) map;
      del var; var := Link RowEmpty
    | RowExtend (map, ty) ->
      Set.iter (fun _ -> List.iter bound) map;
      bound ty
    | Variant ty -> bound ty
    | Record ty -> bound ty
    | ty -> ()
  in
  collect ty1; bound ty2; (ty1, ty2)

let rec close_row = function
  | RowExtend (map, Var ({ contents = Unbound _ } as var)) ->
    var := Link RowEmpty
  | RowExtend (map, rest) -> close_row rest
  | Alias (_, ty) -> close_row ty
  | Var { contents = Link ty } -> close_row ty
  | _ -> ()

let rec copy = function
  | Const name -> Const name
  | App (f, a) -> App (copy f, List.map copy a)
  | Arrow (a, r) -> Arrow (List.map copy a, copy r)
  | Var { contents = Link a } -> Var { contents = Link (copy a) }
  | Var ref -> Var (BatRef.copy ref)
  | Forall (lst, ty) -> Forall (lst, copy ty)
  | Alias (s, t) -> Alias (s, copy t)
  | Abs (lst, ty) -> Abs (lst, copy ty)
  | RowEmpty -> RowEmpty
  | RowExtend (map, rest) ->
    RowExtend (Set.map (List.map copy) map, copy rest)
  | Variant t -> Variant (copy t)
  | Record t -> Record (copy t)

module S = BatSet.Make(String)
let of_list l = List.fold_right S.add l S.empty
let to_list s = S.fold (fun x a -> x :: a) s []

let free ty =
  let rec aux = function
    | Const name -> S.singleton name
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
    | Abs (lst, ty) -> S.diff (aux ty) (of_list lst)
    | RowEmpty -> S.empty
    | RowExtend (map, rest) ->
      Set.fold (fun _ -> S.union)
        (Set.map (fun l -> List.fold_left S.union S.empty (List.map aux l)) map)
        S.empty
      |> S.union (aux rest)
    | Variant t -> aux t
    | Record t -> aux t
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
  | RowExtend (map, rest) ->
    RowExtend (Set.map (List.map (substitute name ty)) map,
               substitute name ty rest)
  | Variant ty' -> substitute name ty ty'
  | Record ty' -> substitute name ty ty'
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
      unlink ty |> function Abs (ids, ty) ->
        (** TODO: lost Alias (bug in pretty-print) and is not exhaustive
                  pattern-matching *)
        begin
          try let lst = List.combine ids args in
            List.fold_right (fun (x, u) -> substitute x u) lst ty
          with Invalid_argument "List.combine" ->
             raise (Mismatch_arguments (Abs (ids, ty)))
        end
      | _ -> assert false (* or is_abstraction is false *)
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
      | RowExtend (map, rest) ->
        RowExtend (Set.map (List.map step) map, step rest)
      | Variant t -> Variant (step t)
      | Record t -> Record (step t)
      | t -> t
  in
  let t' = step t in
  let t' = if t' = t then t else step t' in
  t'
