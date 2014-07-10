module Environment = struct
  include Map.Make (String)

  let extend env name ty = add name ty env
  let lookup env name = find name env
end

module Variable : sig
  val next : unit -> int
  val reset : unit -> unit

  val make : int -> Type.t
  val generic : unit -> Type.t
  val bound : unit -> (int * Type.t)
end = struct
  let id = ref 0

  let next () =
    let i = !id in
    incr id; i

  let reset () =
    id := 0

  let make level = Type.Var (ref (Type.Unbound (next (), level)))
  let generic () = Type.Var (ref (Type.Generic (next ())))
  let bound () = let id = next () in (id, Type.Var (ref (Type.Bound id)))
end

module Map = struct
  include Map.Make (struct type t = int let compare = compare end)

  let removes map ids =
    List.fold_left
      (fun acc x -> remove x acc)
      map ids

  let of_lists keys values =
    List.fold_left2
      (fun acc key value -> add key value acc)
      empty keys values
end

module Set = struct
  type 'a t = ('a, unit) Hashtbl.t

  let create size : 'a t = Hashtbl.create size
  let add set data = Hashtbl.replace set data ()
  let mem set data = Hashtbl.mem set data
end

exception Recursive_type of Type.t
exception Conflict of (Type.t * Type.t)
exception Circularity of (Type.t * Type.t)
exception Expected_argument of (Type.t * int)
exception Expected_function of Type.t
exception Unbound_variable of string
exception Variable_no_instantiated
exception Polymorphic_parameter_inferred of Type.t list
exception Is_not_instance of (Type.t * Type.t)

let string_of_exn = function
  | Recursive_type ty ->
    ("recursive type: " ^ (Type.to_string ty))
  | Conflict (a, b) ->
    ("conflict between "
     ^ (Type.to_string a) ^ " and "
     ^ (Type.to_string b))
  | Circularity (a, b) ->
    ("circularity between "
     ^ (Type.to_string a) ^ " and "
     ^ (Type.to_string b))
  | Expected_argument (f, n) ->
    ("expected " ^ (string_of_int n) ^ " argument(s) in " ^ (Type.to_string f))
  | Expected_function ty ->
    ("expected function: " ^ (Type.to_string ty))
  | Unbound_variable name ->
    ("unbound variable: " ^ name)
  | Variable_no_instantiated -> "variable no instantiated"
  | Polymorphic_parameter_inferred lst ->
    "polymorphic parameter inferred: "
    ^ (String.concat ", " (List.map Type.to_string lst))
  | Is_not_instance (a, b) ->
    (Type.to_string a) ^ " is not instance of " ^ (Type.to_string b)
  | exn ->
    raise (Invalid_argument ("Synthesis.string_of_exn: "
                             ^ (Printexc.to_string exn)))

let compute_variable id level ty =
  let rec aux = function
    | Type.Var { contents = Type.Link ty } -> aux ty
    | Type.Var { contents = Type.Generic _ }
    | Type.Var { contents = Type.Bound _ } -> ()
    | Type.Var ({ contents = Type.Unbound (id', level') } as var) ->
      if id' = id then raise (Recursive_type ty)
      else if level' > level then var := Type.Unbound (id', level)
      else ()
    | Type.App (t, a) ->
      aux t; List.iter aux a
    | Type.Arrow (a, r) ->
      List.iter aux a; aux r
    | Type.Forall (_, ty) -> aux ty
    | Type.Const _ -> ()
  in aux ty

let substitution ids tys ty =
  let rec aux map = function
    | Type.Const _ as ty -> ty
    | Type.Var { contents = Type.Link ty } -> aux map ty
    | Type.Var { contents = Type.Bound id } as ty ->
      begin try Map.find id map with Not_found -> ty end
    | Type.Var _ as ty -> ty
    | Type.App (f, a) ->
      Type.App (aux map f, List.map (aux map) a)
    | Type.Arrow (a, r) ->
      Type.Arrow (List.map (aux map) a, aux map r)
    | Type.Forall (ids, ty) ->
      Type.Forall (ids, aux (Map.removes map ids) ty)
  in aux (Map.of_lists ids tys) ty

let catch_generic_variable ty =
  let set = Set.create 16 in
  let rec aux = function
    | Type.Const _ -> ()
    | Type.Var { contents = Type.Link ty } -> aux ty
    | Type.Var { contents = Type.Bound _ } -> ()
    | Type.Var { contents = Type.Generic _ } as ty ->
      Set.add set ty
    | Type.Var { contents = Type.Unbound _ } -> ()
    | Type.App (f, a) ->
      aux f; List.iter aux a
    | Type.Arrow (a, r) ->
      List.iter aux a; aux r
    | Type.Forall (_, ty) -> aux ty
  in aux ty; set

let union lst ty1 ty2 =
  let set1 = catch_generic_variable ty1 in
  let set2 = catch_generic_variable ty2 in
  List.exists
    (fun var -> Set.mem set1 var || Set.mem set2 var) lst

let rec unification t1 t2 =
  if t1 == t2 then ()
  else match t1, t2 with
    | Type.Const n1, Type.Const n2 when n1 = n2 -> ()
    | Type.App (t1, a1), Type.App (t2, a2) ->
      unification t1 t2;
      begin
        try List.iter2 unification a1 a2
        with Invalid_argument "List.iter2" -> raise (Conflict (t1, t2))
      end
    | Type.Arrow (a1, r1), Type.Arrow (a2, r2) ->
      begin
        try List.iter2 unification a1 a2
        with Invalid_argument "List.iter2" -> raise (Conflict (t1, t2))
      end;
      unification r1 r2
    | Type.Var { contents = Type.Link t1 }, t2
    | t1, Type.Var { contents = Type.Link t2 } ->
      unification t1 t2
    | Type.Var { contents = Type.Generic id1 },
      Type.Var { contents = Type.Generic id2 }
    | Type.Var { contents = Type.Unbound (id1, _ ) },
      Type.Var { contents = Type.Unbound (id2, _) } when id1 = id2 ->
      raise (Circularity (t1 ,t2))
    | Type.Var { contents = Type.Bound _ }, _
    | _, Type.Var { contents = Type.Bound _ } ->
      raise Variable_no_instantiated
    | Type.Var ({ contents = Type.Unbound (id, level) } as var), ty
    | ty, Type.Var ({ contents = Type.Unbound (id, level) } as var) ->
      compute_variable id level ty;
      var := Type.Link ty
    | (Type.Forall (ids1, ty1) as forall1),
      (Type.Forall (ids2, ty2) as forall2) ->
      let lst =
        try List.rev_map2 (fun _ _ -> Variable.generic ()) ids1 ids2
        with Invalid_argument "List.rev_map2" -> raise (Conflict (ty1, ty2))
      in
      let ty1 = substitution ids1 lst ty1 in
      let ty2 = substitution ids2 lst ty2 in
      unification ty1 ty2;
      if union lst forall1 forall2
      then raise (Conflict (forall1, forall2))
    | _, _ -> raise (Conflict (t1, t2))

(*
let rec generalization level = function
  | Type.Var { contents = Type.Unbound (id, level') } when level' > level ->
    Type.Var (ref (Type.Generic id))
  | Type.App (f, a) ->
    Type.App (generalization level f, List.map (generalization level) a)
  | Type.Arrow (a, r) ->
    Type.Arrow (List.map (generalization level) a, generalization level r)
  | Type.Var { contents = Type.Link ty } -> generalization level ty
  | Type.Var { contents = Type.Generic _ }
  | Type.Var { contents = Type.Unbound _ }
  | Type.Const _ as ty -> ty
*)

(*
let specialization level ty =
  let map = Hashtbl.create 10 in
  let rec aux = function
    | Type.Const _ as ty -> ty
    | Type.Var { contents = Type.Link ty } -> aux ty
    | Type.Var { contents = Type.Generic id } ->
      begin
        try Hashtbl.find map id
        with Not_found ->
          let var = Variable.make level in
          Hashtbl.add map id var; var
      end
    | Type.Var { contents = Type.Unbound _ } as ty -> ty
    | Type.App (f, a) ->
      Type.App (aux f, List.map aux a)
    | Type.Arrow (a, r) ->
      Type.Arrow (List.map aux a, aux r)
  in aux ty
*)

let rec specialization level = function
  | Type.Forall (ids, ty) ->
    let lst = List.rev_map (fun _ -> Variable.make level) ids in
    substitution ids lst ty
  | Type.Var { contents = Type.Link ty } -> specialization level ty
  | ty -> ty

let specialization_annotation level = function
  | [], ty -> [], ty
  | ids, ty ->
    let lst = List.rev_map (fun _ -> Variable.make level) ids in
    lst, substitution ids lst ty

let generalization level ty =
  let acc = ref [] in
  let rec aux = function
    | Type.Var { contents = Type.Link ty } -> aux ty
    | Type.Var { contents = Type.Generic _ } -> assert false
    | Type.Var { contents = Type.Bound _ } -> ()
    | Type.Var ({ contents = Type.Unbound (id, level')} as var)
      when level' > level ->
      var := Type.Bound id;
      if not (List.mem id !acc)
      then acc := id :: !acc
    | Type.Var { contents = Type.Unbound _ } -> ()
    | Type.App (f, a) -> aux f; List.iter aux a
    | Type.Arrow (a, r) -> List.iter aux a; aux r
    | Type.Forall (_, ty) -> aux ty
    | Type.Const _ -> ()
  in aux ty; match !acc with
  | [] -> ty
  | ids -> Type.Forall (List.rev ids, ty)

let rec compute_function n = function
  | Type.Arrow (a, r) as ty ->
    if List.length a <> n
    then raise (Expected_argument (ty, n))
    else a, r
  | Type.Var { contents = Type.Link ty } ->
    compute_function n ty
  | Type.Var ({ contents = Type.Unbound (id, level) } as var) ->
    let lst =
      let rec aux acc = function
        | 0 -> acc
        | n -> aux (Variable.make level :: acc) (n - 1)
      in aux [] n
    in let r = Variable.make level in
    var := Type.Link (Type.Arrow (lst, r));
    lst, r
  | _ as ty -> raise (Expected_function ty)

let subsume level ty1 ty2 =
  let ty2' = specialization level ty2 in
  match Type.unlink ty1 with
  | Type.Forall (ids, ty1) as forall ->
    let lst = List.rev_map (fun _ -> Variable.generic ()) ids in
    let ty1' = substitution ids lst ty1 in
    unification ty1' ty2';
    if union lst forall ty2
    then raise (Is_not_instance (ty2, ty1))
  | ty1 -> unification ty1 ty2'

let rec eval env level = function
  | Ast.Var (_, name) ->
    begin
      try Environment.lookup env name
      with Not_found -> raise (Unbound_variable name)
    end
  | Ast.Abs (_, a, c) ->
    let refenv = ref env in
    let lstvar = ref [] in
    let a' = List.map (fun (name, ann) ->
        let ty = match ann with
          | None ->
            let var = Variable.make (level + 1) in
            lstvar := var :: !lstvar;
            var
          | Some ann ->
            let vars, ty = specialization_annotation (level + 1) ann in
            lstvar := vars @ !lstvar;
            ty
        in refenv := Environment.extend !refenv name ty; ty) a
    in
    let r' = eval !refenv (level + 1) c in
    let r' = if Ast.is_annotated c then r' else specialization (level + 1) r' in
    if not (List.for_all Type.is_monomorphic !lstvar)
    then raise (Polymorphic_parameter_inferred !lstvar)
    else generalization level (Type.Arrow (a', r'))
  | Ast.App (_, f, a) ->
    let f' = specialization (level + 1) (eval env (level + 1) f) in
    let a', r' = compute_function (List.length a) f' in
    compute_argument env (level + 1) a' a;
    generalization level (specialization (level + 1) r')
  | Ast.Let (_, n, e, c) ->
    let e' = eval env (level + 1) e in
    eval (Environment.extend env n e') level c
  | Ast.Ann (_, e, ann) ->
    let _, ty = specialization_annotation level ann in
    let e' = eval env level e in
    subsume level ty e';
    ty

and compute_argument env level tys a =
  let plst = List.combine tys a in
  let get_ordering ty arg =
    match Type.unlink ty with
    | Type.Var { contents = Type.Unbound _ } -> 1
    | _ -> 0
  in
  let slst = List.fast_sort
      (fun (ty1, a1) (ty2, a2) ->
         compare (get_ordering ty1 a1) (get_ordering ty2 a2)) plst
  in
  List.iter
    (fun (ty, a) ->
       let ty' = eval env level a in
       if Ast.is_annotated a
       then unification ty ty'
       else subsume level ty ty')
    slst
