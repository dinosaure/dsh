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
end = struct
  let id = ref 0

  let next () =
    let i = !id in
    incr id; i

  let reset () =
    id := 0

  let make level = Type.Var (ref (Type.Unbound (next (), level)))
  let generic () = Type.Var (ref (Type.Generic (next ())))
end

exception Recursive_type of Type.t
exception Conflict of (Type.t * Type.t)
exception Circularity of (Type.t * Type.t)
exception Expected_argument of (Type.t * int)
exception Expected_function of Type.t
exception Unbound_variable of string

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
  | exn ->
    raise (Invalid_argument ("W.string_of_exn: " ^ (Printexc.to_string exn)))


let compute_variable id level ty =
  let rec aux = function
    | Type.Var { contents = Type.Link ty } -> aux ty
    | Type.Var { contents = Type.Generic _ } -> assert false
    | Type.Var ({ contents = Type.Unbound (id', level') } as var) ->
      if id' = id then raise (Recursive_type ty)
      else if level' > level then var := Type.Unbound (id', level)
      else ()
    | Type.App (t, a) ->
      aux t; List.iter aux a
    | Type.Arrow (a, r) ->
      List.iter aux a; aux r
    | Type.Const _ -> ()
  in aux ty

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
    | Type.Var { contents = Type.Unbound (id1, _ ) },
      Type.Var { contents = Type.Unbound (id2, _) } when id1 = id2 ->
      raise (Circularity (t1 ,t2))
    | Type.Var ({ contents = Type.Unbound (id, level) } as var), ty
    | ty, Type.Var ({ contents = Type.Unbound (id, level) } as var) ->
      compute_variable id level ty;
      var := Type.Link ty
    | _, _ -> raise (Conflict (t1, t2))

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

let rec compute_function n = function
  | Type.Arrow (a, r) as ty ->
    if List.length a <> n
    then raise (Expected_argument (ty, n))
    else a, r
  | Type.Var { contents = Type.Link ty } ->
    compute_function n ty
  | Type.Var ({ contents = Type.Unbound (id, level) } as var) ->
    let lst =
      let rec aux = function
        | 0 -> []
        | n -> Variable.make level :: aux (n - 1)
      in aux n
    in let r = Variable.make level in
    var := Type.Link (Type.Arrow (lst, r));
    lst, r
  | _ as ty -> raise (Expected_function ty)

let rec eval env level = function
  | Ast.Var name ->
    begin
      try specialization level (Environment.lookup env name)
      with Not_found -> raise (Unbound_variable name)
    end
  | Ast.Abs (a, c) ->
    let a' = List.map (fun _ -> Variable.make level) a in
    let env' =
      List.fold_left2
        (fun env name ty -> Environment.extend env name ty)
        env a a' in
    let c' = eval env' level c in
    Type.Arrow (a', c')
  | Ast.App (f, a) ->
    let a', r' = compute_function (List.length a) (eval env level f) in
    List.iter2 (fun ty a -> unification ty (eval env level a)) a' a;
    r'
  | Ast.Let (n, e, c) ->
    let e' = eval env (level + 1) e in
    let ty = generalization level e' in
    eval (Environment.extend env n ty) level c

