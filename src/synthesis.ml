module Environment = struct
  include Map.Make (String)

  let extend env name ty = add name ty env
  let lookup env name = find name env
end

module Definition = struct
  include Map.Make (String)
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
exception Mismatch_arguments of Type.t
exception Expected_function of Type.t
exception Unbound_variable of string
exception Variable_no_instantiated
exception Polymorphic_argument_inferred of Type.t list
exception No_instance of (Type.t * Type.t)
exception Unknown_type of string
exception Error of (Location.t * exn)

let rec string_of_exn = function
  | Recursive_type ty ->
    ("recursive type in " ^ (Type.to_string ty))
  | Conflict (a, b) ->
    ("conflict between "
     ^ (Type.to_string a) ^ " and "
     ^ (Type.to_string b))
  | Circularity (a, b) ->
    ("circularity between "
     ^ (Type.to_string a) ^ " and "
     ^ (Type.to_string b))
  | Mismatch_arguments t ->
    ("mismatch argument(s) on " ^ (Type.to_string t))
  | Expected_function ty ->
    ("expected function in " ^ (Type.to_string ty))
  | Unbound_variable name ->
    ("unbound variable " ^ name)
  | Variable_no_instantiated -> "variable no instantiated"
  | Polymorphic_argument_inferred lst ->
    "polymorphic argument inferred in "
    ^ (String.concat " or " (List.map Type.to_string lst))
  | No_instance (a, b) ->
    (Type.to_string a) ^ " is not an instance of " ^ (Type.to_string b)
  | Unknown_type name ->
    ("unknown type " ^ name)
  | Error (loc, exn) ->
    Printf.sprintf "%s at %s" (string_of_exn exn) (Location.to_string loc)
  | exn ->
    raise (Invalid_argument ("Synthesis.string_of_exn: "
                             ^ (Printexc.to_string exn)))

(** compute_variable : makes sure that the type variable being
  * unified doesn't occur within the it is being unified with.
  *
  * Each variable has a "level", which indicates how definition has been
  * created. The higher the level, the higher the variable has been introduced
  * recently. When assigning a variable V by a type T, we must preserve this
  * information. In particular, if the type T contains variables that higher
  * level, it is necessary to lower the level of these variables at V.
  * Everything must happen as if, instead of having introduced a variable to a
  * certain date and then determined its value by unification, we had guessed
  * the correct value at the Introduction of the variable.
  *
  * @param id id of `Unbound` type variable to prevent circularity
  * @param level level of `Unbound` type
  * @param ty type with wich we will unify
  *
  * @raise Recursive_type if [id] is found is [ty]
*)

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

(** substitution : takes a list of `Bound` variables [ids], a list of
 * replacement types [tys] and type a type [ty]. Returns a new type [ty]
 * with `Bound` variables substitued with respective replacement types.
 *
 * @param ids list of id of `Bound` variables
 * @param tys list of type for replace with respective id
 * @param ty type to apply modification
*)

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

(** union : takes a list of `Generic` type variables and types [ty1] and type
 * [ty2] and checks if any of the `Generic` type variables appears in any of
 * the sets of free generic variables in [ty1] or [ty2].
 *
 * See [catch_generic_variable] for capturing free generic variables.
 *
 * @param lst list of `Generic` type variables
 * @param ty1
 * @param ty2
 *
 * @return true if a `Generic` type variables appears in [ty1] or [ty2]
*)

let union lst ty1 ty2 =
  let set1 = catch_generic_variable ty1 in
  let set2 = catch_generic_variable ty2 in
  List.exists
    (fun var -> Set.mem set1 var || Set.mem set2 var) lst

(** normalize : normalizes type by replacing its alias with new bodies to which
 * they are linked. The function returns the standard type and a [bool]
 * notifier if an alias was expanded.
 *
 * @param def all definitions
 * @param ty type normalize
*)

let rec normalize ~def = function
  | Type.Const name ->
    begin
      try (true, Definition.find name def |> Type.copy)
      with Not_found -> (false, Type.Const name)
    end
  | Type.App (f, a) ->
    let (s, f) = normalize ~def f in
    let lst = List.map (normalize ~def) a in
    (s || List.fold_left (fun a x -> a || fst x) false lst,
     Type.App (f, List.map snd lst))
  | Type.Arrow (a, r) ->
    let lst = List.map (normalize ~def) a in
    let (s, r) = normalize ~def r in
    (List.fold_left (fun a x -> a || fst x) false lst || s,
     Type.Arrow (List.map snd lst, r))
  | Type.Var ({ contents = Type.Link t } as var) ->
    let (s, v) = normalize ~def t in
    var := Type.Link v; (s, Type.Var var)
  | Type.Forall (lst, ty) ->
    let (s, ty) = normalize ~def ty in
    (s, Type.Forall (lst, ty))
  | ty -> (false, ty)

(** unification : takes two types and tries to them, i.e. determine if they can
 * be equal. Type constants unify with identical type contents, and arrow types
 * and other structured types are unified by unifying each of their components.
 * After first performing an "occurs check" (see [compute_variable]), unbound
 * type variables can be unified with any type by replacing their reference with
 * a link pointing to the other type T.
 *
 * If type T pointed is known, it should not reduce the variable type T since
 * it can be shared by other variables and we could break links.
 *
 * @param def all definitions
 * @param t1
 * @param t2
 *
 * @raise Conflict if can not unify t1 and t2 (ex: unify int bool)
 * @raise Circularity if [ty1] is dependent with [ty2]
 * @raise Variable_no_instantiated if found `Bound` type variable. Indeed, it's
 * normally impossible after a substitution by `Forall` normalized expression.
*)

let rec unification ?(def = Definition.empty) t1 t2 =
  if t1 == t2 then ()
  else match t1, t2 with
    | Type.Const n1, Type.Const n2 when n1 = n2 -> ()
    | Type.App (t1, a1), Type.App (t2, a2) ->
      unification ~def t1 t2;
      begin
        try List.iter2 (unification ~def) a1 a2
        with Invalid_argument "List.iter2" -> raise (Conflict (t1, t2))
      end
    | Type.Arrow (a1, r1), Type.Arrow (a2, r2) ->
      begin
        try List.iter2 (unification ~def) a1 a2
        with Invalid_argument "List.iter2" -> raise (Conflict (t1, t2))
      end;
      (unification ~def) r1 r2
    | Type.Var { contents = Type.Link t1 }, t2
    | t1, Type.Var { contents = Type.Link t2 } ->
      (unification ~def) t1 t2
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

    (** First, we create a fresh `Generic` type variable for every type
     * variable bound by the two polymorphic types. Here, we rely on the fact
     * that both types are normalized (see [normalize]), so equivalent generic
     * type variables should appear in the same locations in both types.
     *
     * Then, we substitute all `Bound` type variables in both types with
     * `Generic` type variables, an try to unify them.
     *
     * If unification success, we check that none of the `Generic` type
     * variables escapes, otherwise, we would successfilly unify types
     * `(forall (a) (a -> a))` and `(forall (a) (a -> b))`, where `b` is a
     * unifiable `Unbound` type variable.
    *)

    | (Type.Forall (ids1, ty1) as forall1),
      (Type.Forall (ids2, ty2) as forall2) ->
      let lst =
        try List.rev_map2 (fun _ _ -> Variable.generic ()) ids1 ids2
        with Invalid_argument "List.rev_map2" -> raise (Conflict (ty1, ty2))
      in
      let ty1 = substitution ids1 lst (normalize ~def ty1 |> snd) in
      let ty2 = substitution ids2 lst (normalize ~def ty2 |> snd) in
      (unification ~def) ty1 ty2;
      if union lst forall1 forall2
      then raise (Conflict (forall1, forall2))

    (** Sometimes we try to unify a standardized type with an Alias. In this
     * case, we try to normalize the two types and if this treatment notify
     * us expand an alias, it restarts the unification with standardized
     * types.
    *)

    | ty1, ty2 ->
      let (s1, ty1) = normalize ~def ty1 in
      let (s2, ty2) = normalize ~def ty2 in
      if s1 || s2
      then unification ~def ty1 ty2
      else raise (Conflict (t1, t2))

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

(** specialization : instantiates a `Forall` type by substituting bound type
 * variables by fresh `Unbound` type variables, wich can then by unified with
 * any other type.
 *
 * @param level level for create `Unbound` type variable
 * @param ty type to apply modification
*)

let specialization level ty =
  let rec aux level = function
    | Type.Forall (ids, ty) ->
      let lst = List.rev_map (fun _ -> Variable.make level) ids in
      substitution ids lst ty
    | Type.Var { contents = Type.Link ty } -> aux level ty
    | ty -> ty
  in aux level ty

(** specialization_annotation : same as specialization but for annotation *)

let specialization_annotation level (lst, ty) =
  let aux = function
    | [], ty -> [], ty
    | ids, ty ->
      let lst = List.rev_map (fun _ -> Variable.make level) ids in
      lst, substitution ids lst ty
  in aux (lst, ty)

(** generalization : transforms a type into a `Forall` type by substituting all
 * `Unbound` type variables, with levels higher then the [level] with `Bound`
 * type variables. The traverse order is same as Parse.replace.
 *
 * @param level date of declaration
 * @param ty type to apply modification
*)

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
    then raise (Mismatch_arguments ty)
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

(** subsume : takes two types [ty1] [ty2] and determines if [ty1] is an of
 * [ty2]. For example, `(int -> int)` is an instance of
 * `(forall (a) (a -> a))` (the type of `id`) which in turn is an instance
 * of `(foralle (a b) (a -> b))` (type of `magic`). This means that we can
 * pass `id` as an argument to a function expecting `(int -> int)` and we can
 * pass `magic` to a function expecing `(forall (a) (a -> a))` but not the
 * other way round. To determine if [ty1] is an instance of [ty2], [subsume]
 * first instantiates [ty2], the more general type, with `Unbound` type
 * varibales. If [ty1] is not polymorphic, is simply unifies the two types.
 * Otherwise, it instantiates [ty1] with `Generic` type variables and uunifies
 * both instantiated types. If unification success, we check that no generic
 * variables escapes (see [union]).
 *
 * @param level level for make specializationon [ty2]
 * @param ty1
 * @param ty2
 *
 * @raise No_instance if [ty1] is not an instance of [ty2]
*)

let subsume ~def ~level ty1 ty2 =
  let ty2' = specialization level ty2 in
  match Type.unlink ty1 with
  | Type.Forall (ids, ty1) as forall ->
    let lst = List.rev_map (fun _ -> Variable.generic ()) ids in
    let ty1' = substitution ids lst ty1 in
    unification ~def ty1' ty2';
    if union lst forall ty2
    then raise (No_instance (ty1, ty2))
  | ty1 -> unification ~def ty1 ty2'

let ( >!= ) func handle_error =
  try func () with
  | exn -> handle_error exn

let raise_with_loc loc = function
  | Error (loc, exn) as error -> raise error
  | exn -> raise (Error (loc, exn))

let rec eval
    ?(def = Definition.empty)
    ?(env = Environment.empty)
    ?(level = 0) = function
  | Ast.Var (loc, name) ->
    (fun () ->
       try Environment.lookup env name
       with Not_found -> raise (Unbound_variable name))
    >!= raise_with_loc loc
  | Ast.Abs (loc, a, c) ->

    (** To infer the type of functions, we first extend the environment with the
     * types of the parameters, which might be annoted. We remember all new type
     * variables that appear in parameter types in [lstvar] so that we can later
     * make sure that none of them was unified with polymorphic type.
     *
     * We then infer the type of the function body using the extended
     * environment, and instantiate it unless it's annotated.
     *
     * Finally, we generalize the resulting function type.
     *
     * @raise Polymorphic_argument_inferred if a parameter has been unified
     * with polymorphic type
    *)

    (fun () ->
       let refenv = ref env in
       let lstvar = ref [] in
       let a' = List.map (fun (name, ann) ->
           let (ty_normalized, ty) = match ann with
             | None ->
               let var = Variable.make (level + 1) in
               lstvar := var :: !lstvar;
               (None, var)
             | Some (lst, ty) ->
               let _, ty_normalized = normalize ~def ty in
               let vars, ty_normalized = specialization_annotation
                   (level + 1)
                   (lst, ty_normalized) in
               lstvar := vars @ !lstvar;
               (Some ty_normalized, ty)
           in refenv := begin
             match ty_normalized with
             | None -> Environment.extend !refenv name ty
             | Some ty_normalized ->
               Environment.extend !refenv name ty_normalized
           end; ty) a
       in
       let r' = eval ~def ~env:!refenv ~level:(level + 1) c in
       let r' =
         if Ast.is_annotated c then r'
         else specialization (level + 1) r' in
       if not (List.for_all Type.is_monomorphic !lstvar)
       then raise (Polymorphic_argument_inferred !lstvar)
       else generalization level (Type.Arrow (a', r')))
    >!= raise_with_loc loc
  | Ast.App (loc, f, a) ->

    (** To infer the type of function application we first infer the type of the
     * function being called, instantiate it and separate parameter types from
     * function return type.
     *
     * The core of the algorithm is infering argument types in the function
     * [compute_argument].
    *)

    (fun () ->
       let f' = eval ~def ~env ~level:(level + 1) f in
       let f' = normalize ~def f' |> snd in
       let f' = specialization (level + 1) f' in
       let a', r' = compute_function (List.length a) f' in
       compute_argument def env (level + 1) a' a;
       generalization level (specialization (level + 1) r'))
    >!= raise_with_loc loc
  | Ast.Let (loc, n, e, c) ->
    (fun () ->
       let e' = eval ~def ~env ~level:(level + 1) e in
       eval ~def ~env:(Environment.extend env n e') ~level c)
    >!= raise_with_loc loc
  | Ast.Alias (loc, n, e, c) ->
    (fun () -> eval ~def:(Definition.add n e def) ~env ~level c)
    >!= raise_with_loc loc
  | Ast.Rec (loc, n, e, c) ->
    (fun () ->
       let n' = Variable.make (level + 1) in
       let ext = Environment.extend env n n' in
       let e' = eval ~def ~env:ext ~level:(level + 2) e in
       unification ~def n' e';
       eval
         ~def
         ~env:(Environment.extend env n (generalization level e'))
         ~level c)
    >!= raise_with_loc loc
  | Ast.Ann (loc, e, (lst, ty)) ->

    (** Infering type annotation `expr : type` is equivalent to inferring the
     * type of function call `((lambda (x : type) x) expr)`, but optimized in
     * this implementation of [eval].
    *)

    (fun () ->
       let _, ty_normalized = normalize ~def ty in
       let _, ty_normalized =
         specialization_annotation level (lst, ty_normalized) in
       let e' = eval ~def ~env ~level e in
       subsume ~def ~level ty_normalized e';
       ty)
    >!= raise_with_loc loc
  | Ast.If (loc, i, a, b) ->
    (fun () ->
       let i' = eval ~def ~env ~level i in
       let a' = eval ~def ~env ~level a in
       let b' = eval ~def ~env ~level b in
       unification ~def i' (Type.Const "bool");
       unification ~def a' b';
       a')
    >!= raise_with_loc loc
  | Ast.Seq (loc, a, b) ->
    (fun () ->
       let a' = eval ~def ~env ~level a in
       let b' = eval ~def ~env ~level b in
       unification ~def a' (Type.Const "unit");
       b')
    >!= raise_with_loc loc
  | Ast.Int _ -> Type.Const "int"
  | Ast.Bool _ -> Type.Const "bool"
  | Ast.Char _ -> Type.Const "char"
  | Ast.Unit _ -> Type.Const "unit"

(** compute_argument : after infering the type of argument, we use the function
 * [subsume] (or [unification] if the argument is annotated) to determine if the
 * parameter type is an instance of the type of the argument.
 *
 * When calling functions with multiple arguments, we must first [subsume] the
 * types of arguments for those parameters that are type variables, otherwise we
 * would fail to typecheck applications such as `(revapply id poly), where
 * `revapply : (forall (a b) (a -> (a -> b) -> b))`,
 * `poly : ((forall (a) (a -> a ->)) -> (pair int bool))` and
 * `id` : (forall (a) (a -> a)).
 *
 * @param def all definitions
 * @param env environment
 * @param level level for [subsume] and [eval]
 * @param tys list of types of arguments
 * @param a list of arguments
*)

and compute_argument def env level tys a =
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
       let ty' = eval ~def ~env ~level a in
       if Ast.is_annotated a
       then unification ~def ty ty'
       else subsume ~def ~level ty ty')
    slst
