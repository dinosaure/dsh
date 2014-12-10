module Environment = struct
  include Map.Make (String)

  let extend env name ty = add name ty env
  let lookup env name = find name env
end

module TPattern = struct
  type t =
    | Var of (string * Type.t option ref)
    | Any of Type.t option ref
    | Variant of (string * t * Type.t option ref)
    | Tuple of (t list * Type.t option ref)
    | Int of int
    | Char of char
    | Bool of bool
    | Unit

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

  let to_string a =
    let buffer = Buffer.create 16 in
    let rec compute buffer = function
      | Var (name, _) -> Buffer.add_string buffer name
      | Any _ -> Buffer.add_string buffer "_"
      | Variant (name, Unit, _) ->
        Printf.bprintf buffer "%s()" name
      | Variant (name, expr, _) ->
        Printf.bprintf buffer "%s(%a)"
          name
          compute expr
      | Tuple (_, l) ->
        raise (Failure "Not implemented")
      | Int i ->
        Printf.bprintf buffer "%d" i
      | Char c ->
        Printf.bprintf buffer "%c" c
      | Bool b ->
        Printf.bprintf buffer "%b" b
      | Unit -> Buffer.add_string buffer "ø"
    in compute buffer a; Buffer.contents buffer

  (** Attach the type with the pattern, after a [compute_pattern] this function
      should not raise an exception. *)
  let rec of_pattern ty pss = match Type.unlink ty, pss with
    | ty, Pattern.Var (_, name) -> Var (name, ref (Some ty))
    | ty, Pattern.Any _ -> Any (ref (Some ty))
    | Type.App (Type.Const "tuple", tys), Pattern.Tuple (_, pss) ->
      Tuple (List.map2 of_pattern tys pss, ref (Some ty))
    | Type.Const "int", Pattern.Int (_, i) -> Int i
    | Type.Const "char", Pattern.Char (_, c) -> Char c
    | Type.Const "bool", Pattern.Bool (_, b) -> Bool b
    | Type.Const "unit", Pattern.Unit _ -> Unit
    | Type.Variant ty, Pattern.Variant (_, name, pss) ->
      let (map, rest) = Type.compact ty in
      begin try let tys = Type.Set.find name map in
          Variant (name, of_pattern (List.hd tys) pss, ref (Some ty))
          (** XXX: List.hd is right ? *)
        with Not_found -> raise (Invalid_argument "Synthesis.TPattern.of_pattern")
      end
    | _ ->
      raise (Invalid_argument "Synthesis.TPattern.of_pattern")

  let omega = Any (ref None)
  let rec omegas i = if i <= 0 then [] else omega :: (omegas (i - 1))
  let omega_list l = List.map (fun _ -> omega) l

  let zero = Int 0

  (** Normalize a pattern, all arguments are omega (simple pattern) and no more
      variables.
  *)
  let rec normalize q = match q with
    | Any _ | Int _ | Char _ | Bool _ | Unit -> q
    | Var (_, ty) -> Any ty
    | Tuple (l, ty) ->
      Tuple (omega_list l, ty)
    | Variant (label, p, ty) ->
      Variant (label, omega, ty)

  (** Build normalized (eg. [normalize]) discriminating pattern,
      in the non-data type case.
  *)
  let discr_pattern q pss =
    let rec aux acc pss = match pss with
      | ((Any _ | Var _) :: _) :: pss ->
        aux acc pss
      | (((Tuple _) as p) :: _) :: _ ->
        normalize p
      | _ -> acc
    in
    match normalize q with
    | (Any _) as q -> aux q pss
    | q -> q

  (** Check top matching *)
  let simple_match p1 p2 = match p1, p2 with
    | Int i1, Int i2 -> i1 = i2
    | Char c1, Char c2 -> c1 = c2
    | Bool b1, Bool b2 -> b1 = b2
    | Unit, Unit -> true
    | Variant (l1, _ , _), Variant (l2, _, _) -> l1 = l2
    | Tuple _, Tuple _ -> true
    | _, (Any _ | Var _) -> true
    | _, _ -> false

  (** Build argument list when p2 >= p1, where p1 is a simple pattern *)
  let simple_match_args p1 p2 = match p2 with
    | Tuple (args, _) -> args
    | Variant (_, arg, _) -> [arg]
    | Var _ | Any _ ->
      begin match p1 with
      | Tuple (args, _) -> omega_list args
      | Variant (_, _, args) -> [omega]
      | _ -> []
      end
    | _ -> []

  (** Pattern p0 is the discriminating pattern,
      returns [(q0, pss0); ... ; (qn, pssn)]
      where the qi's are simple pattern and pssi's are
      matched matrices.

      (qi, []) is impossible
      In the case when matching is useless (all-variable case),
      return []
  *)
  let filter_all pat0 pss =
    let rec insert q qs env =
      match env with
      | [] ->
        let q0 = normalize q in
        [q0, [simple_match_args q0 q @ qs]]
      | ((q0, pss) as c) :: env ->
        if simple_match q0 q
        then (q0, ((simple_match_args q0 q @ qs) :: pss)) :: env
        else c :: insert q qs env in
    let rec filter_rec env = function
      | ((Any _ | Var _) :: _) :: pss ->
        filter_rec env pss
      | (p :: ps) :: pss ->
        filter_rec (insert p ps env) pss
      | _ -> env
    and filter_omega env = function
      | ((Any _ | Var _) :: ps) :: pss ->
        filter_omega
          (List.map (fun (q, qss) -> (q, (simple_match_args q omega @ ps) :: qss))
           env)
          pss
      | _ :: pss -> filter_omega env pss
      | [] -> env in
    filter_omega
      (filter_rec
        (match pat0 with
         | Tuple _ -> [pat0, []]
         | _ -> [])
       pss)
      pss

  let filter_extra pss =
    let rec filter_rec = function
      | ((Any _ | Var _) :: qs) :: pss ->
        qs :: filter_rec pss
      | _ :: pss -> filter_rec pss
      | [] -> []
    in filter_rec pss

  let full_match try_close env =
    match env with
    | (Variant (_, _, { contents = Some ty }), _) :: _ ->
      (** We try to find all of variants in pattern `env` *)
      let (row, _) = Type.compact ty in
      let fields =
        List.map
          (function (Variant (tag, _, _), _) -> tag
                  | _ -> assert false)
          (** Or, we have `match v { X(ø) → ø | 42 → ø }`,
              so we must do this after first type-checking *)
          env
      in
      if try_close
      then Type.Set.for_all (fun tag _ -> List.mem tag fields) row
        (** We try to test the exhaustiveness of pattern *)
      else Type.Set.for_all (fun tag _ -> List.mem tag fields) row
           && Type.is_close ty
        (** it's [> A | B ] if Type.is_close return false,
          * so it's not exhaustive *)
    | (Int _, _) :: _ -> false (* 2^32 is too much *)
    | (Char _, _) :: _ -> List.length env = 256
    | (Bool _, _) :: _ -> List.length env = 2
    | (Tuple _, _) :: _ -> true
    | (Unit, _) :: _ -> true
    | _ -> raise (Invalid_argument "Synthesis.TPattern.full_match")

  let rec set_last a = function
    | [] -> []
    | [ _ ] -> [ a ]
    | x :: r -> x :: set_last a r

  let rec mark_partial = function
    | ((Any _ | Var _) :: _ as ps) :: pss ->
      ps :: mark_partial pss
    | ps :: pss ->
      (set_last zero ps) :: mark_partial pss
      (** TODO: Why zero ? *)
    | [] -> []

  let rec pressure_variants ?(def=true) = function
    | [] -> false
    | [] :: _ -> true
    | pss ->
        let q0 = discr_pattern omega pss in
        begin match filter_all q0 pss with
          | [] -> pressure_variants ~def (filter_extra pss)
          | constrs ->
            let rec try_non_omega = function
              | (p, pss) :: rem ->
                let ok = pressure_variants ~def pss in
                try_non_omega rem && ok
              | [] -> true
            in
            if full_match false constrs
            then try_non_omega constrs
            else if def = false
              then pressure_variants ~def:false (filter_extra pss)
            else
              let full = full_match true constrs in
              let ok =
                if full then try_non_omega constrs
                else try_non_omega (filter_all q0 (mark_partial pss))
              in
              begin match constrs with
                | (Variant (_, _, { contents = Some ty }), _) :: _ ->
                  if pressure_variants ~def:false (filter_extra pss) then ()
                  else Type.close_row ty
                | _ -> ()
              end; ok
        end
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
exception Unbound_constructor of string
exception Error of (Loc.t * exn)

let () = Printexc.register_printer
    (function
      | Recursive_type ty ->
          Some (Printf.sprintf "recursive type in %s" (Type.to_string ty))
      | Conflict (a, b) ->
          Some (Printf.sprintf "conflict between %s and %s"
                (Type.to_string a)
                (Type.to_string b))
      | Circularity (a, b) ->
          Some (Printf.sprintf "circularity between %s and %s"
                (Type.to_string a)
                (Type.to_string b))
      | Mismatch_arguments ty ->
          Some (Printf.sprintf "mismatch argument(s) on %s" (Type.to_string ty))
      | Expected_function ty ->
          Some (Printf.sprintf "expected function in %s" (Type.to_string ty))
      | Unbound_variable name ->
          Some (Printf.sprintf "unbound variable %s" name)
      | Variable_no_instantiated ->
          Some "variable no instanciatedc"
      | Polymorphic_argument_inferred lst ->
          Some (Printf.sprintf "polymorphic argument inferred in %s"
                (String.concat " or " (List.map Type.to_string lst)))
      | No_instance (a, b) ->
          Some (Printf.sprintf "%s is not an instance of %s"
                (Type.to_string a)
                (Type.to_string b))
      | Unknown_type name ->
          Some (Printf.sprintf "unknown type %s" name)
      | Unbound_constructor name ->
          Some (Printf.sprintf "unbound constructor %s" name)
      | Error (loc, exn) ->
          Some (Printf.sprintf "Typing error: %s at %s"
                (Printexc.to_string exn)
                (Loc.to_string loc))
      | _ -> None)

(** compute_variable : makes sure that the type variable being
    unified doesn't occur within the it is being unified with.

    Each variable has a "level", which indicates how definition has been
    created. The higher the level, the higher the variable has been introduced
    recently. When assigning a variable V by a type T, we must preserve this
    information. In particular, if the type T contains variables that higher
    level, it is necessary to lower the level of these variables at V.
    Everything must happen as if, instead of having introduced a variable to a
    certain date and then determined its value by unification, we had guessed
    the correct value at the Introduction of the variable.

    @param id id of `Unbound` type variable to prevent circularity
    @param level level of `Unbound` type
    @param ty type with wich we will unify

    @raise Recursive_type if [id] is found is [ty]
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
    | Type.Abs (_, ty) -> aux ty
    | Type.Const _ -> ()
    | Type.RowEmpty -> ()
    | Type.Alias (_, ty) -> aux ty
    | Type.RowExtend (map, rest) ->
      Type.Set.iter (fun _ -> List.iter aux) map; aux rest
    | Type.Variant row -> aux row
    | Type.Record row -> aux row
  in aux ty

(** substitution : takes a list of `Bound` variables [ids], a list of
    replacement types [tys] and type a type [ty]. Returns a new type [ty]
    with `Bound` variables substitued with respective replacement types.

    @param ids list of id of `Bound` variables
    @param tys list of type for replace with respective id
    @param ty type to apply modification
*)
let substitution ids tys ty =
  let module Map =
    struct
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
  in
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
    | Type.Abs (ids, ty) ->
      Type.Abs (ids, ty)
    | Type.Alias (name, ty) -> Type.Alias (name, aux map ty)
    | Type.Variant row -> Type.Variant (aux map row)
    | Type.Record row -> Type.Record (aux map row)
    | Type.RowEmpty as ty -> ty
    | Type.RowExtend (set, rest) ->
      Type.RowExtend (Type.Set.map (List.map (aux map)) set, aux map rest)
  in aux (Map.of_lists ids tys) ty

let catch_generic_variable ty =
  let module Set =
    struct
      type 'a t = ('a, unit) Hashtbl.t

      let create size : 'a t = Hashtbl.create size
      let add set data = Hashtbl.replace set data ()
    end
  in let set = Set.create 16 in
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
    | Type.Abs (_, ty) -> aux ty
    | Type.Alias (_, ty) -> aux ty
    | Type.Record row -> aux row
    | Type.Variant row -> aux row
    | Type.RowEmpty -> ()
    | Type.RowExtend (map, rest) ->
      Type.Set.iter (fun _ -> List.iter aux) map; aux rest
  in aux ty; set

(** union : takes a list of `Generic` type variables and types [ty1] and type
    [ty2] and checks if any of the `Generic` type variables appears in any of
    the sets of free generic variables in [ty1] or [ty2].

    See [catch_generic_variable] for capturing free generic variables.

    @param lst list of `Generic` type variables
    @param ty1
    @param ty2

    @return true if a `Generic` type variables appears in [ty1] or [ty2]
*)
let union lst ty1 ty2 =
  let set1 = catch_generic_variable ty1 in
  let set2 = catch_generic_variable ty2 in
  List.exists
    (fun var -> Hashtbl.mem set1 var || Hashtbl.mem set2 var) lst

(** expand : expands type by replacing its alias with new bodies to which
    they are linked. The function returns the standard type and a [bool]
    notifier if an alias was expanded.

    @param gamma all definitions
    @param ty type expand
*)
let rec expand ~gamma ty =
  let is_expanded = ref false in
  let rec aux = function
    | Type.Const name ->
      begin
        try let ty = Type.Alias (name, Gamma.lookup name gamma |> Type.copy)
            in is_expanded := true; ty
        with Not_found -> Type.Const name
      end
    | Type.App (f, a) ->
      Type.App (aux f, List.map aux a)
    | Type.Arrow (a, r) ->
      Type.Arrow (List.map aux a, aux r)
    | Type.Var ({ contents = Type.Link t } as var) ->
      var := Type.Link (aux t);
      Type.Var var
    | Type.Forall (lst, ty) ->
      Type.Forall (lst, aux ty)
    | Type.Alias (name, ty) ->
      Type.Alias (name, aux ty)
    | Type.Abs (lst, ty) ->
      Type.Abs (lst, aux ty)
    | Type.Record row -> Type.Record (aux row)
    | Type.Variant row -> Type.Variant (aux row)
    | Type.RowExtend (map, rest) ->
      Type.RowExtend (Type.Set.map (List.map aux) map, aux rest)
    | ty -> ty
  in aux ty

(** unification : takes two types and tries to them, i.e. determine if they can
    be equal. Type constants unify with identical type contents, and arrow types
    and other structured types are unified by unifying each of their components.
    After first performing an "occurs check" (see [compute_variable]), unbound
    type variables can be unified with any type by replacing their reference
    with a link pointing to the other type T.

    If type T pointed is known, it should not reduce the variable type T since
    it can be shared by other variables and we could break links.

    @param t1
    @param t2

    @raise Conflict if can not unify t1 and t2 (ex: unify int bool)
    @raise Circularity if [ty1] is dependent with [ty2]
    @raise Variable_no_instantiated if found `Bound` type variable. Indeed, it's
    normally impossible after a substitution by `Forall` normalized expression.
*)
let rec unification t1 t2 =
  if t1 == t2 then ()
  else match t1, t2 with
    | Type.Const n1, Type.Const n2 when n1 = n2 -> ()
    | Type.App (c1, a1), Type.App (c2, a2) ->
      unification c1 c2;
      begin
        try List.iter2 (fun a1 a2 -> unification a1 a2) a1 a2
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

    (** First, we create a fresh `Generic` type variable for every type
        variable bound by the two polymorphic types. Here, we rely on the fact
        that both types are normalized (see [normalize]), so equivalent generic
        type variables should appear in the same locations in both types.

        Then, we substitute all `Bound` type variables in both types with
        `Generic` type variables, an try to unify them.

        If unification success, we check that none of the `Generic` type
        variables escapes, otherwise, we would successfilly unify types
        `∀a. a → a` and `∀a b. a → b`, where `b` is a unifiable `Unbound` type
        variable.
    *)
    | (Type.Forall (ids1, ty1) as forall1),
      (Type.Forall (ids2, ty2) as forall2) ->
      let lst =
        try List.rev_map2 (fun _ _ -> Type.Variable.generic ()) ids1 ids2
        with Invalid_argument "List.rev_map2" -> raise (Conflict (ty1, ty2))
      in
      let ty1 = substitution ids1 lst ty1
        |> Type.normalize in
      let ty2 = substitution ids2 lst ty2
        |> Type.normalize in
      unification ty1 ty2;
      if union lst forall1 forall2
      then raise (Conflict (forall1, forall2))

    | Type.Record row1, Type.Record row2 -> unification row1 row2
    | Type.Variant row1, Type.Variant row2 -> unification row1 row2
    | Type.RowEmpty, Type.RowEmpty -> ()
    | (Type.RowExtend _ as row1), (Type.RowExtend _ as row2) ->
      unification_rows row1 row2

    | Type.Alias (_, ty1), ty2 -> unification ty1 ty2
    | ty1, Type.Alias (_, ty2) -> unification ty1 ty2

    | ty1, ty2 ->
      raise (Conflict (ty1, ty2))

and unification_rows row1 row2 =
  let map1, rest1 = Type.compact row1 in
  let map2, rest2 = Type.compact row2 in

  let rec unification_types lst1 lst2 = match lst1, lst2 with
    | ty1 :: rest1, ty2 :: rest2 ->
      unification ty1 ty2; unification_types rest1 rest2
    | _ -> lst1, lst2
  in

  let rec unification_labels missing1 missing2 labels1 labels2 =
    match labels1, labels2 with
    | [], [] -> missing1, missing2
    | [], _ -> (Type.Set.add_list missing1 labels2, missing2)
    | _, [] -> (missing1, Type.Set.add_list missing2 labels1)
    | (label1, tys1) :: rest1, (label2, tys2) :: rest2 ->
      begin match String.compare label1 label2 with
        | 0 ->
            let missing1, missing2 = match unification_types tys1 tys2 with
            | [], [] -> missing1, missing2
            | ty1s, [] -> missing1, Type.Set.add label1 tys1 missing2
            | [], tys2 -> Type.Set.add label2 tys2 missing1, missing2
            | _ -> assert false
          in
          unification_labels missing1 missing2 rest1 rest2
        | x when x < 0 ->
          unification_labels
            missing1 (Type.Set.add label1 tys1 missing2)
            rest1 labels2
        | x ->
          unification_labels
            (Type.Set.add label2 tys2 missing1) missing2
            labels1 rest2
      end
  in
  let missing1, missing2 = unification_labels
    Type.Set.empty Type.Set.empty
    (Type.Set.bindings map1) (Type.Set.bindings map2)
  in
  match Type.Set.is_empty missing1, Type.Set.is_empty missing2 with
  | true, true -> unification rest1 rest2
  | true, false -> unification rest2 (Type.RowExtend (missing2, rest1))
  | false, true -> unification rest1 (Type.RowExtend (missing1, rest2))
  | false, false ->
    match rest1 with
    | Type.RowEmpty ->
      unification rest1 (Type.RowExtend (missing1, Type.Variable.make 0))
    | Type.Var ({ contents = Type.Unbound (_, level) } as var) ->
      begin
        let rest' = Type.Variable.make level in
        unification rest2 (Type.RowExtend (missing2, rest'));
        match !var with
          | Type.Link _ ->
            raise (Recursive_type (Type.RowExtend (missing2, rest')))
          | _ -> ();
        unification rest1 (Type.RowExtend (missing1, rest'))
      end
    | _ -> assert false

(** specialization : instantiates a `Forall` type by substituting bound type
    variables by fresh `Unbound` type variables, wich can then by unified with
    any other type.

    @param level level for create `Unbound` type variable
    @param ty type to apply modification
*)
let specialization level ty =
  let rec aux level = function
    | Type.Forall (ids, ty) ->
      let lst = List.rev_map (fun _ -> Type.Variable.make level) ids in
      substitution ids lst ty
    | Type.Var { contents = Type.Link ty } -> aux level ty
    | Type.Alias (name, ty) -> Type.Alias (name, aux level ty)
    | Type.RowExtend (map, rest) ->
      Type.RowExtend (Type.Set.map (List.map (aux level)) map, aux level rest)
    | ty -> ty
  in aux level ty

(** specialization_annotation : same as specialization but for annotation *)
let specialization_annotation level (lst, ty) =
  let aux = function
    | [], ty -> [], ty
    | ids, ty ->
      let lst = List.rev_map (fun _ -> Type.Variable.make level) ids in
      lst, substitution ids lst ty
  in aux (lst, ty)

(** generalization : transforms a type into a `Forall` type by substituting all
    `Unbound` type variables, with levels higher then the [level] with `Bound`
    type variables. The traverse order is same as Parse.replace.

    @param level date of declaration
    @param ty type to apply modification
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
    | Type.Alias (_, ty) -> aux ty
    | Type.Abs (_, ty) -> aux ty
    | Type.Variant row -> aux row
    | Type.Record row -> aux row
    | Type.RowEmpty -> ()
    | Type.RowExtend (map, rest) ->
      Type.Set.iter (fun _ -> List.iter aux) map; aux rest
  in aux ty; match !acc with
  | [] -> ty
  | ids -> Type.Forall (List.rev ids, ty)

let rec compute_function n = function
  | Type.Alias (_, ty) -> compute_function n ty
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
        | n -> aux (Type.Variable.make level :: acc) (n - 1)
      in aux [] n
    in let r = Type.Variable.make level in
    var := Type.Link (Type.Arrow (lst, r));
    lst, r
  | _ as ty -> raise (Expected_function ty)

(** subsume : takes two types [ty1] [ty2] and determines if [ty1] is an of
    [ty2]. For example, `int → int` is an instance of `∀a. a → a` (the type of
    `id`) which in turn is an instance of `∀a b. a → b` (type of `magic`). This
    means that we can pass `id` as an argument to a function expecting
    `int → int` and we can pass `magic` to a function expecing `∀a. a → a` but
    not the other way round. To determine if [ty1] is an instance of [ty2],
    [subsume] first instantiates [ty2], the more general type, with `Unbound`
    type varibales. If [ty1] is not polymorphic, is simply unifies the two
    types. Otherwise, it instantiates [ty1] with `Generic` type variables and
    unifies both instantiated types. If unification success, we check that no
    generic variables escapes (see [union]).

    @param level level for make specializationon [ty2]
    @param ty1
    @param ty2

    @raise No_instance if [ty1] is not an instance of [ty2]
*)
let subsume ~gamma ~level ty1 ty2 =
  let ty2' = specialization level ty2 in
  match Type.unlink ty1 with
  | Type.Forall (ids, ty1) as forall ->
    let lst = List.rev_map (fun _ -> Type.Variable.generic ()) ids in
    let ty1' = substitution ids lst ty1 in
    unification ty1' ty2';
    if union lst forall ty2
    then raise (No_instance (ty1, ty2))
  | ty1 -> unification ty1 ty2'

let ( >!= ) func handle_error =
  try func () with
  | exn -> handle_error exn

let raise_with_loc loc = function
  | Error (loc, exn) as error -> raise error
  | exn -> raise (Error (loc, exn))

let rec eval
    ?(gamma = Gamma.empty)
    ?(env = Environment.empty)
    ?(level = 0) = function
  | Ast.Var (loc, name) ->
    (fun () ->
       try Environment.lookup env name |> expand ~gamma |> Type.normalize
       with Not_found -> raise (Unbound_variable name))
    >!= raise_with_loc loc
  | Ast.Abs (loc, a, c) ->

    (** To infer the type of functions, we first extend the environment with the
        types of the parameters, which might be annoted. We remember all new
        type variables that appear in parameter types in [lstvar] so that we
        can later make sure that none of them was unified with polymorphic type.

        We then infer the type of the function body using the extended
        environment, and instantiate it unless it's annotated.

        Finally, we generalize the resulting function type.

        @raise Polymorphic_argument_inferred if a parameter has been unified
        with polymorphic type
    *)
    (fun () ->
       let refenv = ref env in
       let lstvar = ref [] in
       let a' = List.map (fun (name, ann) ->
           let ty = match ann with
             | None ->
               let var = Type.Variable.make (level + 1) in
               lstvar := var :: !lstvar;
               var
             | Some (lst, ty) ->
               let ty_expanded = expand ~gamma ty in
               let ty_normalized = Type.normalize ty_expanded in
               let vars, ty_normalized = specialization_annotation
                   (level + 1)
                   (lst, ty_normalized) in
               lstvar := vars @ !lstvar;
               ty_normalized
           in refenv := Environment.extend !refenv name ty;
           ty) a
       in
       let r' = eval ~gamma ~env:!refenv ~level:(level + 1) c in
       let r' =
         if Ast.is_annotated c then r'
         else specialization (level + 1) r' in
       if not (List.for_all Type.is_monomorphic !lstvar)
       then raise (Polymorphic_argument_inferred !lstvar)
       else generalization level (Type.Arrow (a', r')))
    >!= raise_with_loc loc
  | Ast.App (loc, f, a) ->

    (** To infer the type of function application we first infer the type of the
        function being called, instantiate it and separate parameter types from
        function return type.

        The core of the algorithm is infering argument types in the function
        [compute_argument].
    *)
    (fun () ->
       let ft = eval ~gamma ~env ~level:(level + 1) f in
       let ft_expanded = expand ~gamma ft in
       let ft_normalized = Type.normalize ft_expanded in
       let ft_normalized = specialization (level + 1) ft_normalized in
       let a', r' = compute_function (List.length a) ft_normalized in
       compute_argument gamma env (level + 1) a' a;
       generalization level (specialization (level + 1) r'))
    >!= raise_with_loc loc
  | Ast.Let (loc, n, e, c) ->
    (fun () ->
       let e' = eval ~gamma ~env ~level:(level + 1) e in
       eval ~gamma ~env:(Environment.extend env n e') ~level c)
    >!= raise_with_loc loc
  | Ast.Alias (loc, n, e, c) ->
    (fun () -> eval ~gamma:(Gamma.add n e gamma) ~env ~level c)
    >!= raise_with_loc loc
  | Ast.Rec (loc, n, e, c) ->
    (fun () ->
       let n' = Type.Variable.make (level + 1) in
       let ext = Environment.extend env n n' in
       let e' = eval ~gamma ~env:ext ~level:(level + 2) e in

       (** The type of recursive function N will surely not subject to
           generalization. However, this is certainly the case for E (which is
           often the result of a `Lambda`) and it will contain a `Forall` (which
           will fake for a simple unification). Thus, we consider that N is an
           instance of E and we check it with the [subsume] function.
       *)

       subsume ~gamma ~level n' e';
       eval
         ~gamma
         ~env:(Environment.extend env n (generalization level e'))
         ~level c)
    >!= raise_with_loc loc
  | Ast.Ann (loc, e, (lst, ty)) ->

    (** Infering type annotation `expr : type` is equivalent to inferring the
        type of function call `(λ x : type .x)[expr]`, but optimized in
        this implementation of [eval].
    *)
    (fun () ->
       let ty_expanded = expand ~gamma ty in
       let ty_normalized = Type.normalize ty_expanded in
       let _, ty_normalized =
         specialization_annotation level (lst, ty_normalized) in
       let e' = eval ~gamma ~env ~level e in
       subsume ~gamma ~level ty_normalized e';
       ty_normalized)
    >!= raise_with_loc loc
  | Ast.If (loc, i, a, b) ->
    (fun () ->
       let i' = eval ~gamma ~env ~level i in
       let a' = eval ~gamma ~env ~level a in
       let b' = eval ~gamma ~env ~level b in
       unification i' Type.bool;
       unification a' b';

       (** In the case of a variant, conditional branching must bound the
           variant like this:
           > true ? A() | B()
           [ A : unit, B : unit | _a ] to [ A : unit, B : unit ]

           Both variants can be bounded only if their variables are the same
           after the unification. Otherwise, there is a type error.
       *)

       let (ret, _) = Type.bound a' b' in ret)
    >!= raise_with_loc loc
  | Ast.Seq (loc, a, b) ->
    (fun () ->
       let _ = eval ~gamma ~env ~level a in
       let ty = eval ~gamma ~env ~level b in
       unification ty Type.unit;
       ty)
    >!= raise_with_loc loc
  | Ast.Int _ -> Type.int
  | Ast.Bool _ -> Type.bool
  | Ast.Char _ -> Type.char
  | Ast.Unit _ -> Type.unit
  | Ast.Tuple (_, l) ->
    Type.tuple (List.map (eval ~gamma ~env ~level) l)

  | Ast.Variant (loc, ctor, expr) ->
    (fun () ->
      let rest' = Type.Variable.make level in
      let ctor' = Type.Variable.make level in
      let ptr' = ctor' in
      let result' =
        Type.Variant
          (Type.RowExtend (Type.Set.singleton ctor [ctor'], rest')) in
      let expr' = (eval ~gamma ~env ~level expr) in
      unification ptr' expr';
      result')
    >!= raise_with_loc loc

  | Ast.Case (loc, expr, pattern) ->
    let make_matrix ty pss =
      let pss = List.map (fun p -> [TPattern.of_pattern ty p; TPattern.omega]) pss in
      let ty = [ty; Type.Const "ε"] in
      (ty, pss)
    in
    (fun () ->
       let ty = eval ~gamma ~env ~level expr in
       let rt = Type.Variable.make level in
       let compute_branch (branch, expr) =
         let (ty', garbage) = compute_pattern gamma [] level branch in
         unification ty ty';
         let env = List.fold_right (fun (k, v) -> Environment.add k v) garbage env in
         let rt' = eval ~gamma ~env ~level expr in
         unification rt rt'
       in
       List.iter compute_branch pattern;
       let (ty, pss) = make_matrix ty (List.map fst pattern) in
       ignore (TPattern.pressure_variants pss);
       rt)
    >!= raise_with_loc loc

  | Ast.RecordEmpty loc ->
    (fun () -> Type.Record Type.RowEmpty)
    >!= raise_with_loc loc
  | Ast.RecordSelect (loc, expr, label) ->
    (fun () ->
      let rest = Type.Variable.make level in
      let field = Type.Variable.make level in
      let ty =
        Type.Record
          (Type.RowExtend (Type.Set.singleton label [field], rest)) in
      let rt = field in
      unification ty (eval ~gamma ~env ~level expr);
      rt)
    >!= raise_with_loc loc
  | Ast.RecordRestrict (loc, expr, label) ->
    (fun () ->
      let rest = Type.Variable.make level in
      let field = Type.Variable.make level in
      let ty =
        Type.Record
          (Type.RowExtend (Type.Set.singleton label [field], rest)) in
      let rt = Type.Record rest in
      unification ty (eval ~gamma ~env ~level expr);
      rt)
    >!= raise_with_loc loc

(** compute_argument : after infering the type of argument, we use the function
    [subsume] (or [unification] if the argument is annotated) to determine if
    the parameter type is an instance of the type of the argument.

    When calling functions with multiple arguments, we must first [subsume] the
    types of arguments for those parameters that are type variables, otherwise
    we would fail to typecheck applications such as `(revapply id poly), where
    `revapply : ∀a b. a → (a → b) → b`,
    `poly : (∀a. a → a) → tuple[int, bool]` and
    `id` : ∀a. a → a.

    @param gamma all definitions
    @param env environment
    @param level level for [subsume] and [eval]
    @param tys list of types of arguments
    @param a list of arguments
*)
and compute_argument gamma env level tys a =
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
       let ty' = eval ~gamma ~env ~level a in
       if Ast.is_annotated a
       then unification ty ty'
       else subsume ~gamma ~level ty ty')
    slst

and compute_pattern gamma lst level = function
  | Pattern.Var (_, name) ->
    let ty = Type.Variable.make level in
    (ty, (name, ty) :: lst)
  | Pattern.Any _ ->
    (Type.Variable.make level, lst)
  | Pattern.Bool _ -> (Type.bool, lst)
  | Pattern.Int _ -> (Type.int, lst)
  | Pattern.Char _ -> (Type.char, lst)
  | Pattern.Unit _ -> (Type.unit, lst)
  | Pattern.Tuple (_, l) ->
    let (tys, new_lst) =
      List.fold_left
        (fun (tys, lst) x ->
          let (ty, new_lst) = compute_pattern gamma lst level x in
          (ty :: tys, new_lst))
        ([], lst) l
    in (Type.tuple (List.rev tys), new_lst)
  | Pattern.Variant (_, ctor, expr) ->
    let (expr', lst') = compute_pattern gamma lst level expr in
    let rest' = Type.Variable.make level in
    (Type.Variant (Type.RowExtend (Type.Set.singleton ctor [expr'], rest')),
     lst')
