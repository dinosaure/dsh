module Environment = struct
  include Map.Make (String)

  let extend env names values =
    List.fold_left
      (fun acc (names, values) -> add names values acc)
      env (List.combine names values)
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

type t =
  | Int of int
  | Bool of bool
  | Char of char
  | Tuple of t list
  | Unit
  | Closure of (t Environment.t * string list * Ast.t * string option)
  | Primitive of (t list -> t)
  | Variant of (string * t)

let rec to_string = function
  | Int i -> string_of_int i
  | Bool b -> if b then "true" else "false"
  | Char c -> String.make 1 c
  | Tuple l ->
    Printf.sprintf "(%s)" (String.of_list ~sep:", " to_string l)
  | Unit -> "()"
  | Closure _ -> "#closure"
  | Primitive _ -> "#primitive"
  | Variant (ctor, Unit) -> ctor
  | Variant (ctor, expr) ->
    Printf.sprintf "(%s %a)" ctor (fun () -> to_string) expr

exception Unbound_variable of string
exception Expected_function
exception Expected_boolean
exception Mismatch_pattern of (Pattern.t * t)
exception Pattern_matching_fail
exception Error of (Location.t * exn)

let () = Printexc.register_printer
    (function
      | Unbound_variable name ->
        Some ("unbound variable " ^ name)
      | Expected_function ->
        Some "expected function"
      | Expected_boolean ->
        Some "expected boolean"
      | Mismatch_pattern (p, t) ->
        Some (Printf.sprintf "mismatch pattern between %s and %s"
              (Pattern.to_string p) (to_string t))
      | Pattern_matching_fail -> Some "pattern-matching fail"
      | Error (loc, exn) ->
        Some (Printf.sprintf "%s at %s"
                (Printexc.to_string exn)
                (Location.to_string loc))
      | _ -> None)

let ( >!= ) func handle_error =
  try func ()
  with exn -> handle_error exn

let raise_with_loc loc = function
  | Error (loc, exn) as error -> raise error
  | exn -> raise (Error (loc, exn))

let rec eval env = function
  | Ast.Var (loc, name) ->
    (fun () ->
       try Environment.find name env
       with Not_found -> raise (Unbound_variable name))
    >!= raise_with_loc loc

  (** The `if` is a primitive language because evaluation must be lazy. Indeed,
    * in a policy call-by-value, in the case of a recursive definition, we will
    * evaluate the `if` arguments independently of the predicat that would a
    * loop infinite.
  *)

  | Ast.If (loc, i, a, b) ->
    (fun () ->
       eval env i
       |> (function
           | Bool true -> eval env a
           | Bool false -> eval env b
           | _ -> raise Expected_boolean))
    >!= raise_with_loc loc
  | Ast.Seq (loc, a, b) ->
    (fun () ->
       let _ = eval env a in
       eval env b)
    >!= raise_with_loc loc
  | Ast.App (loc, f, a) ->
    (fun () ->
       let a = List.map (eval env) a in
       eval env f
       |> (function
           | Closure (ext, names, body, None) ->
             eval (Environment.extend ext names a) body
           | Closure (ext, names, body, Some fix) as closure ->
             eval (Environment.extend ext (fix :: names) (closure :: a)) body
           | Primitive func -> func a
           | _ -> raise Expected_function))
    >!= raise_with_loc loc
  | Ast.Abs (loc, a, c) ->
    Closure (env, List.map (fun (name, _) -> name) a, c, None)
  | Ast.Let (loc, name, value, expr) ->
    eval (Environment.add name (eval env value) env) expr

  (**
      It is impossible to add the definition of the function in the environment
      - this would imply that in the environment definition of this new
      feature, we add the definition of the same function - it would a loop at
      infinity.

      The Closure model therefore includes a string option indicating whether
      it is recursive or not. Thus, during application thereof, is added to the
      environment, without having cyclic recursively.
  *)
  | Ast.Rec (loc, name, value, expr) ->
    (fun () ->
       eval env value
       |> (function
           | Closure (env', args, body, None) ->
             Closure (env', args, body, Some name)
           | x -> x)
       |> fun value -> eval (Environment.add name value env) expr)
    >!= raise_with_loc loc
  | Ast.Ann (_, expr, _) ->
    eval env expr
  | Ast.Int (_, value) -> Int value
  | Ast.Bool (_, value) -> Bool value
  | Ast.Char (_, value) -> Char value
  | Ast.Unit _ -> Unit
  | Ast.Alias (_, _, _, expr) -> eval env expr
  | Ast.Variant (_, ctor, expr) -> Variant (ctor, eval env expr)
  | Ast.Tuple (_, l) -> Tuple (List.map (eval env) l)
  | Ast.Match (loc, expr, patterns) ->
    (fun () ->
      let value = eval env expr in
      let rec compute_branch = function
        | (pattern, expr) :: r ->
          begin
            try let (names, values) = compute_pattern value pattern
                                      |> List.split
                in eval (Environment.extend env names values) expr
            with _ -> compute_branch r
          end
        | _ -> raise Pattern_matching_fail
      in compute_branch patterns)
    >!= raise_with_loc loc

and compute_pattern value pattern = match value, pattern with
  | (value, Pattern.Var (_, name)) -> [(name, value)]
  | (Bool b1, Pattern.Bool (_, b2)) when b1 = b2 -> []
  | (Int i1, Pattern.Int (_, i2)) when i1 = i2 -> []
  | (Char c1, Pattern.Char (_, c2)) when c1 = c2 -> []
  | (Unit, Pattern.Unit _) -> []
  | (Tuple l1, Pattern.Tuple (loc, l2)) ->
    let rec aux = function
      | [], [] -> []
      | x1 :: r1, x2 :: r2 ->
        compute_pattern x1 x2 @ aux (r1, r2)
      | _ -> raise (Mismatch_pattern (Pattern.Tuple (loc, l2), Tuple l1))
    in aux (l1, l2)
  | (Variant (n1, e1), Pattern.Variant (_, n2, e2)) when n1 = n2 ->
    compute_pattern e1 e2
  | _ -> raise Pattern_matching_fail
