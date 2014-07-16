module Environment = struct
  include Map.Make (String)

  let extend env names values =
    List.fold_left
      (fun acc (names, values) -> add names values acc)
      env (List.combine names values)
end

type t =
  | Int of int
  | Bool of bool
  | Closure of (t Environment.t * string list * Ast.t * string option)
  | Primitive of (t list -> t)

exception Unbound_variable of string
exception Expected_function
exception Expected_boolean

let to_string = function
  | Int i -> string_of_int i
  | Bool b -> if b then "true" else "false"
  | Closure _ -> "#closure"
  | Primitive _ -> "#primitive"

let rec eval ?(env = Environment.empty) = function
  | Ast.Var (_, name) ->
    begin
      try Environment.find name env
      with Not_found -> raise (Unbound_variable name)
    end
  | Ast.If (_, i, a, b) ->
    eval ~env i
    |> (function
        | Bool true -> eval ~env a
        | Bool false -> eval ~env b
        | _ -> raise Expected_boolean)
  | Ast.App (_, f, a) ->
    let a = List.map (eval ~env) a in
    eval ~env f
    |> (function
        | Closure (ext, names, body, None) ->
          eval ~env:(Environment.extend ext names a) body
        | Closure (ext, names, body, Some fix) as closure ->
          eval ~env:(Environment.extend ext (fix :: names) (closure :: a)) body
        | Primitive func -> func a
        | _ -> raise Expected_function)
  | Ast.Abs (_, a, c) ->
    Closure (env, List.map (fun (name, _) -> name) a, c, None)
  | Ast.Let (_, name, value, expr) ->
    eval ~env:(Environment.add name (eval ~env value) env) expr
  | Ast.Rec (_, name, value, expr) ->
    eval ~env value
    |> (function
        | Closure (env', args, body, None) ->
          Closure (env', args, body, Some name)
        | x -> x)
    |> fun value -> eval ~env:(Environment.add name value env) expr
  | Ast.Ann (_, expr, _) ->
    eval ~env expr
  | Ast.Int (_, value) -> Int value
  | Ast.Bool (_, value) -> Bool value
