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
  | Char of char
  | Unit
  | Closure of (t Environment.t * string list * Ast.t * string option)
  | Primitive of (t list -> t)

exception Unbound_variable of string
exception Expected_function
exception Expected_boolean
exception Error of (Location.t * exn)

let rec string_of_exn = function
  | Unbound_variable name ->
    ("unbound variable " ^ name)
  | Expected_function -> "expected function"
  | Expected_boolean -> "expected boolean"
  | Error (loc, exn) ->
    Printf.sprintf "%s at %s" (string_of_exn exn) (Location.to_string loc)
  | exn ->
    raise (Invalid_argument ("Interpreter.string_of_exn: "
                             ^ (Printexc.to_string exn)))

let to_string = function
  | Int i -> string_of_int i
  | Bool b -> if b then "true" else "false"
  | Char c -> String.make 1 c
  | Unit -> "()"
  | Closure _ -> "#closure"
  | Primitive _ -> "#primitive"

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
       with Not_found -> raise (Unbound_variable name)
    ) >!= raise_with_loc loc
  | Ast.If (loc, i, a, b) ->
    (fun () ->
       eval env i
       |> (function
           | Bool true -> eval env a
           | Bool false -> eval env b
           | _ -> raise Expected_boolean)
    ) >!= raise_with_loc loc
  | Ast.Seq (loc, a, b) ->
    (fun () ->
      let _ = eval env a in
      eval env b
    ) >!= raise_with_loc loc
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
           | _ -> raise Expected_function)
    ) >!= raise_with_loc loc
  | Ast.Abs (loc, a, c) ->
    Closure (env, List.map (fun (name, _) -> name) a, c, None)
  | Ast.Let (loc, name, value, expr) ->
    eval (Environment.add name (eval env value) env) expr
  | Ast.Rec (loc, name, value, expr) ->
    (fun () ->
       eval env value
       |> (function
           | Closure (env', args, body, None) ->
             Closure (env', args, body, Some name)
           | x -> x)
       |> fun value -> eval (Environment.add name value env) expr
    ) >!= raise_with_loc loc
  | Ast.Ann (_, expr, _) ->
    eval env expr
  | Ast.Int (_, value) -> Int value
  | Ast.Bool (_, value) -> Bool value
  | Ast.Char (_, value) -> Char value
  | Ast.Unit _ -> Unit

let top env lst =
  let rec aux env = function
    | [] -> ()
    | Ast.Def (_, name, expr) :: r ->
      aux (Environment.add name (eval env expr) env) r
    | Ast.Expr (_, expr) :: r ->
      let _ = eval env expr in aux env r
  in aux env lst
