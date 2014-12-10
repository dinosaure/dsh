type t =
  | Var of (Loc.t * string)
  | Any of Loc.t
  | Variant of (Loc.t * string * t)
  | Tuple of (Loc.t * t list)
  | Int of (Loc.t * int)
  | Char of (Loc.t * char)
  | Bool of (Loc.t * bool)
  | Unit of Loc.t

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
    | Var (_, name) -> Buffer.add_string buffer name
    | Variant (_, name, Unit _) ->
      Printf.bprintf buffer "%s()" name
    | Variant (_, name, expr) ->
      Printf.bprintf buffer "%s(%a)"
        name
        compute expr
    | Tuple (_, l) ->
      raise (Failure "Not implemented")
    | Int (_, i) ->
      Printf.bprintf buffer "%d" i
    | Char (_, c) ->
      Printf.bprintf buffer "%c" c
    | Bool (_, b) ->
      Printf.bprintf buffer "%b" b
    | Unit _ -> Buffer.add_string buffer "ø"
  in compute buffer a; Buffer.contents buffer
