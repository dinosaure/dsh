type t =
  | Var of (Location.t * string)
  | Variant of (Location.t * string * t)
  | Tuple of (Location.t * t list)
  | Int of (Location.t * int)
  | Char of (Location.t * char)
  | Bool of (Location.t * bool)
  | Unit of Location.t

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
