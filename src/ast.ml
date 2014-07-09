type t =
  | Var of string
  | App of t * t list
  | Abs of string list * t
  | Let of string * t * t

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

let to_string tree =
  let buffer = Buffer.create 16 in
  let rec compute buffer = function
    | Var name -> Buffer.add_string buffer name
    | App (f, a) ->
      Printf.bprintf buffer "(%a %a)"
        compute f
        (Buffer.add_list ~sep:" " compute) a
    | Abs (a, c) ->
      Printf.bprintf buffer "(lambda (%a) %a)"
        (Buffer.add_list ~sep:", " Buffer.add_string) a
        compute c
    | Let (name, e, c) ->
      Printf.bprintf buffer "(let (%s %a) %a)"
        name
        compute e
        compute c
  in compute buffer tree; Buffer.contents buffer
