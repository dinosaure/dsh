type t =
  | Var of (Location.t * string)
  | App of (Location.t * t * t list)
  | Abs of (Location.t * string list * t)
  | Let of (Location.t * string * t * t)

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
    | Var (_, name) -> Buffer.add_string buffer name
    | App (_, f, a) ->
      Printf.bprintf buffer "(%a %a)"
        compute f
        (Buffer.add_list ~sep:" " compute) a
    | Abs (_, a, c) ->
      Printf.bprintf buffer "(lambda (%a) %a)"
        (Buffer.add_list ~sep:", " Buffer.add_string) a
        compute c
    | Let (_, name, e, c) ->
      Printf.bprintf buffer "(let (%s %a) %a)"
        name
        compute e
        compute c
  in compute buffer tree; Buffer.contents buffer
