type t =
  | Const of string
  | App of (t * t list)
  | Arrow of (t list * t)
  | Var of var ref
and var =
  | Unbound of int * int
  | Link of t
  | Generic of int

module Hashtbl = struct
  include Hashtbl

  let to_list tbl = Hashtbl.fold (fun id name acc -> name :: acc) tbl []
end

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

let to_string ty =
  let buffer = Buffer.create 16 in
  let map = Hashtbl.create 16 in
  let name =
    let count = ref 0 in
    (fun () ->
       let i = !count in
       incr count;
       let name =
         (String.make 1 (char_of_int (int_of_char 'a' + (i mod 26))))
         ^ (if i >= 26 then (string_of_int (i / 26)) else "")
       in name)
  in
  let rec compute ?(in_left = false) ?(level = 0) buffer = function
    | Const name -> Buffer.add_string buffer name
    | App (f, a) ->
      Printf.bprintf buffer "(%a %a)"
        (compute ~in_left ~level) f
        (Buffer.add_list ~sep:" " compute) a
    | Arrow (a, r) ->
      Printf.bprintf buffer "%s%a -> %a%s"
        (if in_left || level = 0 then "(" else "")
        (Buffer.add_list ~sep:" -> "
           (compute
              ~in_left:(List.length a >= 1)
              ~level:(level + 1))) a
        (compute ~in_left:false ~level:(level + 1)) r
        (if in_left || level = 0 then ")" else "")
    | Var { contents = Generic id } ->
      let name =
        try Hashtbl.find map id
        with _ -> let name = name () in Hashtbl.add map id name; name
      in Buffer.add_string buffer name
    | Var { contents = Unbound (id, _) } ->
      Buffer.add_string buffer ("_" ^ (string_of_int id))
    | Var { contents = Link t } -> compute ~in_left ~level buffer t
  in compute buffer ty;
  if Hashtbl.length map > 0
  then
    Printf.sprintf "(forall (%a) %s)"
      (fun () -> String.of_list ~sep:" " (fun x -> x))
      (List.sort String.compare (Hashtbl.to_list map))
      (Buffer.contents buffer)
  else Buffer.contents buffer
