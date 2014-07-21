type position = { lnum : int; cnum : int; seek : int; }
type t = (position * position)

let make start_pos end_pos =
  let get_pos pos =
    { lnum = pos.Lexing.pos_lnum;
      cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol;
      seek = pos.Lexing.pos_cnum; }
  in (get_pos start_pos, get_pos end_pos)

let compose start stop = (start, stop)

let dummy =
  ({ lnum = 0; cnum = 0; seek = 0 },
   { lnum = 0; cnum = 0; seek = 0 })

let start = fst
let stop = snd

let catch_str ?(hidden=['\n'; '\t']) start stop in_ch =
  let buffer = Buffer.create 16 in
  let rec aux = function
    | 0, _ -> Buffer.contents buffer
    | n, c when List.exists ((=) c) hidden = false ->
      Buffer.add_char buffer c;
      aux (n - 1, input_char in_ch)
    | n, _ -> aux (n - 1, input_char in_ch)
  in
  seek_in in_ch start.seek;
  aux (stop.seek - start.seek, input_char in_ch)

let to_string_of_file (a, b) filename =
  let fd = open_in filename in
  let cc = catch_str a b fd in
  close_in fd; cc

let to_string_of_line (a, b) str =
  Printf.sprintf "%s\n%-*s%s\n%!" str a.seek ""
    (String.make (b.seek - a.seek) '^')

let to_string (a, b) =
  let print_aux ty () (a, b) =
    if a = b then Printf.sprintf "%s%d" ty a
    else Printf.sprintf "%s%d - %d" ty a b
  in
  Printf.sprintf "%a %a"
    (print_aux "l.") (a.lnum, b.lnum)
    (print_aux "c.") (a.cnum, b.cnum)
