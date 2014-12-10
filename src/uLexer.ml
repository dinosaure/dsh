exception Lexical_error

let char_of_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let char_of_hexa a b =
  let to_value c =
    if c >= 97 then c - 87
    else if c >= 65 then c - 55
    else c - 40
  in
  let av = Char.code a |> to_value in
  let bv = Char.code b |> to_value in
  Char.chr (av * 16 + bv)

let digit = [%sedlex.regexp? '0'..'9']
let lcase = [%sedlex.regexp? 'a'..'z']
let ucase = [%sedlex.regexp? 'A'..'Z']
let ocase = [%sedlex.regexp? lcase | ucase | digit | '_' | "'"]
let scase = [%sedlex.regexp? '\\' | '\'' | '"' | 'n' | 't' | 'r' | 'b' | 'v' | 'f']
let blank = [%sedlex.regexp? ' ' | '\t' | '\013' | '\014' | '\r']

let uname = [%sedlex.regexp? ucase, Star ocase]
let lname = [%sedlex.regexp? lcase, Star ocase]

let rec ident buffer lexbuf =
  match%sedlex lexbuf with
  | blank -> Buffer.contents buffer
  | eof -> Buffer.contents buffer
  | _ ->
    let chr = Sedlexing.next lexbuf |> Char.chr in
    Buffer.add_char buffer chr;
    ident buffer lexbuf

type position =
  { mutable lnum : int;
    mutable bol: int;
    mutable cnum: int; }

let print_position ch { lnum; bol; cnum; } =
  Printf.fprintf ch "%d;%d;%d" lnum bol cnum

let rec token ~pos lexbuf =
  let jump_lexeme = ref true in
  let token = match%sedlex lexbuf with
    | blank ->
      jump_lexeme := false;
      pos.cnum <- pos.cnum + 1;
      token ~pos lexbuf
    | '\n' ->
      jump_lexeme := false;
      pos.cnum <- pos.cnum + 1;
      pos.lnum <- pos.lnum + 1;
      pos.bol <- pos.cnum;
      token ~pos lexbuf
    | '\\' -> UParser.BACKSLASH
    | '(' -> UParser.LPAR
    | ')' -> UParser.RPAR
    | '[' -> UParser.LBRACKET
    | ']' -> UParser.RBRACKET
    | '{' -> UParser.LBRACE
    | '}' -> UParser.RBRACE
    | ':' -> UParser.COLON
    | ';' -> UParser.SEMICOLON
    | '|' -> UParser.PIPE
    | ',' -> UParser.COMMA
    | '?' -> UParser.MARK
    | '=' -> UParser.EQUAL
    | '.' -> UParser.POINT
    | '_' -> UParser.WILDCARD

    | 0x2192 -> UParser.ARROW      (* → *)
    | 0x2203 -> UParser.SOME       (* ∃ *)
    | 0x2200 -> UParser.FORALL     (* ∀ *)
    | 0x03bb -> UParser.LAMBDA     (* λ *)
    | 0x00f8 -> UParser.NULL       (* ø *)

    | 'Y' -> UParser.REC
    | "in" -> UParser.IN
    | "match" -> UParser.MATCH
    | "type" -> UParser.TYPE

    | "true" -> UParser.BOOL true
    | "false" -> UParser.BOOL false

    | Plus digit ->
      UParser.NUMBER (int_of_string (Sedlexing.Utf8.lexeme lexbuf))

    | uname -> UParser.UNAME (Sedlexing.Utf8.lexeme lexbuf)
    | lname -> UParser.LNAME (Sedlexing.Utf8.lexeme lexbuf)

    | eof -> UParser.EOF

    | _ ->
      let buffer = Buffer.create 16 in
      UParser.LNAME (ident buffer lexbuf)
  in
  if !jump_lexeme
  then pos.cnum <- pos.cnum + (String.length (Sedlexing.Utf8.lexeme lexbuf));
  token

let transform { lnum; bol; cnum } =
  let open Lexing in
  { pos_lnum = lnum;
    pos_bol = bol;
    pos_cnum = cnum;
    pos_fname = ""; }

let parse () =
  let pos = { lnum = 1; bol = 0; cnum = 0; } in
  ((fun lexbuf ->
    let token = token ~pos lexbuf in
    let old =
      { lnum = pos.lnum;
        bol = pos.bol;
        cnum = pos.cnum - (String.length (Sedlexing.Utf8.lexeme lexbuf)); } in
    (token, transform old, transform pos)),
   (fun lexbuf ->
    let old =
      { lnum = pos.lnum;
        bol = pos.bol;
        cnum = pos.cnum - (String.length (Sedlexing.Utf8.lexeme lexbuf)); } in
    transform old),
   (fun (_ : Sedlexing.lexbuf) -> transform pos))

let to_string token =
  let open UParser in match token with
  | LPAR -> "("
  | RPAR -> ")"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | COLON -> ":"
  | SEMICOLON -> ";"
  | PIPE -> "|"
  | COMMA -> ","
  | MARK -> "?"
  | EQUAL -> "="
  | POINT -> "."
  | WILDCARD -> "_"
  | BACKSLASH -> "\\"
  | ARROW -> "→"
  | SOME -> "∃"
  | FORALL -> "∀"
  | REC -> "Y"
  | LAMBDA -> "λ"
  | NULL -> "ø"
  | IN -> "in"
  | MATCH -> "match"
  | TYPE -> "type"
  | BOOL b -> if b then "true" else "false"
  | NUMBER i -> string_of_int i
  | CHAR c -> String.make 1 c
  | UNAME n -> n
  | LNAME n -> n
  | EOF -> "<eof>"
