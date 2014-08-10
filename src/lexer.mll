{

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

}

let name =
  ['_' 'A'-'Z' 'a'-'z' '=' '!' '<' '>' '+' '-' '*' '/' '%' '#' '|']+
  ['_' 'A'-'Z' 'a'-'z' '0'-'9'] * ['\''] * | [ ',' ]
let ctor =
  ['A'-'Z']+ ['_' 'A'-'Z' 'a'-'z' '0'-'9'] * ['\''] *
let digit = ['0'-'9']
let character = ['a'-'z' 'A'-'Z']
let special = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']

rule token = parse
  | [' ' '\t' '\r']                       { token lexbuf }
  | '\n'                                  { Lexing.new_line lexbuf;
                                            token lexbuf }
  | '('                                   { Parser.LPAR }
  | ')'                                   { Parser.RPAR }
  | '['                                   { Parser.LBRA }
  | ']'                                   { Parser.RBRA }
  | ':'                                   { Parser.COMMA }
  | ';'                                   { Parser.SEMICOLON }
  | '|'                                   { Parser.PIPE }
  | "lambda"                              { Parser.LAMBDA }
  | "let"                                 { Parser.LET }
  | "rec"                                 { Parser.REC }
  | "forall"                              { Parser.FORALL }
  | "some"                                { Parser.SOME }
  | "->"                                  { Parser.ARROW }
  | "true"                                { Parser.BOOL true }
  | "false"                               { Parser.BOOL false }
  | "if"                                  { Parser.IF }
  | "type"                                { Parser.TYPE }
  | "'" '\\' (hexa as a) (hexa as b) "'"  { Parser.CHAR (char_of_hexa a b) }
  | "'" '\\' (special as c) "'"           { Parser.CHAR (char_of_backslash c) }
  | "'" ([^ '\\'] as c) "'"               { Parser.CHAR c }
  | digit+ as n                           { Parser.NUMBER (int_of_string n) }
  | ctor as n                             { Parser.CTOR n }
  | name as n                             { Parser.NAME n }
  | eof                                   { Parser.EOF }
  | _                                     { raise Lexical_error }

{

let string_of_token = function
  | Parser.LPAR -> "("
  | Parser.RPAR -> ")"
  | Parser.LBRA -> "["
  | Parser.RBRA -> "]"
  | Parser.COMMA -> ":"
  | Parser.SEMICOLON -> ";"
  | Parser.PIPE -> "|"
  | Parser.LET -> "let"
  | Parser.REC -> "rec"
  | Parser.LAMBDA -> "lambda"
  | Parser.FORALL -> "forall"
  | Parser.SOME -> "some"
  | Parser.ARROW -> "->"
  | Parser.IF -> "if"
  | Parser.TYPE -> "type"
  | Parser.CTOR n -> n
  | Parser.NAME n -> n
  | Parser.NUMBER n -> string_of_int n
  | Parser.BOOL b -> if b then "true" else "false"
  | Parser.CHAR c -> String.make 1 c
  | Parser.EOF -> "<EOF>"

}
