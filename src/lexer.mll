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

let name = ['_' 'A'-'Z' 'a'-'z' '0'-'9']
let uname = ['A'-'Z'] name *
let lname = ['a'-'z'] name *

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
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
  | '{'                                   { Parser.LACC }
  | '}'                                   { Parser.RACC }
  | ':'                                   { Parser.COLON }
  | ';'                                   { Parser.SEMICOLON }
  | '|'                                   { Parser.PIPE }
  | ','                                   { Parser.COMMA }
  | '='                                   { Parser.EQUAL }
  | '.'                                   { Parser.POINT }
  | '-'                                   { Parser.DASH }
  | '?'                                   { Parser.MARK }
  | '~'                                   { Parser.TILDE }
  | '+'                                   { Parser.PLUS }
  | '*'                                   { Parser.STAR }
  | '%'                                   { Parser.PERCENT }
  | '>'                                   { Parser.UPPER }
  | '<'                                   { Parser.LOWER }
  | '/'                                   { Parser.SLASH }
  | '\\'                                  { Parser.BACKSLASH }

  | "Y"                                   { Parser.REC }
  | "V"                                   { Parser.FORALL }
  | "E"                                   { Parser.SOME }

  | "true"                                { Parser.BOOL true }
  | "false"                               { Parser.BOOL false }

  | "'" '\\' (hexa as a) (hexa as b) "'"  { Parser.CHAR (char_of_hexa a b) }
  | "'" '\\' (special as c) "'"           { Parser.CHAR (char_of_backslash c) }
  | "'" ([^ '\\'] as c) "'"               { Parser.CHAR c }
  | digit+ as n                           { Parser.NUMBER (int_of_string n) }

  | lname as n                            { Parser.LNAME n }
  | uname as n                            { Parser.UNAME n }

  | eof                                   { Parser.EOF }
  | _                                     { raise Lexical_error }

{

let string_of_token = function
  | Parser.LPAR -> "("
  | Parser.RPAR -> ")"
  | Parser.LBRA -> "["
  | Parser.RBRA -> "]"
  | Parser.COLON -> ":"
  | Parser.SEMICOLON -> ";"
  | Parser.PIPE -> "|"
  | Parser.COMMA -> ","
  | Parser.LACC -> "{"
  | Parser.RACC -> "}"
  | Parser.EQUAL -> "="
  | Parser.POINT -> "."
  | Parser.DASH -> "-"
  | Parser.MARK -> "?"
  | Parser.TILDE -> "~"
  | Parser.PLUS -> "+"
  | Parser.STAR -> "*"
  | Parser.PERCENT -> "%"
  | Parser.UPPER -> ">"
  | Parser.LOWER -> "<"
  | Parser.SLASH -> "/"
  | Parser.BACKSLASH -> "\\"
  | Parser.REC -> "Y"
  | Parser.FORALL -> "V"
  | Parser.SOME -> "E"
  | Parser.UNAME n -> n
  | Parser.LNAME n -> n
  | Parser.NUMBER n -> string_of_int n
  | Parser.BOOL b -> if b then "true" else "false"
  | Parser.CHAR c -> String.make 1 c
  | Parser.EOF -> "<EOF>"

}
