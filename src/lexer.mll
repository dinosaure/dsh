{

exception Lexical_error

}

let name =
  ['_' 'A'-'Z' 'a'-'z' '=' '!' '<' '>' '+' '-' '*' '/' '%' '#' ',' '|']+
  ['_' 'A'-'Z' 'a'-'z' '0'-'9'] *
let digit = ['0'-'9']

rule token = parse
  | [' ' '\t' '\r' '\n']            { token lexbuf }
  | '('                             { Parser.LPAR }
  | ')'                             { Parser.RPAR }
  | '['                             { Parser.LBRA }
  | ']'                             { Parser.RBRA }
  | ':'                             { Parser.COMMA }
  | "define"                        { Parser.DEFINE }
  | "lambda"                        { Parser.LAMBDA }
  | "let"                           { Parser.LET }
  | "rec"                           { Parser.REC }
  | "forall"                        { Parser.FORALL }
  | "some"                          { Parser.SOME }
  | "->"                            { Parser.ARROW }
  | "true"                          { Parser.BOOL true }
  | "false"                         { Parser.BOOL false }
  | "if"                            { Parser.IF }
  | digit+ as n                     { Parser.NUMBER (int_of_string n) }
  | name as n                       { Parser.NAME n }
  | eof                             { Parser.EOF }
  | _                               { raise Lexical_error }

{

let string_of_token = function
  | Parser.LPAR -> "("
  | Parser.RPAR -> ")"
  | Parser.LBRA -> "["
  | Parser.RBRA -> "]"
  | Parser.COMMA -> ":"
  | Parser.DEFINE -> "define"
  | Parser.LET -> "let"
  | Parser.REC -> "rec"
  | Parser.LAMBDA -> "lambda"
  | Parser.FORALL -> "forall"
  | Parser.SOME -> "some"
  | Parser.ARROW -> "->"
  | Parser.IF -> "if"
  | Parser.NAME n -> n
  | Parser.NUMBER n -> string_of_int n
  | Parser.BOOL b -> if b then "true" else "false"
  | Parser.EOF -> "<EOF>"

}
