{

exception Lexical_error

}

let name =
  ['_' 'A'-'Z' 'a'-'z' '=' '!' '<' '>' '+' '-' '*' '/' '%' '#' ',' '|']+
  ['_' 'A'-'Z' 'a'-'z' '0'-'9'] *
let digit = ['0'-'9']+

rule token = parse
  | [' ' '\t' '\r' '\n']            { token lexbuf }
  | '('                             { Parser.LPAR }
  | ')'                             { Parser.RPAR }
  | '['                             { Parser.LBRA }
  | ']'                             { Parser.RBRA }
  | ':'                             { Parser.COMMA }
  | "lambda"                        { Parser.LAMBDA }
  | "let"                           { Parser.LET }
  | "forall"                        { Parser.FORALL }
  | "some"                          { Parser.SOME }
  | "->"                            { Parser.ARROW }
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
  | Parser.LET -> "let"
  | Parser.LAMBDA -> "lambda"
  | Parser.FORALL -> "forall"
  | Parser.SOME -> "some"
  | Parser.ARROW -> "->"
  | Parser.NAME n -> n
  | Parser.EOF -> "<EOF>"

}
