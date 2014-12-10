include AParser

let single_expr token lexbuf =
  let lexing () = token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised single_expr lexing

let single_ty token lexbuf =
  let lexing () = token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised single_ty lexing

let exprs token lexbuf =
  let lexing () = token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised exprs lexing
