%{

let replace lst ty =
  let open Type in
  let env = List.fold_left
    (fun env name -> W.Environment.extend env name (W.Variable.generic ()))
    W.Environment.empty lst
  in
  let rec aux = function
    | Const name as ty ->
      begin
        try W.Environment.lookup env name
        with Not_found -> ty
      end
    | Var _ as ty -> ty
    | App (f, a) ->
      App (aux f, List.map aux a)
    | Arrow (a, r) ->
      Arrow (List.map aux a, aux r)
  in aux ty

%}

%token <string> NAME
%token LET LAMBDA FORALL
%token LPAR RPAR LBRA RBRA
%token ARROW
%token EOF

%start single_expr
%type <Ast.t> single_expr

%start single_ty
%type <Type.t> single_ty

%start single_forall
%type <Type.t> single_forall

%%

slist(X):
  | LPAR l = X+ RPAR
  { l }

alist(C, X):
  | a = X C r = alist(C, X)
  { r |> fun (x, r) -> (a :: x, r) }
  | x = X C y = X
  { ([ x ], y) }

single_expr:
  | a = expr EOF
  { a }

single_ty:
  | a = ty EOF
  { a }

single_forall:
  | a = forall EOF
  { a }

expr:
  | LPAR LET LPAR n = NAME e = expr RPAR c = expr RPAR
  { Ast.Let (Location.make $startpos $endpos, n, e, c) }
  | LPAR LAMBDA l = slist(NAME) e = expr RPAR
  { Ast.Abs (Location.make $startpos $endpos, l, e) }
  | n = NAME
  { Ast.Var (Location.make $startpos $endpos, n) }
  | LPAR e = expr RPAR
  { e }
  | LPAR f = expr a = expr+ RPAR
  { Ast.App (Location.make $startpos $endpos, f, a) }
  | LBRA a = expr o = expr b = expr RBRA
  { Ast.App (Location.make $startpos $endpos, o, [a; b]) }

ty:
  | n = NAME
  { Type.Const n }
  | LPAR f = ty a = ty+ RPAR
  { Type.App (f, a) }
  | LPAR c = alist(ARROW, ty) RPAR
  { Type.Arrow c }
  | LPAR x = ty RPAR
  { x }

forall:
  | LPAR FORALL l = slist(NAME) x = ty RPAR
  { replace l x }
  | x = ty
  { x }
