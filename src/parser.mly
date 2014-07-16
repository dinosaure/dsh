%{

(** replace : turns type constants `a` and `b` into `Bound` type variable
 * if they are bound by `Forall` or `Some`.
 *
 * @param ids list of constants
 * @param ty type contains constants
 * @return list of `Bound` type variable and type
*)

let replace ids ty =
  let open Type in
  let map = Hashtbl.create 16 in
  let lst = ref [] in
  List.iter (fun name -> Hashtbl.replace map name None) ids;
  let rec aux = function
    | Const name as ty ->
      begin
        try match Hashtbl.find map name with
          | Some var -> var
          | None ->
            let (id, var) = Synthesis.Variable.bound () in
            lst := id :: !lst;
            Hashtbl.replace map name (Some var);
            var
        with Not_found -> ty
      end
    | Var _ as ty -> ty
    | App (f, a) ->
      let f' = aux f in
      let a' = List.map aux a in
      App (f', a')
      (** App (aux f, List.map aux a)
       * This does not respect the order of appearance of the variables *)
    | Arrow (a, r) ->
      let a' = List.map aux a in
      let r' = aux r in
      Arrow (a', r')
      (** see App *)
    | Forall (ids, ty) ->
      Forall (ids, aux ty)
  in (List.rev !lst, aux ty)

%}

%token <string> NAME
%token <int> NUMBER
%token <bool> BOOL
%token DEFINE LET LAMBDA FORALL SOME REC IF
%token LPAR RPAR LBRA RBRA
%token ARROW COMMA
%token EOF

%start single_expr
%type <Ast.t> single_expr

%start single_ty
%type <Type.t> single_ty

%start exprs
%type <Ast.i list> exprs

%%

slist(X):
  | LPAR l = X+ RPAR
  { l }

alist(C, X):
  | a = X C r = alist(C, X)
  { r |> fun (x, r) -> (a :: x, r) }
  | x = X C y = X
  { ([ x ], y) }

exprs:
  | LPAR DEFINE n = NAME e = expr RPAR r = exprs
  { Ast.Def (Location.make $startpos $endpos($5), n, e) :: r }
  | e = expr r = exprs
  { Ast.Expr (Location.make $startpos $endpos(e), e) :: r }
  | EOF
  { [] }

single_expr:
  | a = expr EOF
  { a }

single_ty:
  | a = ty EOF
  { a }

expr:
  | LPAR LET LPAR n = NAME e = expr RPAR c = expr RPAR
  { Ast.Let (Location.make $startpos $endpos, n, e, c) }
  | LPAR REC LPAR n = NAME e = expr RPAR c = expr RPAR
  { Ast.Rec (Location.make $startpos $endpos, n, e, c) }
  | LPAR LAMBDA l = slist(param) e = expr RPAR
  { Ast.Abs (Location.make $startpos $endpos, l, e) }
  | n = NAME
  { Ast.Var (Location.make $startpos $endpos, n) }
  | LPAR e = expr RPAR
  { e }
  | LPAR f = expr a = expr+ RPAR
  { Ast.App (Location.make $startpos $endpos, f, a) }
  | LBRA a = expr o = expr b = expr RBRA
  { Ast.App (Location.make $startpos $endpos, o, [a; b]) }
  | LPAR IF i = expr a = expr b = expr RPAR
  { Ast.If (Location.make $startpos $endpos, i, a, b) }
  | a = expr COMMA n = ann
  { Ast.Ann (Location.make $startpos $endpos, a, n) }
  | i = NUMBER
  { Ast.Int (Location.make $startpos $endpos, i) }
  | b = BOOL
  { Ast.Bool (Location.make $startpos $endpos, b) }

param:
  | x = NAME
  { (x, None) }
  | x = NAME COMMA n = ann
  { (x, Some n) }

ty:
  | n = NAME
  { Type.Const n }
  | LPAR f = ty a = ty+ RPAR
  { Type.App (f, a) }
  | LPAR c = alist(ARROW, ty) RPAR
  { Type.Arrow c }
  | LPAR x = ty RPAR
  { x }
  | LPAR FORALL l = slist(NAME) x = ty RPAR
  { let (ids, ty) = replace l x in
    match ids with
    | [] -> ty
    | _ -> Type.Forall (ids, ty) }

ann:
  | x = ty
  { ([], x) }
  | LPAR SOME l = slist(NAME) x = ty RPAR
  { replace l x }
