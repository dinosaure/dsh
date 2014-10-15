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
            let (id, var) = Type.Variable.bound () in
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
    | Abs (ids, ty) ->
      Abs (ids, aux ty)
    | Alias _ -> assert false (** It's premature *)
    | Set row -> Set (aux row)
    | Record row -> Record (aux row)
    | RowEmpty -> RowEmpty
    | RowExtend (map, row) ->
      RowExtend (Set.map (List.map aux) map, aux row)
  in (List.rev !lst, aux ty)

let reduce (lst, stop) startpos endpos =
  let rec aux = function
    | [] -> stop
    | [ x ] ->
      Ast.Seq (Ast.loc x, x, stop)
    | x :: r ->
      let start = Ast.loc x |> Location.start in
      let stop = Ast.loc (List.rev r |> List.hd) |> Location.stop in
      Ast.Seq (Location.compose start stop, x, aux r)
  in aux lst

%}

%token <string> UNAME
%token <string> LNAME

%token <int> NUMBER
%token <bool> BOOL
%token <char> CHAR

%token LPAR RPAR LBRA RBRA LACC RACC

%token COLON SEMICOLON PIPE COMMA EQUAL POINT BACKSLASH MARK DASH PLUS STAR
%token PERCENT SLASH UPPER LOWER TILDE DIFF EQUP EQLO

%token REC FORALL SOME

%token EOF

%start single_expr
%type <Ast.t> single_expr

%start single_ty
%type <Type.t> single_ty

%start exprs
%type <Ast.t> exprs

%%

slist(X):
  | LPAR l = X+ RPAR
  { l }

alist(C, X):
  | a = X C r = alist(C, X)
  { r |> fun (x, r) -> (a :: x, r) }
  | x = X C y = X
  { ([ x ], y) }

ulist(C, X):
  | x = X C r = ulist(C, X)
  { x :: r }
  | x = X
  { [ x ] }

exprs:
  | LPAR POINT  n = LNAME t = ty RPAR   r = exprs
  { Ast.Alias (Location.make $startpos $endpos, n, t, r) }

  | LPAR COLON  n = LNAME e = expr RPAR r = exprs
  { Ast.Let (Location.make $startpos $endpos($5), n, e, r) }

  | LPAR REC    n = LNAME e = expr RPAR r = exprs
  { Ast.Rec (Location.make $startpos $endpos($5), n, e, r) }

  | e = expr r = exprs
  { Ast.Seq (Location.make $startpos $endpos(e), e, r) }

  | EOF
  { Ast.Unit (Location.make $startpos $endpos) }

single_expr:
  | a = expr EOF
  { a }

single_ty:
  | a = ty EOF
  { a }

expr:
  | LPAR POINT        LPAR n = LNAME t = ty RPAR    e = expr RPAR
  { Ast.Alias (Location.make $startpos $endpos, n, t, e) }
  | LPAR COLON        LPAR n = LNAME e = expr RPAR  c = expr RPAR
  { Ast.Let (Location.make $startpos $endpos, n, e, c) }
  | LPAR REC          LPAR n = LNAME e = expr RPAR  c = expr RPAR
  { Ast.Rec (Location.make $startpos $endpos, n, e, c) }
  | LPAR BACKSLASH    l = slist(param)              e = expr RPAR
  { Ast.Abs (Location.make $startpos $endpos, l, e) }
  | LPAR PIPE         e = expr                      l = branch+ RPAR
  { Ast.Match (Location.make $startpos $endpos, e, l) }
  | LPAR MARK         i = expr                      a = expr b = expr RPAR
  { Ast.If (Location.make $startpos $endpos, i, a, b) }

  | x = expr_infix { x }
  | x = expr_atom { x }
  | x = expr_app { x }
  | x = expr_seq { x }
  | x = expr_ann { x }

  (** XXX: Conflict is here *)
  | LPAR TILDE c = UNAME e = expr RPAR
  { Ast.Variant (Location.make $startpos $endpos, c, e) }

expr_ann:
  | a = expr COLON n = ann
  { Ast.Ann (Location.make $startpos $endpos, a, n) }

expr_infix:
  | LBRA a = expr o = expr b = expr RBRA
  { Ast.App (Location.make $startpos $endpos, o, [a; b]) }

expr_seq:
  | LBRA lst = alist(SEMICOLON, expr) RBRA
  { reduce lst $startpos $endpos }

expr_app:
  | LPAR f = expr a = expr+ RPAR
  { Ast.App (Location.make $startpos $endpos, f, a) }
  | LPAR COMMA l = expr+ RPAR
  { Ast.Tuple (Location.make $startpos $endpos, l) }

expr_atom:
  | i = NUMBER
  { Ast.Int (Location.make $startpos $endpos, i) }
  | b = BOOL
  { Ast.Bool (Location.make $startpos $endpos, b) }
  | c = CHAR
  { Ast.Char (Location.make $startpos $endpos, c) }
  | LPAR RPAR
  { Ast.Unit (Location.make $startpos $endpos) }
  | c = UNAME
  { let pos = Location.make $startpos $endpos in
    Ast.Variant (pos, c, Ast.Unit pos) }
  | n = LNAME
  { Ast.Var (Location.make $startpos $endpos, n) }
  | LPAR e = expr RPAR
  { e }
  | PLUS
  { Ast.Var (Location.make $startpos $endpos, "+") }
  | DASH
  { Ast.Var (Location.make $startpos $endpos, "-") }
  | STAR
  { Ast.Var (Location.make $startpos $endpos, "*") }
  | SLASH
  { Ast.Var (Location.make $startpos $endpos, "/") }
  | PERCENT
  { Ast.Var (Location.make $startpos $endpos, "%") }
  | UPPER
  { Ast.Var (Location.make $startpos $endpos, ">") }
  | LOWER
  { Ast.Var (Location.make $startpos $endpos, "<") }
  | EQUAL
  { Ast.Var (Location.make $startpos $endpos, "=") }
  | DIFF
  { Ast.Var (Location.make $startpos $endpos, "<>") }
  | EQUP
  { Ast.Var (Location.make $startpos $endpos, ">=") }
  | EQLO
  { Ast.Var (Location.make $startpos $endpos, "<=") }

branch:
  | LPAR p = pattern e = expr RPAR
  { (p, e) }

pattern:
  | c = UNAME
  { let pos = Location.make $startpos $endpos in
    Pattern.Variant (pos, c, Pattern.Unit pos) }
  | LPAR c = UNAME e = pattern RPAR
  { Pattern.Variant (Location.make $startpos $endpos, c, e) }
  | n = LNAME
  { Pattern.Var (Location.make $startpos $endpos, n) }
  | i = NUMBER
  { Pattern.Int (Location.make $startpos $endpos, i) }
  | b = BOOL
  { Pattern.Bool (Location.make $startpos $endpos, b) }
  | c = CHAR
  { Pattern.Char (Location.make $startpos $endpos, c) }
  | LPAR RPAR
  { Pattern.Unit (Location.make $startpos $endpos) }
  | LPAR COMMA l = pattern+ RPAR
  { Pattern.Tuple (Location.make $startpos $endpos, l) }
  | LPAR x = pattern RPAR
  { x }

param:
  | x = LNAME
  { (x, None) }
  | x = LNAME COLON n = ann
  { (x, Some n) }

ty:
  | n = LNAME
  { Type.Const n }
  | LPAR f = ty a = ty+ RPAR
  { Type.App (f, a) }
  | LPAR c = alist(UPPER, ty) RPAR
  { Type.Arrow c }
  | LPAR x = ty RPAR
  { x }
  | LPAR STAR l = ty+ RPAR
  { Type.App (Type.Const "*", l) }
  (*
  | LPAR l = ulist(PIPE, ty_variant) RPAR
  { Type.Set (Type.Set.of_list l) }
  *)
  | LPAR FORALL     l = slist(LNAME) x = ty RPAR
  { let (ids, ty) = replace l x in
    match ids with
    | [] -> ty
    | _ -> Type.Forall (ids, ty) }
  | LPAR BACKSLASH  l = slist(LNAME) x = ty RPAR
  { Type.Abs (l, x) }

ann:
  | x = ty
  { ([], x) }
  | LPAR SOME l = slist(LNAME) x = ty RPAR
  { replace l x }
