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
    | Set l ->
      Set (Type.Set.map aux l)
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

%token <string> CTOR
%token <string> NAME
%token <int> NUMBER
%token <bool> BOOL
%token <char> CHAR
%token MATCH LET LAMBDA FORALL SOME REC IF TYPE
%token LPAR RPAR LBRA RBRA
%token ARROW COMMA SEMICOLON PIPE COLON
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
  | LPAR TYPE n = NAME t = ty RPAR r = exprs
  { Ast.Alias (Location.make $startpos $endpos, n, t, r) }
  | LPAR LET n = NAME e = expr RPAR r = exprs
  { Ast.Let (Location.make $startpos $endpos($5), n, e, r) }
  | LPAR REC n = NAME e = expr RPAR r = exprs
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
  | LPAR LET LPAR n = NAME e = expr RPAR c = expr RPAR
  { Ast.Let (Location.make $startpos $endpos, n, e, c) }
  | LPAR REC LPAR n = NAME e = expr RPAR c = expr RPAR
  { Ast.Rec (Location.make $startpos $endpos, n, e, c) }
  | LPAR LAMBDA l = slist(param) e = expr RPAR
  { Ast.Abs (Location.make $startpos $endpos, l, e) }

  (** XXX: Conflict is here *)
  | LPAR c = CTOR e = expr RPAR
  { Ast.Variant (Location.make $startpos $endpos, c, e) }

  | LPAR l = ulist(COMMA, expr) RPAR
  { Ast.Tuple (Location.make $startpos $endpos, l) }
  | c = CTOR
  { let pos = Location.make $startpos $endpos in
    Ast.Variant (pos, c, Ast.Unit pos) }
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
  | a = expr COLON n = ann
  { Ast.Ann (Location.make $startpos $endpos, a, n) }
  | LBRA lst = alist(SEMICOLON, expr) RBRA
  { reduce lst $startpos $endpos }
  | i = NUMBER
  { Ast.Int (Location.make $startpos $endpos, i) }
  | b = BOOL
  { Ast.Bool (Location.make $startpos $endpos, b) }
  | c = CHAR
  { Ast.Char (Location.make $startpos $endpos, c) }
  | LPAR TYPE LPAR n = NAME t = ty RPAR e = expr RPAR
  { Ast.Alias (Location.make $startpos $endpos, n, t, e) }
  | LPAR RPAR
  { Ast.Unit (Location.make $startpos $endpos) }
  | LPAR MATCH e = expr l = branch+ RPAR
  { Ast.Match (Location.make $startpos $endpos, e, l) }

branch:
  | LPAR p = pattern e = expr RPAR
  { (p, e) }

pattern:
  | c = CTOR
  { let pos = Location.make $startpos $endpos in
    Pattern.Variant (pos, c, Pattern.Unit pos) }
  | LPAR c = CTOR e = pattern RPAR
  { Pattern.Variant (Location.make $startpos $endpos, c, e) }
  | n = NAME
  { Pattern.Var (Location.make $startpos $endpos, n) }
  | i = NUMBER
  { Pattern.Int (Location.make $startpos $endpos, i) }
  | b = BOOL
  { Pattern.Bool (Location.make $startpos $endpos, b) }
  | c = CHAR
  { Pattern.Char (Location.make $startpos $endpos, c) }
  | LPAR RPAR
  { Pattern.Unit (Location.make $startpos $endpos) }
  | LPAR l = ulist(COMMA, pattern) RPAR
  { Pattern.Tuple (Location.make $startpos $endpos, l) }
  | LPAR x = pattern RPAR
  { x }

param:
  | x = NAME
  { (x, None) }
  | x = NAME COLON n = ann
  { (x, Some n) }

ty_variant:
  | c = CTOR
  { (c, Type.unit) }
  | LPAR c = CTOR ty = ty RPAR
  { (c, ty) }

ty:
  | n = NAME
  { Type.Const n }
  | LPAR f = ty a = ty+ RPAR
  { Type.App (f, a) }
  | LPAR c = alist(ARROW, ty) RPAR
  { Type.Arrow c }
  | LPAR x = ty RPAR
  { x }
  | LPAR l = ulist(PIPE, ty_variant) RPAR
  { Type.Set (Type.Set.of_list l) }
  | LPAR FORALL l = slist(NAME) x = ty RPAR
  { let (ids, ty) = replace l x in
    match ids with
    | [] -> ty
    | _ -> Type.Forall (ids, ty) }
  | LPAR LAMBDA l = slist(NAME) x = ty RPAR
  { Type.Abs (l, x) }

ann:
  | x = ty
  { ([], x) }
  | LPAR SOME l = slist(NAME) x = ty RPAR
  { replace l x }
