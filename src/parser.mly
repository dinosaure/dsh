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
    | Variant row -> Variant (aux row)
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

let expr_record_extend lst rest =
  let map =
    List.fold_left
      (fun map (label, expr) ->
        let lst =
          try expr :: (Type.Set.find label map)
          with _ -> [expr]
        in Type.Set.add label lst map)
      Type.Set.empty lst
  in (map, rest)

let ty_row_extend lst rest =
  let map =
    List.fold_right
      (fun (label, ty) map ->
        let lst =
          try ty :: (Type.Set.find label map)
          with _ -> [ty]
        in Type.Set.add label lst map)
      lst Type.Set.empty
  in Type.RowExtend (map, rest)

%}

%token <string> UNAME
%token <string> LNAME

%token <int> NUMBER
%token <bool> BOOL
%token <char> CHAR

%token LPAR RPAR LBRACKET RBRACKET LBRACE RBRACE
%token SEMICOLON COLON PIPE COMMA EQUAL MARK POINT BACKSLASH
%token REC IN ARROW FORALL SOME LAMBDA NULL MATCH TYPE
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
  | n = LNAME EQUAL e = expr r = exprs
  { Ast.Let (Location.make $startpos $endpos(e), n, e, r) }
  | TYPE n = LNAME EQUAL t = ty r = exprs
  { Ast.Alias (Location.make $startpos $endpos, n, t, r) }
  | REC LPAR n = LNAME RPAR EQUAL e = expr r = exprs
  { Ast.Rec (Location.make $startpos $endpos(e), n, e, r) }
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
  | n = LNAME EQUAL e = expr IN c = expr
  { Ast.Let (Location.make $startpos $endpos, n, e, c) }
  | TYPE n = LNAME EQUAL t = ty IN c = expr
  { Ast.Alias (Location.make $startpos $endpos, n, t, c) }
  | REC LPAR n = LNAME RPAR EQUAL e = expr IN c = expr
  { Ast.Rec (Location.make $startpos $endpos, n, e, c) }
  | LAMBDA l = param+ POINT e = expr
  { Ast.Abs (Location.make $startpos $endpos, l, e) }
  | MATCH e = expr LBRACE PIPE? l = ulist(PIPE, branch) RBRACE
  { Ast.Case (Location.make $startpos $endpos, e, l) }
  | x = expr_atom
  { x }
  | x = expr_mark
  { x }
  | x = expr_atom COLON ty = ann
  { Ast.Ann (Location.make $startpos $endpos, x, ty) }

expr_atom:
  | i = NUMBER
  { Ast.Int (Location.make $startpos $endpos, i) }
  | b = BOOL
  { Ast.Bool (Location.make $startpos $endpos, b) }
  | c = CHAR
  { Ast.Char (Location.make $startpos $endpos, c) }
  | NULL
  { Ast.Unit (Location.make $startpos $endpos) }
  | n = LNAME
  { Ast.Var (Location.make $startpos $endpos, n) }
  | LPAR e = expr RPAR
  { e }
  | LBRACE RBRACE
  { Ast.RecordEmpty (Location.make $startpos $endpos) }
  | LBRACE l = expr_record PIPE r = expr RBRACE
  { let pos = Location.make $startpos $endpos in
    let map, rest = expr_record_extend l r in
    Ast.RecordExtend (pos, map, rest) }
  | LBRACE l = expr_record RBRACE
  { let pos = Location.make $startpos $endpos in
    let map, rest = expr_record_extend l (Ast.RecordEmpty pos) in
    Ast.RecordExtend (pos, map, rest) }
  | LBRACE x = expr BACKSLASH n = LNAME RBRACE
  { Ast.RecordRestrict (Location.make $startpos $endpos, x, n) }
  | x = expr_atom POINT n = LNAME
  { Ast.RecordSelect (Location.make $startpos $endpos, x, n) }
  | x = expr_app
  { x }
  | x = expr_variant
  { x }
  | LPAR l = expr_tuple RPAR
  { Ast.Tuple (Location.make $startpos $endpos, l) }
  | LPAR a = expr_infix RPAR
  { a }
  | LPAR s = expr_seq RPAR
  { s }

expr_variant:
  | n = UNAME LPAR e = expr RPAR
  { Ast.Variant (Location.make $startpos $endpos, n, e) }
  | n = UNAME LPAR RPAR
  { let pos = Location.make $startpos $endpos in
    Ast.Variant (pos, n, Ast.Unit pos)}
  | n = UNAME LPAR l = expr_tuple RPAR
  { let e = Ast.Tuple (Location.make $startpos(l) $endpos(l), l) in
    Ast.Variant (Location.make $startpos $endpos, n, e) }

expr_tuple:
  | x = expr_atom COMMA r = expr_tuple
  { x :: r }
  | x = expr_atom COMMA r = expr_atom
  { x :: [ r ] }

expr_infix:
  | a = expr_atom o = LNAME b = expr_infix
  { let op = Ast.Var (Location.make $startpos(o) $endpos(o), o) in
    Ast.App (Location.make $startpos $endpos, op, [a; b]) }
  | a = expr_atom o = LNAME b = expr_atom
  { let op = Ast.Var (Location.make $startpos(o) $endpos(o), o) in
    Ast.App (Location.make $startpos $endpos, op, [a; b]) }

expr_seq:
  | a = expr_atom SEMICOLON b = expr_seq
  { Ast.Seq (Location.make $startpos $endpos, a, b) }
  | a = expr_atom SEMICOLON b = expr_atom
  { Ast.Seq (Location.make $startpos $endpos, a, b) }

expr_record:
  | n = LNAME EQUAL e = expr
  { [(n, e)] }
  | l = expr_record COMMA n = LNAME EQUAL e = expr
  { (n, e) :: l }

expr_app:
  | f = expr_atom LBRACKET a = expr_arg RBRACKET
  { Ast.App (Location.make $startpos $endpos, f, a) }
  | f = expr_atom LBRACKET RBRACKET
  { Ast.App (Location.make $startpos $endpos, f, []) }

expr_arg:
  | x = expr
  { [ x ] }
  | x = expr COMMA r = expr_arg
  { x :: r }

expr_mark:
  | i = expr_atom MARK a = expr PIPE b = expr
  { Ast.If (Location.make $startpos $endpos, i, a, b) }

branch:
  | p = pattern ARROW e = expr
  { (p, e) }

pattern:
  | n = UNAME LPAR RPAR
  { let pos = Location.make $startpos $endpos in
    Pattern.Variant (pos, n, Pattern.Unit pos) }
  | n = UNAME LPAR p = pattern RPAR
  { Pattern.Variant (Location.make $startpos $endpos, n, p) }
  | n = UNAME LPAR l = pattern_tuple RPAR
  { let p = Pattern.Tuple (Location.make $startpos(l) $endpos(l), l) in
    Pattern.Variant (Location.make $startpos $endpos, n, p) }
  | n = LNAME
  { Pattern.Var (Location.make $startpos $endpos, n) }
  | i = NUMBER
  { Pattern.Int (Location.make $startpos $endpos, i) }
  | b = BOOL
  { Pattern.Bool (Location.make $startpos $endpos, b) }
  | c = CHAR
  { Pattern.Char (Location.make $startpos $endpos, c) }
  | NULL
  { Pattern.Unit (Location.make $startpos $endpos) }
  | LPAR x = pattern RPAR
  { x }
  | LPAR l = pattern_tuple RPAR
  { Pattern.Tuple (Location.make $startpos $endpos, l) }

pattern_tuple:
  | x = pattern COMMA r = pattern
  { x :: [ r ] }
  | x = pattern COMMA r = pattern_tuple
  { x :: r }

param:
  | x = LNAME
  { (x, None) }
  | x = LNAME COLON n = ann
  { (x, Some n) }

ty:
  | x = ty_atom
  { x }
  | l = ty_arrow ARROW r = ty_atom
  { Type.Arrow (l, r) }
  | x = ty_forall
  { x }
  | LAMBDA l = LNAME+ POINT t = ty
  { Type.Abs (l, t) }

ty_forall:
  | FORALL l = LNAME+ POINT x = ty
  { let (ids, ty) = replace l x in
    match ids with
    | [] -> ty
    | _ -> Type.Forall (ids, ty) }

ty_atom:
  | n = LNAME
  { Type.Const n }
  | f = ty_atom LBRACKET a = ty_arg RBRACKET
  { Type.App (f, a) }
  | LPAR x = ty RPAR
  { x }
  | LBRACE x = LNAME RBRACE
  { Type.Record (Type.Const x) }
  | LBRACE RBRACE
  { Type.Record Type.RowEmpty }
  | LBRACE l = ty_record PIPE r = ty RBRACE
  { Type.Record (ty_row_extend l r) }
  | LBRACE l = ty_record RBRACE
  { Type.Record (ty_row_extend l Type.RowEmpty) }
  | LBRACKET x = UNAME RBRACKET
  { Type.Variant (Type.Const x) }
  | LBRACKET l = ty_variant PIPE r = ty RBRACKET
  { Type.Variant (ty_row_extend l r) }
  | LBRACKET l = ty_variant RBRACKET
  { Type.Variant (ty_row_extend l Type.RowEmpty) }
  | LBRACKET RBRACKET
  { Type.Variant Type.RowEmpty }

ty_arrow:
  | x = ty_atom
  { [ x ] }
  | l = ty_arrow ARROW r = ty_atom
  { l @ [ r ] }

ty_record:
  | x = LNAME COLON ty = ty
  { [(x, ty)] }
  | l = ty_record COMMA x = LNAME COLON ty = ty
  { (x, ty) :: l }

ty_variant:
  | x = UNAME COLON ty = ty
  { [(x, ty)] }
  | l = ty_record COMMA x = UNAME COLON ty = ty
  { (x, ty) :: l }

ty_arg:
  | x = ty
  { [ x ] }
  | x = ty COMMA r = ty_arg
  { x :: r }

ann:
  | x = ty
  { ([], x) }
  | SOME l = LNAME+ POINT x = ty
  { replace l x }
