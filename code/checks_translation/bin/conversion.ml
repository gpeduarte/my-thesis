open Ppxlib
open Parsetree
open Gospel
open Uast
open Location

let rec is_translatable (t: term) =
  match t.term_desc with
  | Ttrue -> true
  | Tfalse -> true
  | Tconst _ -> true
  | Tpreid _ -> true
  | Tidapp (_, tl) -> List.for_all is_translatable tl
  | Tfield (e, _) -> is_translatable e
  | Tapply (e, el) -> is_translatable e && List.for_all is_translatable el
  | Tinfix (, _, _) -> _
  | Tbinop (t1, _, t2) -> is_translatable t1 && is_translatable t2
  | Tnot _ -> true
  | Tif (e1, e2, e3) -> is_translatable e1 && is_translatable e2 && is_translatable e3
  | Tquant (_, _, _) -> false
  | Tattr (_, _) -> _
  | Tlet (_, vbl, expr) -> List.for_all is_translatable (s_value_binding vbl) && is_translatable expr
  | Tcase (_, _) -> _
  | Tcast (_, _) -> _
  | Ttuple t -> List.for_all is_translatable t
  | Trecord _ -> _
  | Tupdate (_, _) -> _
  | Tscope (_, _) -> _
  | Told e -> is_translatable e

let rec term_desc2expr (t: term_desc) =
  match t with
  | Ttrue -> _
  | Tfalse -> _
  | Tconst _ -> _
  | Tpreid x -> Sexp_ident x
  | Tidapp (_, _) -> _
  | Tfield (_, _) -> _
  | Tapply (_, _) -> _
  | Tinfix (_, _, _) -> _
  | Tbinop (_, _, _) -> _
  | Tnot _ -> _
  | Tif (_, _, _) -> _
  | Tquant (_, _, _) -> _
  | Tattr (_, _) -> _
  | Tlet (_, _, _) -> _
  | Tcase (_, _) -> _
  | Tcast (_, _) -> _
  | Ttuple tl -> Sexp_tuple (List.map term2expr tl)
  | Trecord _ -> _
  | Tupdate (_, _) -> _
  | Tscope (_, _) -> _
  | Told _ -> _

and term2expr (t: term) =
  if not (is_translatable t) then
    Location.raise_errorf ~loc:t.term_loc
      "Gospel: cannot translate term %a to an expression in line: %d"
      Print.term t t.term_loc.loc_start.line;
  {  spexp_desc = term_desc2expr t.term_desc;
     spexp_loc = t.term_loc;
     spexp_loc_stack = [];
     spexp_attributes = [];
  }

let rec s_expression_desc (expr: s_expression_desc) =
  match expr with
  | Sexp_while (e1, e2, None) -> Sexp_while (e1, e2, None)
  | Sexp_while (e1, e2, Some spec) ->
    let cl = List.map term2expr spec.loop_checks_invariant in
    let cl = List.map (fun e -> sexp_assert e) cl in
    let e2 = List.fold_left (fun e c -> sexp_sequence c e) e2 (List.rev cl) in
    let e2 = List.fold_left (fun e c -> sexp_sequence e c) e2 cl in
      Sexp_while (e1, e2, Some spec)
  | Sexp_for (_, e1, e2, df, e3, None) -> Sexp_for (_, e1, e2, df, e3, None)
  | Sexp_for (_, e1, e2, df, e3, Some spec) ->
    let cl = List.map term2expr spec.loop_checks_invariant in
    let cl = List.map (fun e -> sexp_assert e) cl in
    let e3 = List.fold_left (fun e c -> sexp_sequence c e) e3 (List.rev cl) in
    let e3 = List.fold_left (fun e c -> sexp_sequence e c) e3 cl in
      Sexp_for (_, e1, e2, df, e3, Some spec)
  | e -> e

let s_expression (expr: s_expression) =
  { expr with spexp_desc = s_expression_desc expr.spexp_desc }

let s_value_binding (vb: s_value_binding) =
  { vb with spvb_expr = s_expression vb.spvb_expr }

let s_structure_item_desc (s: s_structure_item_desc) =
  match s with
  | Str_value (r, vb) ->
      Str_value (r, List.map s_value_binding vb)
  | i -> i

let s_structure_item (s: s_structure_item) =
  { s with sstr_desc = s_structure_item_desc s.sstr_desc }

let s_structure (s: s_structure) =
  List.map s_structure_item s
