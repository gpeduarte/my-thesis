open Ppxlib
(* open Parsetree *)
open Gospel
open Uast
open Location

exception Translation_error of Location.t

let () =
  let open Location.Error in
  register_error_of_exn (function
    | Translation_error loc ->
        Fmt.kstr (fun str -> Some (make ~loc ~sub:[] str))
          "translation error"
    | _ -> None)

let qualid2longident (x: qualid) =
  match x with
  | Qpreid id -> { loc = id.pid_loc; txt = Lident id.pid_str }
  | Qdot (_, _) -> assert false (* TODO *)

let mk_id x =
  match x with
  | Qpreid id ->
    { spexp_desc = Sexp_ident (qualid2longident x);
      spexp_loc = id.pid_loc;
      spexp_loc_stack = [];
      spexp_attributes = []; }
  | Qdot (_, _) -> assert false (* TODO *)

let rec is_translatable (t: term) =
  match t.term_desc with
  | Ttrue -> true
  | Tfalse -> true
  | Tconst _ -> true
  | Tpreid _ -> true
  | Tidapp (_, tl) -> List.for_all is_translatable tl
  | Tfield (e, _) -> is_translatable e
  | Tapply (e1, e2) -> is_translatable e1 && is_translatable e2
  | Tinfix (_, _, _) -> assert false (* TODO *)
  | Tbinop (t1, _, t2) -> is_translatable t1 && is_translatable t2
  | Tnot _ -> true
  | Tif (e1, e2, e3) -> is_translatable e1 && is_translatable e2 && is_translatable e3
  | Tquant (_, _, _) -> false
  | Tattr (_, _) -> assert false (* TODO *)
  | Tlet (_, vbl, expr) -> is_translatable vbl && is_translatable expr
  | Tcase (_, _) -> assert false (* TODO *)
  | Tcast (_, _) -> assert false (* TODO *)
  | Ttuple t -> List.for_all is_translatable t
  | Trecord _ -> assert false (* TODO *)
  | Tupdate (_, _) -> assert false (* TODO *)
  | Tscope (_, _) -> assert false (* TODO *)
  | Told e -> is_translatable e

and term_desc2expr (t: term_desc) =
  let no_label l = List.map (fun e -> (Nolabel, e)) l in
  match t with
  | Ttrue -> assert false (* TODO *)
  | Tfalse -> assert false (* TODO *)
  | Tconst _ -> assert false (* TODO *)
  | Tpreid x -> Sexp_ident (qualid2longident x)
  | Tidapp (x, tl) -> Sexp_apply (mk_id x, no_label (List.map term2expr tl))
  | Tfield (_, _) -> assert false (* TODO *)
  | Tapply (_, _) -> assert false (* TODO *)
  | Tinfix (_, _, _) -> assert false (* TODO *)
  | Tbinop (_, _, _) -> assert false (* TODO *)
  | Tnot _ -> assert false (* TODO *)
  | Tif (_, _, _) -> assert false (* TODO *)
  | Tquant (_, _, _) -> assert false (* TODO *)
  | Tattr (_, _) -> assert false (* TODO *)
  | Tlet (_, _, _) -> assert false (* TODO *)
  | Tcase (_, _) -> assert false (* TODO *)
  | Tcast (_, _) -> assert false (* TODO *)
  | Ttuple tl -> Sexp_tuple (List.map term2expr tl)
  | Trecord _ -> assert false (* TODO *)
  | Tupdate (_, _) -> assert false (* TODO *)
  | Tscope (_, _) -> assert false (* TODO *)
  | Told _ -> assert false (* TODO *)

and term2expr (t: term) =
  if not (is_translatable t) then
    raise (Translation_error t.term_loc);
  { spexp_desc = term_desc2expr t.term_desc;
    spexp_loc = t.term_loc;
    spexp_loc_stack = [];
    spexp_attributes = []; }

and s_expression_desc (expr: s_expression_desc) =
  let exp2assert e = { e with spexp_desc = Sexp_assert e } in
  let exp2seq e c = { e with spexp_desc = Sexp_sequence (c, e) } in
  let exp2seq_rev e c = { e with spexp_desc = Sexp_sequence (e, c) } in
  match expr with
  | Sexp_while (e1, e2, None) -> Sexp_while (e1, e2, None)
  | Sexp_while (e1, e2, Some spec) ->
    let cl = List.map term2expr spec.loop_checks_invariant in
    let cl = List.map exp2assert cl in
    let e2 = List.fold_left exp2seq e2 (List.rev cl) in
    let e2 = List.fold_left exp2seq_rev e2 cl in
    Sexp_while (e1, e2, Some spec)
  | Sexp_for (p, e1, e2, df, e3, None) -> Sexp_for (p, e1, e2, df, e3, None)
  | Sexp_for (p, e1, e2, df, e3, Some spec) ->
    let cl = List.map term2expr spec.loop_checks_invariant in
    let cl = List.map exp2assert cl in
    let e3 = List.fold_left exp2seq e3 (List.rev cl) in
    let e3 = List.fold_left exp2seq_rev e3 cl in
    Sexp_for (p, e1, e2, df, e3, Some spec)
  | e -> e

and s_expression (expr: s_expression) =
  { expr with spexp_desc = s_expression_desc expr.spexp_desc }

and s_value_binding (vb: s_value_binding) : s_value_binding =
  { vb with spvb_expr = s_expression vb.spvb_expr }

and s_structure_item_desc (s: s_structure_item_desc) =
  match s with
  | Str_value (r, vb) ->
    Str_value (r, List.map s_value_binding vb)
  | i -> i

and s_structure_item (s: s_structure_item) =
  { s with sstr_desc = s_structure_item_desc s.sstr_desc }

let s_structure (s: s_structure) =
  List.map s_structure_item s
