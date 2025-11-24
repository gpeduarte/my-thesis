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

let mk_id_from_str str = 
  { spexp_desc = Sexp_ident { loc = Location.none; txt = Lident str };
    spexp_loc = Location.none;
    spexp_loc_stack = [];
    spexp_attributes = []; }

let mk_string_loc Preid.{ pid_str; pid_loc; _ } =
  { loc = pid_loc; txt = pid_str }

let rec is_translatable (t: term) =
  match t.term_desc with
  | Ttrue -> true
  | Tfalse -> true
  | Tconst _ -> true
  | Tpreid _ -> true
  | Tidapp (_, tl) -> List.for_all is_translatable tl
  | Tfield (e, _) -> is_translatable e
  | Tapply (e1, e2) -> is_translatable e1 && is_translatable e2
  | Tinfix (e1, _, e2) -> is_translatable e1 && is_translatable e2
  | Tbinop (t1, _, t2) -> is_translatable t1 && is_translatable t2
  | Tnot _ -> true
  | Tif (e1, e2, e3) -> is_translatable e1 && is_translatable e2 && is_translatable e3
  | Tquant (_, _, _) -> false
  | Tattr (_, e) -> is_translatable e
  | Tlet (_, vbl, expr) -> is_translatable vbl && is_translatable expr
  | Tcase (e, el) -> is_translatable e && List.for_all (fun (_, t) -> is_translatable t) el
  | Tcast (e, _) -> is_translatable e
  | Ttuple t -> List.for_all is_translatable t
  | Trecord al -> List.for_all (fun (_, v) -> is_translatable v) al
  | Tupdate (r, aul) -> is_translatable r && List.for_all (fun (_, v) -> is_translatable v) aul
  | Tscope (_, e) -> is_translatable e
  | Told e -> is_translatable e

and term2expr (t: term) =
  if not (is_translatable t) then
    raise (Translation_error t.term_loc);
  let no_label l = List.map (fun e -> (Nolabel, e)) l in
  let mk_pattern (x: Preid.t) = {
    ppat_desc = Ppat_var (mk_string_loc x);
    ppat_loc = x.pid_loc;
    ppat_loc_stack = [];
    ppat_attributes = []
  } in
  let mk_s_value_binding x t = {
    spvb_pat = mk_pattern x;
    spvb_expr = term2expr t;
    spvb_attributes = [];
    spvb_vspec = None;
    spvb_loc = t.term_loc;
  } in
  let mk_longident_loc x = { txt = Lident x; loc = t.term_loc } in
  let binop_to_str x = 
    match x with
    | Tand -> "&&"
    | Tand_asym -> "&&"
    | Tor -> "||"
    | Tor_asym -> "||"
    | _ -> assert false
  in
  let term_desc2expr (term_desc: term_desc) =
    match term_desc with
    | Ttrue -> Sexp_construct (mk_longident_loc "true" , None)
    | Tfalse -> Sexp_construct (mk_longident_loc "false" , None)
    | Tconst c -> Sexp_constant (term2expr c)
    | Tpreid x -> Sexp_ident (qualid2longident x)
    | Tidapp (x, tl) -> Sexp_apply (mk_id x, no_label (List.map term2expr tl))
    | Tfield (t, q) -> Sexp_field (term2expr t, qualid2longident q)
    | Tapply (e1, expl) -> Sexp_apply (term2expr e1, no_label (List.map term2expr expl))
    | Tinfix (t1, op, t2) -> Sexp_apply (mk_id op, no_label [term2expr t1; term2expr t2])
    | Tbinop (t1, op, t2) -> Sexp_apply (binop_to_str op, no_label [term2expr t1 op term2expr t2])
    | Tnot t -> Sexp_apply (mk_id_from_str "not", no_label [term2expr t])
    | Tif (t1, t2, t3) ->
      Sexp_ifthenelse (term2expr t1, term2expr t2, Some (term2expr t3))
    | Tquant (_, _, _) -> assert false (* unreachable point in code *)
    | Tattr (_, t) -> term2expr t
    | Tlet (x, t1, t2) -> 
      Sexp_let (Nonrecursive, [mk_s_value_binding x t1], term2expr t2)
    | Tcase (t, cl) -> Sexp_match (term2expr t,
        List.map (fun (p, term) -> (mk_pattern p, term2expr term)) cl)
    | Tcast (_, _) -> assert false (* Não existe Sexp *)
    | Ttuple tl -> Sexp_tuple (List.map term2expr tl)
    | Trecord tl -> Sexp_record (List.map term2expr tl)
    | Tupdate (_, _) -> assert false (* Não existe Sexp *)
    | Tscope (_, _) -> assert false (* Não existe Sexp *)
    | Told _ -> assert false (* TODO: if there is time, implement translation for "old" *)
  in
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
