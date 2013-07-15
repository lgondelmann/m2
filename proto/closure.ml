
(* construction des clotures *)

open Format
open Ast
open Typing
open Typed_ast

type env = ident list

type cexpr =
  { cexpr_desc : cexpr_desc;
    cexpr_typ  : typ; }

and cexpr_desc =
  | Ccte of constant
  | Cunop of unop * cexpr
  | Cbinop of binop * cexpr * cexpr
  | Cif of cexpr * cexpr * cexpr
  | Cnil
  | Ccons of cexpr * cexpr
  | Capp of cexpr * cexpr
  | Ctuple of cexpr list
  | Clet of is_rec * tpatt * cexpr * cexpr
  | Cclos of string * env
  | Cglobal of ident
  | Clocal of ident
  | Cmatch of cexpr * cexpr * (tpatt * tpatt * cexpr)

type fundef = string * env * tpatt * cexpr

type letdef = is_rec * tpatt * cexpr

type ctx = fundef list (* la liste accumulee des clotures construites *)

(* [pat_vars s p] ajoute à l'ensemble [s] les variables du pattern [p] *)
let pat_vars =
  let rec pvars s p = match p.tpatt_desc with
    | TP_any -> s
    | TP_ident x -> Sset.add x s
    | TP_tuple pl -> List.fold_left pvars s pl
  in
  pvars

(* [fvars bv p e] calcule les variables libres dans [fun p -> e] *)
(* [bv] est l'ensemble des variables qu'il ne faut pas considérer *)
let fvars bv p e =
  let rec fv f b e = match e.texpr_desc with
    | TE_nil | TE_cte _ -> f
    | TE_unop (_, e) -> fv f b e
    | TE_binop (_, e1, e2) | TE_cons(e1, e2) | TE_app (e1, e2) -> 
	fv (fv f b e1) b e2
    | TE_if (e1, e2, e3) -> fv (fv (fv f b e1) b e2) b e3
    | TE_tuple el -> List.fold_left (fun f e -> fv f b e) f el
    | TE_let (false, p, e1, e2) ->
	let f = fv f b e1 in
	let b = pat_vars b p in
	fv f b e2
    | TE_let (true, p, e1, e2) ->
	let b = pat_vars b p in
	fv (fv f b e1) b e2
    | TE_fun (p, e1) ->
	let b = pat_vars b p in
	fv f b e1
    | TE_ident x ->
	if Sset.mem x b then f else Sset.add x f
    | TE_match (e1, e2, (ph, pt, e3)) ->
	let f = fv (fv f b e1) b e2 in
	let b = pat_vars (pat_vars b ph) pt in
	fv f b e3
  in
  fv Sset.empty (pat_vars bv p) e

(* [bind gl p] supprime de [gl] les variables liées dans [p] *)
let bind gl p = Sset.diff gl (pat_vars Sset.empty p) 

let new_fun =
  let r = ref 0 in
  fun () -> incr r; "Fun_" ^ string_of_int !r

let rec expr gl ctx e =
  let ctx, d = desc gl ctx e.texpr_desc in
  ctx, { cexpr_desc = d; cexpr_typ = e.texpr_typ }

and desc gl ctx = function
  | TE_cte c -> 
      ctx, Ccte c
  | TE_unop (op, e) ->
      let ctx, e = expr gl ctx e in
      ctx, Cunop (op, e)
  | TE_binop (op, e1, e2) ->
      let ctx, e1 = expr gl ctx e1 in
      let ctx, e2 = expr gl ctx e2 in
      ctx, Cbinop (op, e1, e2)
  | TE_if (e1, e2, e3) ->
      let ctx, e1 = expr gl ctx e1 in
      let ctx, e2 = expr gl ctx e2 in
      let ctx, e3 = expr gl ctx e3 in
      ctx, Cif (e1, e2, e3)
  | TE_nil ->
      ctx, Cnil
  | TE_cons (e1, e2) ->
      let ctx, e1 = expr gl ctx e1 in
      let ctx, e2 = expr gl ctx e2 in
      ctx, Ccons (e1, e2)
  | TE_app (e1, e2) ->
      let ctx, e1 = expr gl ctx e1 in
      let ctx, e2 = expr gl ctx e2 in
      ctx, Capp (e1, e2)
  | TE_tuple el ->
      let ctx, el = 
	List.fold_left 
	  (fun (ctx,el) e -> let ctx, e = expr gl ctx e in ctx, e::el)
	  (ctx,[]) el
      in
      ctx, Ctuple el
  | TE_let (is_rec, p, e1, e2) ->
      let gl' = bind gl p in
      let ctx, e1 = expr (if is_rec then gl' else gl) ctx e1 in
      let ctx, e2 = expr gl' ctx e2 in
      ctx, Clet (is_rec, p, e1, e2)
  | TE_fun (p, e1) ->
      let v = Sset.elements (fvars gl p e1) in
      let gl' = bind gl p in
      let ctx, e1 = expr gl' ctx e1 in
      let n = new_fun () in
      let f = (n, v, p, e1) in
      (* printf "closure %s [@[%a@]]@." n (print_comma_list pp_print_string) v; *)
      f :: ctx, Cclos (n, v)
  | TE_ident x ->
      (* printf "ident %s : global=%b@." x (Sset.mem x gl); *)
      ctx, if Sset.mem x gl then Cglobal x else Clocal x
  | TE_match (e1, e2, (ph, pt, e3)) ->
      let ctx, e1 = expr gl ctx e1 in
      let ctx, e2 = expr gl ctx e2 in
      let gl' = bind (bind gl ph) pt in
      let ctx, e3 = expr gl' ctx e3 in
      ctx, Cmatch (e1, e2, (ph, pt, e3))

let decl gl ctx (is_rec, p, e) = 
  let gl' = pat_vars gl p in
  let ctx, e = expr (if is_rec then gl' else gl) ctx e in
  gl', ctx, (is_rec, p, e)

let prims = Hashtbl.fold (fun p _ acc -> Sset.add p acc) primitives Sset.empty
let initial_gl = prims

let file f =
  let _, ctx, dl = 
    List.fold_left 
      (fun (gl, ctx, dl) d -> 
	 let gl, ctx, d = decl gl ctx d in gl, ctx, d :: dl)
      (initial_gl, [], []) f
  in
  List.rev dl, ctx
