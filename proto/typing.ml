
open Format
open Ast

type tvar = {
  id : int;
  mutable level : int;
  mutable def : typ option;
}

and typ =
  | Tvar of tvar
  | Tarrow of typ * typ
  | Tint
  | Tunit
  | Tbool
  | Tstring
  | Tlist of typ
  | Ttuple of typ list

type texpr =
  { texpr_desc : texpr_desc;
    texpr_typ  : typ; }

and texpr_desc =
  | TE_cte of Ast.constant
  | TE_unop of Ast.unop * texpr
  | TE_binop of Ast.binop * texpr * texpr
  | TE_if of texpr * texpr * texpr
  | TE_nil
  | TE_cons of texpr * texpr
  | TE_app of texpr * texpr
  | TE_tuple of texpr list
  | TE_let of Ast.is_rec * tpatt * texpr * texpr
  | TE_fun of tpatt * texpr
  | TE_ident of Ast.ident
  | TE_match of texpr * texpr * (tpatt * tpatt * texpr)

and tpatt =
  { tpatt_desc : tpatt_desc;
    tpatt_typ  : typ }

and tpatt_desc =
  | TP_any
  | TP_ident of Ast.ident
  | TP_tuple of tpatt list

type t_def = Ast.is_rec * tpatt * texpr

type program = t_def list

let rec print_comma_list print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: l -> fprintf fmt "%a,@ %a" print x (print_comma_list print) l

let rec print_type fmt = function
  | Tvar { level = n; id = id; def = None } -> fprintf fmt "'x%d(%d)" id n
  | Tvar { def = Some t } -> print_type fmt t
  | Tarrow (t1,t2) -> fprintf fmt "@[(%a ->@ %a)@]" print_type t1 print_type t2
  | Tint -> fprintf fmt "int"
  | Tunit -> fprintf fmt "unit"
  | Tbool -> fprintf fmt "bool"
  | Tstring -> fprintf fmt "string"
  | Tlist t -> fprintf fmt "@[(%a list)@]" print_type t
  | Ttuple tl -> fprintf fmt "@[(%a)@]" (print_comma_list print_type) tl

let fresh_tvar =
  let r = ref 0 in
  fun lvl -> incr r; Tvar { id = !r; level = lvl; def = None }

type error =
  | UnificationFailure of typ * typ
  | RecursiveValue
  | UnboundVariable of ident
  | DuplicatePatIdent of ident

exception Error of location * error

let report fmt = function
  | UnificationFailure (t1, t2) ->
      fprintf fmt "Cannot unify type %a with type %a"
	print_type t1 print_type t2
  | RecursiveValue ->
      fprintf fmt "this kind of recursion is not allowed"
  | UnboundVariable id ->
      fprintf fmt "unbound variable %s" id
  | DuplicatePatIdent id ->
      fprintf fmt "duplicate variable %s in pattern" id

let unification_error loc t1 t2 =
  raise (Error (loc, UnificationFailure (t1, t2)))

let rec occur v = function
  | Tvar { def = Some t } -> occur v t
  | Tvar w -> v.id = w.id
  | Tarrow (t1, t2) -> occur v t1 || occur v t2
  | Ttuple tl -> List.exists (occur v) tl
  | Tlist t -> occur v t
  | Tint | Tbool | Tunit | Tstring -> false

let rec fix_levels n = function
  | Tvar { def = Some t } -> fix_levels n t
  | Tvar ({ def = None } as v) -> if v.level > n then v.level <- n
  | Tarrow (t1, t2) -> fix_levels n t1; fix_levels n t2
  | Ttuple tl -> List.iter (fix_levels n) tl
  | Tlist t -> fix_levels n t
  | Tunit | Tint | Tbool | Tstring -> ()

let rec unify loc t1 t2 = match t1, t2 with
  | Tvar v1, Tvar v2 when v1.id = v2.id ->
      ()
  | Tvar { def = Some t1 }, t2
  | t1, Tvar { def = Some t2 } ->
      unify loc t1 t2
  | Tvar ({ def = None } as v), t
  | t, Tvar ({ def = None } as v) ->
      if occur v t then unification_error loc t1 t2;
      fix_levels v.level t;
      v.def <- Some t
  | Tarrow (t11, t12), Tarrow (t21, t22) ->
      unify loc t11 t21;
      unify loc t12 t22
  | Tlist t1, Tlist t2 ->
      unify loc t1 t2
  | Ttuple tl1, Ttuple tl2 ->
      List.iter2 (unify loc) tl1 tl2
  | Tint, Tint
  | Tunit, Tunit
  | Tbool, Tbool
  | Tstring, Tstring ->
      ()
  | _ ->
      unification_error loc t1 t2

(* let unify loc t1 t2 = *)
(*   printf "unify %a %a...@?" print_type t1 print_type t2; *)
(*   unify loc t1 t2; *)
(*   printf "done@." *)

module Smap = Map.Make(String)

type env = {
  lvl : int;
  env : (typ * int) Smap.t;
    (* x:(ty,n) signifie << x:ty et toutes les variables de niveau strictement
       supérieur à n sont généralisées >> *)
}

let specialize n m ty =
  let h = Hashtbl.create 17 in
  let rec specialize = function
    | Tunit | Tbool | Tint | Tstring as ty -> ty
    | Tvar { id = id; level = l; def = None } when l > m ->
	begin
	  try
	    Hashtbl.find h id
	  with Not_found ->
	    let v = fresh_tvar n in Hashtbl.add h id v; v
	end
    | Tvar { def = None } as t -> t
    | Tvar { def = Some t } -> specialize t
    | Tarrow (t1, t2) -> Tarrow (specialize t1, specialize t2)
    | Tlist t -> Tlist (specialize t)
    | Ttuple tl -> Ttuple (List.map specialize tl)
  in
  specialize ty

module Sset = Set.Make(String)

let check_pattern p =
  let loc = p.ppatt_loc in
  let rec check s p = match p.ppatt_desc with
    | PP_any ->
	s
    | PP_ident x ->
	if Sset.mem x s then raise (Error (loc, DuplicatePatIdent x));
	Sset.add x s
    | PP_tuple pl ->
	List.fold_left check s pl
  in
  ignore (check Sset.empty p)

let check_cons_pattern ph pt =
  check_pattern { ph with ppatt_desc = PP_tuple [ph; pt] }

let rec type_expr env e =
  let e, ty = type_expr_desc env e.pexpr_loc e.pexpr_desc in
  { texpr_desc = e; texpr_typ = ty }

and type_expr_desc env loc = function
  | PE_cte Cunit -> TE_cte Cunit, Tunit
  | PE_cte (Cint _ as c) -> TE_cte c, Tint
  | PE_cte (Cbool _ as c) -> TE_cte c, Tbool
  | PE_cte (Cstring _ as c) -> TE_cte c, Tstring
  | PE_unop (Unot, e) ->
      TE_unop (Unot, type_expr_as env Tbool e), Tbool
  | PE_unop (Uminus, e) ->
      TE_unop (Uminus, type_expr_as env Tint e), Tint
  | PE_binop (Beq | Bneq | Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let e1 = type_expr_as env Tint e1 in
      let e2 = type_expr_as env Tint e2 in
      (* unify loc e1.texpr_typ e2.texpr_typ; *)
      TE_binop (op, e1, e2), Tbool
  | PE_binop (Badd | Bsub | Bmul | Bdiv as op, e1, e2) ->
      let e1 = type_expr_as env Tint e1 in
      let e2 = type_expr_as env Tint e2 in
      TE_binop (op, e1, e2), Tint
  | PE_binop (Band | Bor as op, e1, e2) ->
      let e1 = type_expr_as env Tbool e1 in
      let e2 = type_expr_as env Tbool e2 in
      TE_binop (op, e1, e2), Tbool
  | PE_if (e1, e2, e3) ->
      let e1 = type_expr_as env Tbool e1 in
      let e2 = type_expr env e2 in
      let e3 = type_expr env e3 in
      unify loc e2.texpr_typ e3.texpr_typ;
      TE_if (e1, e2, e3), e2.texpr_typ
  | PE_nil ->
      let v = fresh_tvar env.lvl in
      TE_nil, Tlist v
  | PE_cons (e1, e2) ->
      let e1 = type_expr env e1 in
      let e2 = type_expr env e2 in
      unify loc (Tlist e1.texpr_typ) e2.texpr_typ;
      TE_cons (e1, e2), e2.texpr_typ
  | PE_app (e1, e2) ->
      let alpha = fresh_tvar env.lvl in
      let beta = fresh_tvar env.lvl in
      let e1 = type_expr_as env (Tarrow (alpha, beta)) e1 in
(*       printf "alpha = %a beta = %a@." print_type alpha print_type beta; *)
      let e2 = type_expr_as env alpha e2 in
      TE_app (e1, e2), beta
  | PE_tuple el ->
      let el = List.map (type_expr env) el in
      TE_tuple el, Ttuple (List.map (fun e -> e.texpr_typ) el)
  | PE_let (true, {ppatt_desc = PP_ident id},
	   ({pexpr_desc = PE_fun _} as e1), e2) ->
      let ty = fresh_tvar (env.lvl + 1) in
      let p = { tpatt_desc = TP_ident id; tpatt_typ = ty } in
      let env = { env with env = Smap.add id (ty, env.lvl+1) env.env } in
      let e1 = type_expr { env with lvl = env.lvl + 1 } e1 in
      let env = { env with env = Smap.add id (ty, env.lvl) env.env } in
      unify loc e1.texpr_typ p.tpatt_typ;
      let e2 = type_expr env e2 in
      TE_let (true, p, e1, e2), e2.texpr_typ
  | PE_let (true, _, _, _) ->
      raise (Error (loc, RecursiveValue))
  | PE_let (false, p, e1, e2) ->
      check_pattern p;
      let e1 = type_expr { env with lvl = env.lvl + 1 } e1 in
      let p, env = type_pattern ~is_let:true env p in
      unify loc e1.texpr_typ p.tpatt_typ;
      let e2 = type_expr env e2 in
      TE_let (false, p, e1, e2), e2.texpr_typ
  | PE_fun (p, e) ->
      check_pattern p;
      let p, env = type_pattern env p in
      let e = type_expr env e in
      TE_fun (p, e), Tarrow (p.tpatt_typ, e.texpr_typ)
  | PE_ident id ->
      let ty, m =
	try Smap.find id env.env
	with Not_found -> raise (Error (loc, UnboundVariable id))
      in
      let n = env.lvl in
      let ty = specialize n m ty in
      (* printf "specialize(%s) : %a@." id print_type ty; *)
      TE_ident id, ty
  | PE_match (e1, bnil, (ph, pt, bcons)) ->
      check_cons_pattern ph pt;
      let v = fresh_tvar env.lvl in
      let e1 = type_expr_as env (Tlist v) e1 in
      let bnil = type_expr env bnil in
      let phloc = ph.ppatt_loc in
      let ph, env = type_pattern env ph in
      unify phloc ph.tpatt_typ v;
      let ptloc = pt.ppatt_loc in
      let pt, env = type_pattern env pt in
      unify ptloc pt.tpatt_typ (Tlist v);
      let bcons = type_expr env bcons in
      unify loc bnil.texpr_typ bcons.texpr_typ;
      TE_match (e1, bnil, (ph, pt, bcons)), bcons.texpr_typ

and type_expr_as env ty e =
  let loc = e.pexpr_loc in
  let e = type_expr env e in
  unify loc e.texpr_typ ty;
  e

and type_pattern ?(is_let=false) env p = match p.ppatt_desc with
  | PP_any ->
      let lvl' = if is_let then env.lvl + 1 else env.lvl in
      let ty = fresh_tvar lvl' in
      { tpatt_desc = TP_any; tpatt_typ = ty }, env
  | PP_ident id ->
      let lvl' = if is_let then env.lvl + 1 else env.lvl in
      let ty = fresh_tvar lvl' in
      { tpatt_desc = TP_ident id; tpatt_typ = ty },
      { env with env = Smap.add id (ty, env.lvl) env.env }
  | PP_tuple pl ->
      let pl, env =
	List.fold_right
	  (fun p (pl,env) ->
	    let p,env = type_pattern ~is_let env p in p :: pl, env)
	  pl ([], env)
      in
      { tpatt_desc = TP_tuple pl;
	tpatt_typ = Ttuple (List.map (fun p -> p.tpatt_typ) pl) },  env

let type_def env d = match d.pdef_desc with
  | false, p, e ->
      check_pattern p;
      let e = type_expr { env with lvl = env.lvl + 1 } e in
      let p, env = type_pattern ~is_let:true env p in
      unify d.pdef_loc e.texpr_typ p.tpatt_typ;
      (* printf "type = @[%a@]@." print_type e.texpr_typ; *)
      (false, p, e), env
  | true, {ppatt_desc = PP_ident id}, ({pexpr_desc = PE_fun _} as e) ->
      let ty = fresh_tvar (env.lvl + 1) in
      let p = { tpatt_desc = TP_ident id; tpatt_typ = ty } in
      let env = { env with env = Smap.add id (ty, env.lvl+1) env.env } in
      let e = type_expr { env with lvl = env.lvl + 1 } e in
      let env = { env with env = Smap.add id (ty, env.lvl) env.env } in
      unify d.pdef_loc e.texpr_typ p.tpatt_typ;
      (true, p, e), env
  | true, _, _ ->
      raise (Error (d.pdef_loc, RecursiveValue))

let primitives = Hashtbl.create 17
let () = Hashtbl.add primitives "print_int" (Tarrow (Tint, Tunit), 0)
let () = Hashtbl.add primitives "print_string" (Tarrow (Tstring, Tunit), 0)
let () = Hashtbl.add primitives "print_newline" (Tarrow (Tunit, Tunit), 0)
let () = Hashtbl.add primitives "read_int" (Tarrow (Tunit, Tint), 0)
let () = Hashtbl.add primitives "read_line" (Tarrow (Tunit, Tstring), 0)

let initial_env =
  Hashtbl.fold Smap.add primitives Smap.empty

let file dl =
  let _, dl =
    List.fold_left
      (fun (env,dl) d -> let d, env = type_def env d in env, d :: dl)
      ({ lvl = 0; env = initial_env }, []) dl
  in
  List.rev dl



(* Canonization of types *)

let rec canon_typ = function
  | Tvar { id = v; def = None } -> Typed_ast.Tvar v
  | Tvar { def = Some ty } -> canon_typ ty
  | Tarrow (ty1, ty2) -> Typed_ast.Tarrow (canon_typ ty1, canon_typ ty2)
  | Tint -> Typed_ast.Tint
  | Tunit -> Typed_ast.Tunit
  | Tbool -> Typed_ast.Tbool
  | Tstring -> Typed_ast.Tstring
  | Tlist ty -> Typed_ast.Tlist (canon_typ ty)
  | Ttuple tyl -> Typed_ast.Ttuple (List.map canon_typ tyl)

let canon_def d = assert false (*TODO*)

let rec canon_texpr e =
  { Typed_ast.texpr_desc = canon_texpr_desc e.texpr_desc;
    Typed_ast.texpr_typ = canon_typ e.texpr_typ; }

and canon_texpr_desc = function
  | TE_cte c -> Typed_ast.TE_cte c
  | TE_unop (u, e) -> Typed_ast.TE_unop (u, canon_texpr e)
  | TE_binop (b, e1, e2) ->
      Typed_ast.TE_binop (b, canon_texpr e1, canon_texpr e2)
  | TE_if (e1, e2, e3) ->
      Typed_ast.TE_if (canon_texpr e1, canon_texpr e2, canon_texpr e3)
  | TE_nil -> Typed_ast.TE_nil
  | TE_cons (e1, e2) -> Typed_ast.TE_cons (canon_texpr e1, canon_texpr e2)
  | TE_app (e1, e2) -> Typed_ast.TE_app (canon_texpr e1, canon_texpr e2)
  | TE_tuple el -> Typed_ast.TE_tuple (List.map canon_texpr el)
  | TE_let (r, p, e1, e2) ->
      Typed_ast.TE_let (r, canon_tpatt p, canon_texpr e1, canon_texpr e2)
  | TE_fun (p, e) -> Typed_ast.TE_fun (canon_tpatt p, canon_texpr e)
  | TE_ident id -> Typed_ast.TE_ident id
  | TE_match (e1, e2, (p1, p2, e3)) ->
      Typed_ast.TE_match (canon_texpr e1, canon_texpr e2,
                          (canon_tpatt p1, canon_tpatt p2, canon_texpr e3))

and canon_tpatt p =
  { Typed_ast.tpatt_desc = canon_tpatt_desc p.tpatt_desc;
    Typed_ast.tpatt_typ = canon_typ p.tpatt_typ }

and canon_tpatt_desc = function
  | TP_any -> Typed_ast.TP_any
  | TP_ident id -> Typed_ast.TP_ident id
  | TP_tuple pl -> Typed_ast.TP_tuple (List.map canon_tpatt pl)

let file dl = List.map canon_def (file dl)
