
open Typed_ast

(*
let rec inl_let t = match t with
  | TE_cte _ -> t
  | TE_unop (u, t') -> TE_unop (u, inl_let t')
  | TE_binop (b, p1, p2) -> TE_binop (b, inl_let t1, inl_let t2)
  | TE_if (t0, t1, t2) -> TE_if (inl_let t0, inl_let t1, inl_let t2)
  | TE_nil -> t
  | TE_cons (hd, tl) -> TE_cons (inl_let hd, inl_let tl)
  | TE_app (t1, t2) -> TE_app (inl_let t1, inl_let t2)
  | TE_tuple tl -> TE_tuple (List.map inl_let tl)
  | TE_let (r, p, t1, t2) ->
    let t1',t2' = inl_let t1, inl_let t2 in
    *)
(*
let tpatt_head p = match p.tpatt_desc with
  | TP_tuple tl -> List.hd tl
  | _ -> p

let tpatt_check_ho = function
  | TP_any ->
  | pat :: _ ->
    if pat.typ = Tarrow _ then
*)

(*






let inl_expr t =
  let pl, t = decomp_args t in
  if pl = [] then inl_context t
  else if is_first_order (List.hd pl)
  then inl_context t
  else inl_head

let inl_expr (t: texpr) = match t with
  | TE_let (r, p, t1, t2) ->
    let t1', t2' = inl_expr t1', inl_expr t2' in
    let (pl,  =
    if is_first_order p
    then inl_let (r,p,t1') t2'
    else TE_let (r, p, t1, t2)
  | _ -> t

let inl_def (d: t_def) = match d with
  | (r, p, t) -> (r, p, inl_expr t)


    *)
let rec decomp_args e = match e.texpr_desc with
  | TE_fun ({tpatt_desc = TP_ident _} as p, e)  ->
    let args, e = decomp_args e in
    p :: args, e
  | TE_fun ({tpatt_desc = TP_tuple _}, _)
  | TE_fun ({tpatt_desc = TP_any}, _) -> assert false
  | _ -> [], e



let rec is_first_order typ = match typ with
  | Tarrow (Tarrow (_,_), _) -> false
  | Tarrow (_, typ2) -> is_first_order typ2
  | Ttuple _ -> assert false
  | _ -> true


module Env = Map.Make(String)


let add_env (r,p,t) env =
  match p.tpatt_desc with
    | TP_ident id ->
      let tid = {texpr_desc = TE_ident id; texpr_typ = p.tpatt_typ} in
      let texpr = {texpr_desc = TE_let (r,p,t, tid); texpr_typ = p.tpatt_typ}
      in Env.add id texpr env
    | _ -> assert false

let rec inline_expr env t = match t.texpr_desc with
  | TE_ident id -> (try Env.find id env with Not_found -> t)
  | TE_cte _ -> t
  | TE_unop (u, t') ->
    {t with texpr_desc = TE_unop (u, inline_expr env t')}
  | TE_binop (b, t1, t2) ->
    {t with texpr_desc = TE_binop (b, inline_expr env t1, inline_expr env t2)}
  | TE_if (t0, t1, t2) ->
    {t with texpr_desc =
	TE_if (inline_expr env t0, inline_expr env t1, inline_expr env t2)}
  | TE_nil -> t
  | TE_cons (hd, tl) ->
    {t with texpr_desc = TE_cons (inline_expr env hd, inline_expr env tl)}
  | TE_app (t1, t2) ->
    {t with texpr_desc = TE_app (inline_expr env t1, inline_expr env t2)}
  | TE_tuple tl -> assert false
  | TE_let (r, p, t1, t2) ->
    {t with texpr_desc = TE_let (r, p, inline_expr env t1, inline_expr env t2)}
  | _ -> t


let rec program env p  = match p with
  | [] -> []
  | ((r, p, t) as d) :: dl ->
    let d' = (r, p, inline_expr env t) in
    if is_first_order p.tpatt_typ
    then d' :: program env dl
    else program (add_env d env) dl

let program p = program Env.empty p




