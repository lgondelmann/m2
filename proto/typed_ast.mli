
(* Arbres de syntaxe abstraite typés *)

type tvar = int

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
