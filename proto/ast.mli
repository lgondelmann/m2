(* Arbres de syntaxe abstraite *)

type location = Lexing.position * Lexing.position

type ident = string

type constant =
  | Cunit
  | Cbool of bool
  | Cint of int
  | Cstring of string

type unop =
  | Unot | Uminus

type binop =
  | Beq | Bneq | Blt | Ble | Bgt | Bge
  | Badd | Bsub | Bmul | Bdiv
  | Band | Bor

type is_rec = bool

type p_expr =
  { pexpr_desc: p_expr_desc;
    pexpr_loc: location; }

and p_expr_desc =
  | PE_cte of constant
  | PE_ident of ident
  | PE_unop of unop * p_expr
  | PE_binop of binop * p_expr * p_expr
  | PE_if of p_expr * p_expr * p_expr
  | PE_app of p_expr * p_expr
  | PE_fun of p_patt * p_expr
  | PE_tuple of p_expr list
  | PE_let of is_rec * p_patt * p_expr * p_expr
  | PE_match of p_expr * p_expr * (p_patt * p_patt * p_expr)
  | PE_nil
  | PE_cons of p_expr * p_expr

and p_patt =
  { ppatt_desc: p_patt_desc;
    ppatt_loc: location; }

and p_patt_desc =
  | PP_any
  | PP_ident of ident
  | PP_tuple of p_patt list

type p_def =
    { pdef_desc: is_rec * p_patt * p_expr;
      pdef_loc: location; }

type p_file = p_def list

