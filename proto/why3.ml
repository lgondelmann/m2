
open Format
open Typed_ast

let rec print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; sep fmt (); print_list sep print fmt r

let comma fmt () = fprintf fmt ",@ "
let newline fmt () = fprintf fmt "@\n"
let newline2 fmt () = fprintf fmt "@\n@\n"
let space fmt () = fprintf fmt "@ "

let rec typ fmt = function
  | Tvar i -> fprintf fmt "'a%d" i
  | Tarrow (ty1, ty2) -> fprintf fmt "(%a -> %a)" typ ty1 typ ty2
  | Tint -> fprintf fmt "int"
  | Tunit -> fprintf fmt "unit"
  | Tbool -> fprintf fmt "bool"
  | Tstring -> assert false (*TODO*)
  | Tlist ty -> fprintf fmt "(list %a)" typ ty
  | Ttuple tyl -> fprintf fmt "(%a)" (print_list comma typ) tyl

let print_rec fmt = function
  | true -> fprintf fmt "rec "
  | false -> ()

let rec pat fmt p = match p.tpatt_desc with
  | TP_any -> fprintf fmt "_"
  | TP_ident s -> fprintf fmt "%s" s
  | TP_tuple pl -> fprintf fmt "(%a)" (print_list comma pat) pl

let rec pat_ty fmt p = match p.tpatt_desc with
  | TP_any | TP_ident _ -> fprintf fmt "(%a : %a)" pat p typ p.tpatt_typ
  | TP_tuple _ -> assert false

let binop fmt = function
  | Ast.Beq -> fprintf fmt "="
  | Ast.Bneq -> fprintf fmt "<>"
  | Ast.Blt -> fprintf fmt "<"
  | Ast.Ble -> fprintf fmt "<="
  | Ast.Bgt -> fprintf fmt ">"
  | Ast.Bge -> fprintf fmt ">="
  | Ast.Badd -> fprintf fmt "+"
  | Ast.Bsub -> fprintf fmt "-"
  | Ast.Bmul -> fprintf fmt "*"
  | Ast.Band -> fprintf fmt "&&"
  | Ast.Bor -> fprintf fmt "||"
  | Ast.Bdiv -> assert false

let rec decomp_args e = match e.texpr_desc with
  | TE_fun ({tpatt_desc = TP_any | TP_ident _} as p, e) ->
      let args, e = decomp_args e in
      p :: args, e
  | TE_fun ({tpatt_desc = TP_tuple _}, _) -> assert false
  | _ -> [], e

let rec expr fmt e = match e.texpr_desc with
  | TE_cte Ast.Cunit -> fprintf fmt ""
  | TE_cte (Ast.Cbool true) -> fprintf fmt "True"
  | TE_cte (Ast.Cbool false) -> fprintf fmt "False"
  | TE_cte (Ast.Cint n) -> fprintf fmt "%d" n
  | TE_cte (Ast.Cstring _) -> assert false (*TODO*)
  | TE_unop (Ast.Unot, e1) -> fprintf fmt "(notb %a)" expr e1
  | TE_unop (Ast.Uminus, e1) -> fprintf fmt "(- %a)" expr e1
  | TE_binop (Ast.Bdiv, e1, e2) -> fprintf fmt "(div %a %a)" expr e1 expr e2
  | TE_binop (op, e1, e2) -> fprintf fmt "(%a %a %a)" expr e1 binop op expr e2
  | TE_if (e1, e2, e3) -> fprintf fmt "(if %a then %a else %a)"
      expr e1 expr e2 expr e3
  | TE_nil -> fprintf fmt "Nil"
  | TE_cons (e1, e2) -> fprintf fmt "(Cons %a %a)" expr e1 expr e2
  | TE_app (e1, e2) -> fprintf fmt "(%a %a)" expr e1 expr e2
  | TE_tuple el -> fprintf fmt "(%a)" (print_list comma expr) el
  | TE_let (r, p, e1, e2) ->
      let args, e1 = decomp_args e1 in
      fprintf fmt "(let %a%a %a = %a in@ %a)"
        print_rec r pat p (print_list space pat_ty) args expr e1 expr e2
  | TE_fun _ -> assert false
  | TE_ident s -> fprintf fmt "%s" s
  | TE_match (e1, b1, (hd2, tl2, b2)) ->
      fprintf fmt "match %a with Nil -> %a | Cons %a %a -> %a end"
        expr e1 expr b1 pat hd2 pat tl2 expr b2

let def fmt (r, p, e) =
  let args, e = decomp_args e in
  fprintf fmt "  @[<hov 2>let %a%a %a =@ %a@]@\n"
    print_rec r pat_ty p (print_list space pat_ty) args expr e

let print_program fmt p =
  fprintf fmt "module M@.";
  fprintf fmt "  use import int.Int@.";
  fprintf fmt "  use import int.ComputerDivision@.";
  fprintf fmt "  use import list.List@.";
  print_list newline def fmt p;
  fprintf fmt "end@."


