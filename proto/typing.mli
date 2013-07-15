
type error

exception Error of Ast.location * error

val report: Format.formatter -> error -> unit

val file: Ast.p_file -> Typed_ast.program
