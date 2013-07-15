
(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Ast

let usage = "usage: compilo [options] file.ml"

let parse_only = ref false
let type_only = ref false

let spec =
  [ "-parse-only", Arg.Set parse_only, "  stops after parsing";
    "-type-only", Arg.Set type_only, "  stops after typing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".ml") then
      raise (Arg.Bad "no .ml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let f = Typing.file f in
    if !type_only then exit 0;
    (* let f = Closure.file f in *)
    let code = Inline.program f in
    let c = open_out (Filename.chop_suffix file ".ml" ^ ".mlw") in
    let fmt = formatter_of_out_channel c in
    Why3.print_program fmt code;
    close_out c
  with
    | Lexical_error s ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s\n@." s;
	exit 1
    | Parsing.Parse_error ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error\n@.";
	exit 1
    | Typing.Error(l,e) ->
	report_loc l;
	eprintf "%a\n@." Typing.report e;
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2



