{

  open Lexing
  open Parser
  open Ast

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [ 
	"else", ELSE;
	"false", CONST_BOOL(false); 
	"function", FUNCTION;
	"if", IF;
	"in", IN;
	"let", LET;
	"match", MATCH;
	"not", NOT;
	"rec", REC;
	"then", THEN;
	"true", CONST_BOOL(true);
	"with", WITH;
      ];
    fun s -> 
      try Hashtbl.find h s with Not_found -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let comment_cpt = ref 0
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

rule token = parse
  | '\n' 
      { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ 
      { token lexbuf }
  | "(*"   
      { incr comment_cpt; comment lexbuf; token lexbuf }
  | "_"
      { UNDERSCORE }
  | ident 
      { id_or_keyword (lexeme lexbuf) }
  | digit+ 
      { CONST_INT (int_of_string (lexeme lexbuf)) } 
  | "|" 
      { BAR }
  | ";" 
      { SEMI }
  | "-"
      { MINUS }
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "/"
      { SLASH }
  | ">" 
      { COMP Bgt }
  | ">=" 
      { COMP Bge }
  | "<" 
      { COMP Blt }
  | "<=" 
      { COMP Ble }
  | "<>"
      { NEQ }
  | "&&"
      { AND }
  | "||"
      { OR }
  | "->"
      { MINUS_GT }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | "::"
      { COLONCOLON }
  | "="
      { EQUAL }
  | ","
      { COMMA }
  | '"'
      { let buf = Buffer.create 512 in
	string buf lexbuf;
	CONST_STRING (Buffer.contents buf) }
  | _ 
      { raise (Lexical_error (lexeme lexbuf)) }
  | eof
      { EOF }

and string buf = parse
  | '"' { () }
  | '\\' 'n' 
      { Buffer.add_string buf "\n";
	string buf lexbuf }
  | '\\' '\\' 
      { Buffer.add_string buf "\\";
	string buf lexbuf }
  | '\\' '"' 
      { Buffer.add_string buf "\"";
	string buf lexbuf }
  | [^ '\\' '"' '\n']+ 
      { Buffer.add_string buf (lexeme lexbuf);
	string buf lexbuf }
  | '\\' 
      { raise (Lexical_error "illegal escape character") }
  | '\n' | eof
      { raise (Lexical_error "unterminated string") }
  | _ 
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }

and comment = parse
  | "(*" { incr comment_cpt; comment lexbuf }
  | "*)" { decr comment_cpt; if !comment_cpt > 0 then comment lexbuf }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }

