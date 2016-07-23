{ 
	open Parser 
	let depth = ref 0
	
	let unescape s =
		Scanf.sscanf (""" ^ s ^ """) "%S%!" (fun x -> x)
}

let whitespace = [' ' '\t'  '\r' '\n']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = alpha (alpha | digit | '_')*
let int = digit+
let float = (digit+) '.' (digit+)
let char = '\'' ( ascii) '\''
let escape = '\\' ['\\' '\'' '"' 'n' 'r' 't']
let escape_char = ''' (escape) '''
let string = '"' ( (ascii | escape)* as s) '"'


rule token = parse
	whitespace { token lexbuf }
  | "/*"       { incr depth; comment lexbuf }
  
  (* separator *)
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | ';'      { SEMI }
  | ','      { COMMA }
  | '.'      { DOT }
  
  (* Operators *)
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | '%'      { MODULO }
  | '='      { ASSIGN }
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  | "&"      { AND }
  | "|"      { OR }
  | "!"      { NOT }
  | '['      { LBRACKET }
  | ']'      { RBRACKET }
  
  (* Branch Control *)
  | "if"     { IF }
  | "else"   { ELSE }
  | "for"    { FOR }
  | "while"  { WHILE }
  | "return" { RETURN }

  (* Data Types *)
  | "int"       { INT }
  | "float"     { FLOAT }
  | "boolean"   { BOOLEAN }
  | "char"      { CHAR }
  | "void"      { VOID }
  | "null"      { NULL }
  | "true"      { TRUE }
  | "false"     { FALSE }

  (* Classes *)
  | "class"       { CLASS }
  | "constructor" { CONSTRUCTOR }
  | "extends"     { EXTENDS }
  | "import"     { IMPORT }
  | "this"        { THIS }
  | "break" 	  { BREAK }
  | "continue"	  { CONTINUE }
  | "new" 	   	  { NEW }
  
  | int as lxm          { INT_LITERAL(int_of_string lxm) }
  | float as lxm        { FLOAT_LITERAL(float_of_string lxm) }
  | char as lxm         { CHAR_LITERAL(String.get lxm 1) }
  | escape_char as lxm  { CHAR_LITERAL(String.get (unescape lxm) 1) }
  | string       		{ STRING_LITERAL(unescape s) }
  | id as lxm           { ID(lxm) }
  | eof                 { EOF }
  
  | _ as illegal  { raise (Failure("illegal character " ^ Char.escaped illegal )) }

and comment = parse
	"*/" 	{ decr depth; if !depth > 0 then comment lexbuf else token lexbuf }
  | "/*" 	{ incr depth; comment lexbuf }
  |  _    	{ comment lexbuf }
