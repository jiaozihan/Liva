type token =
  | CLASS
  | EXTENDS
  | CONSTRUCTOR
  | IMPORT
  | DOT
  | THIS
  | INT
  | FLOAT
  | BOOLEAN
  | CHAR
  | VOID
  | NULL
  | TRUE
  | FALSE
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | AND
  | NOT
  | OR
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | MODULO
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | BREAK
  | CONTINUE
  | NEW
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | ID of (string)
  | CHAR_LITERAL of (char)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
