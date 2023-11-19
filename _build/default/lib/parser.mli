
(* The type of tokens. *)

type token = 
  | WITH
  | TYPE
  | RPAREN
  | REC
  | PROVE
  | OF
  | MATCH
  | LPAREN
  | LET
  | INT of (int)
  | INDUCTION
  | IDENT of (string)
  | HINT
  | EQUAL
  | EOF
  | ENDCOMMENT
  | COLON
  | BAR
  | AXIOM
  | ASTERISK
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.declaration list)
