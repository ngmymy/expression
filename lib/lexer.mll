{
 open Parser
 exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
 | [' ' '\t'] { token lexbuf }
 | newline { Lexing.new_line lexbuf; token lexbuf }
 | "(*prove*)" { PROVE }
 | "(*hint: " { HINT }
 | "->" { ARROW }
 | "|" { BAR }
 | ":" { COLON }
 | "=" { EQUAL }
 | '*' { ASTERISK }
 | "(*" { comment 0 lexbuf }
 | '(' { LPAREN }
 | ')' { RPAREN }
 | "*)" { ENDCOMMENT }
 | ['a'-'z' 'A'-'Z' '0'-'9' '_' '/']+ as word { 
    match word with 
    | "axiom" -> AXIOM
    | "type" -> TYPE
    | "of" -> OF
    | "with" -> WITH
    | "let" -> LET
    | "match" -> MATCH 
    | "induction" -> INDUCTION
    | "rec" -> REC
    | _ -> IDENT(word) }
 | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 | eof { EOF }
and comment level = parse
| newline { Lexing.new_line lexbuf;comment level lexbuf }
| "*)" { if level = 0 then token lexbuf
            else comment (level - 1) lexbuf }
| "(*" { comment (level + 1) lexbuf }
| _ { comment level lexbuf }
| eof { raise (SyntaxError "Unclosed comment") }