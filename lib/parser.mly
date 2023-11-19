%{
  open Ast
%}
%token <string> IDENT
%token <int> INT
%token EOF RPAREN LPAREN
%token LET REC PROVE AXIOM MATCH TYPE INDUCTION OF WITH
%token HINT ENDCOMMENT
%token COLON EQUAL BAR ARROW ASTERISK

%start main
%type <declaration list> main
%type <variable list> variable_list
%type <variable> variable
%type <equality> equality
%type <hint> hint
%type <expression> expression
%type <typeconstructor> constructor
%%

main:
| list(declaration) ; EOF { $1 } 

variable:
| v1 = IDENT ; COLON; v2 = IDENT
    { Vars (v1,v2) }
| LPAREN ; var = variable; RPAREN { var }

variable_list:
| v = variable { [v] }
| v = variable; vl = variable_list { v :: vl }

equality: 
| eq1 = expression; EQUAL ; eq2 = expression
    { Equality (eq1, eq2) }
| LPAREN ; eql = equality ; RPAREN {eql}

constructor:
| nm = IDENT { Typecon (nm, []) }
| nm = IDENT ; OF ; LPAREN ; type1 = separated_nonempty_list(ASTERISK, string) ; RPAREN {Typecon(nm, type1)}

string:
| nm = IDENT {nm}

hint: 
| HINT ; AXIOM ; ENDCOMMENT {Axiom}
| HINT ; INDUCTION ; x = IDENT ; ENDCOMMENT {Induction x}

declaration:
| LET ; PROVE ; func_name = IDENT ; args = list(variable) ; EQUAL ; eql = equality ; h = option(hint) ; ENDCOMMENT {Dec (func_name, args, eql, h)}
| TYPE ; name = IDENT ; EQUAL ; option(BAR) ; constr = separated_nonempty_list(BAR, constructor) {Type (name, constr)} 
| LET ; REC ; name = IDENT ; args = list(variable) ; COLON ; return = IDENT ; EQUAL {Rec (name, args, return)}

expression:
| LPAREN ; e = expression ; RPAREN { e }
| nm = IDENT { Identifier nm }
| MATCH ; nm = IDENT ; WITH {Match nm}
| e1 = expression; nm = IDENT 
    { Application (e1, Identifier nm) }
| e1 = expression; LPAREN; e2 = expression; RPAREN  
    { Application (e1, e2) }
