include Ast
module Parser = Parser
module Lexer = Lexer

let rec string_of_expression (e: expression) =
  match e with
  | Identifier nm -> nm
  | Application (e1,e2) -> 
      (string_of_expression e1) ^ 
      " " ^ (string_of_expression_with_parens e2)
  | Match nm -> "match " ^ nm ^ " with "

  and string_of_expression_with_parens e = 
    match e with
    | Identifier nm -> nm
    | Application _ -> 
        "(" ^ string_of_expression e ^ ")"
    | Match nm -> " match " ^ nm ^ " with "

let rec string_of_declaration (d: declaration) =
  match d with
  | Dec (name, params, eq, hint) ->
      "let (*prove*) " ^ name ^ " : " ^ string_of_variable params ^ " = " ^
      string_of_equality (eq) ^ string_of_hint_option hint
  | Rec (nm, args, return) ->
      "let rec " ^ nm ^ (string_of_variable args) ^ " : " ^ return ^ " = "
  | Type (name, constr) -> "type" ^ name ^ " = " ^ string_of_typeconstructor_list constr

  and string_of_typeconstructor (tc: typeconstructor) : string =
    match tc with
    | Typecon (name, params) ->
      name ^ "(" ^ String.concat ", " params ^ ")"
  
  and string_of_typeconstructor_list (constr: typeconstructor list) : string =
    String.concat " | " (List.map string_of_typeconstructor constr)

  and string_of_variable (v: variable list) = 
    match v with
    | [] -> ""
    | Vars (v1,v2)::tl -> "(" ^ v1 ^ " : " ^ v2 ^ ")" ^ string_of_variable tl

  and string_of_equality (eq: equality) =
    match eq with 
    | Equality(eq1,eq2) -> string_of_expression (eq1) ^  " = " ^ string_of_expression (eq2)

  and string_of_hint_option (hint: hint option) =
    match hint with
    | Some Axiom -> " (*hint: axiom*)"
    | Some Induction x -> " (*hint: induction  " ^ x ^ "*)"
    | None -> ""

    let rec string_of_pattern (p : pattern) : string =
      match p with
      | Cons (name, patterns) ->
          name ^ " (" ^ String.concat ", " (List.map string_of_pattern patterns) ^ ")"
      | Var (name, type_name) -> name ^ " : " ^ type_name