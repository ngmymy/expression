type expression = 
  | Identifier of string
  | Application of expression * expression
  | Match of string

type variable = 
  | Vars of string * string

type typeconstructor = 
  | Typecon of (string * string list)

type hint =
  | Axiom
  | Induction of string

type equality =
  | Equality of expression * expression

type declaration =
  | Dec of (string * (variable list) * equality * hint option)
  | Type of (string * typeconstructor list)
  | Rec of (string * (variable list) * string)

type pattern =
  | Cons of (string * pattern list)
  | Var of (string * string)