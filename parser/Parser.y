{
module Parser(parseTokens) where
import Lexer

}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  '()'      { (TK_UNIT, _, _)}
  '1'       { (TK_1, _, _)}
  '('       { (TK_LPAREN, _, _)}
  ')'       { (TK_RPAREN, _, _)}
  ','       { (TK_COMMA, _, _)}
  '+'       { (TK_SUM, _, _)}
  '*'       { (TK_PROD, _, _)}
  'inl'     { (TK_INL, _, _)}
  'inr'     { (TK_INR, _, _)}
  '{'       { (TK_LBRACE, _, _)}
  '}'       { (TK_RBRACE, _, _)}
  '['       { (TK_LBRACK, _, _)}
  ']'       { (TK_RBRACK, _, _)}
  '<->'     { (TK_ISO, _, _)}
  '->'      { (TK_RARROW, _, _)}
  '|'       { (TK_BAR, _, _)}
  '='       { (TK_EQ, _, _)}
  ':'       { (TK_COLON, _, _)}
  'lam'     { (TK_LAM, _, _)}
  'mu'      { (TK_MU, _, _)}
  '~>'      { (TK_CURLYARROW, _, _)}
  'let'     { (TK_LET, _, _)}
  'in'      { (TK_IN, _, _)}
  ';'       { (TK_DELIM, _, _)}
  'def'     { (TK_DEF, _, _) }
  'typedef' { (TK_TYPEDEF, _, _)}
  VAR       { ((TK_LIT $$), _, _)}

%%

Program : Body {$1}

Body : 'def' IsoDef ';' Body {$2 : $4}
     | 'def' TermDef ';' Body {$2 : $4}
     | 'typedef' TypeDef ';' Body {$2 : $4}
     | { [] }

TypeDef : VAR '=' Type {Def_Type $1 $3} 

TermDef : VAR ':' Type '=' Term {Def_Term $1 $3 $5}

IsoDef : VAR ':' IsoType '=' Iso  {Def_Iso $1 $3 $5}
       | VAR ':' IsoType '|' IsoList {Def_Iso $1 $3 (I_Set $5)}

IsoType : Type '<->' Type {IT_Iso $1 $3}
        | '(' Type '<->' Type ')' '->' IsoType {IT_Func $2 $4 $7}
        | '(' IsoType ')' {$2}

Type : '1' {Ty_One}
     | Type '+' Type {Ty_Sum $1 $3}
     | Type '*' Type {Ty_Prod $1 $3}
     | '[' Type ']' {Ty_List $2}
     | VAR {Ty_Var $1}
     | '(' Type ')' {$2} 

Value : '()' {V_Unit}
      | VAR {V_Var $1}
      | 'inl' Value {V_Inl $2}
      | 'inr' Value {V_Inr $2}
      | '(' Value ',' Value ')' {V_Pair $2 $4}

Product : '()' {P_Unit}
        | VAR {P_Var $1}
        | '(' Product ',' Product ')' {P_Pair $2 $4}

Extended : Value {E_Value $1}
         | 'let' Product '=' Iso Product 'in' Extended {E_Assign $2 $4 $5 $7}

Iso : '{' IsoList '}' {I_Set $2}
    | 'lam' VAR '~>' Iso {I_Lam $2 $4}
    | 'mu' VAR '~>' Iso {I_Mu $2 $4}
    | VAR {I_Name $1}
    | Iso Iso {I_App $1 $2}
    | '(' Iso ')' {$2}

IsoList : Value '<->' Extended {[($1, $3)]}
        | Value '<->' Extended '|' IsoList {($1, $3) : $5}

Term : '()' {T_Unit}
     | VAR {T_Var $1}
     | 'inl' Term {T_Inl $2}
     | 'inr' Term {T_Inr $2}
     | '(' Term ',' Term ')' {T_Sum $2 $4}
     | Iso Term {T_App $1 $2}
     | 'let' Product '=' Term 'in' Term {T_Ext $2 $4 $6}
     | '(' Term ')' {$2}

{
data Type
  = Ty_One
  | Ty_Sum Type Type
  | Ty_Prod Type Type
  | Ty_List Type
  | Ty_Var String
  deriving (Eq, Show)

data IsoType
  = IT_Iso Type Type
  | IT_Func Type Type IsoType
  deriving (Eq, Show)

data Value
  = V_Unit
  | V_Var String
  | V_Inl Value
  | V_Inr Value
  | V_Pair Value Value
  deriving (Eq, Show)

data Product
  = P_Unit
  | P_Var String
  | P_Pair Product Product
  deriving (Eq, Show)

data Extended
  = E_Value Value
  | E_Assign Product Iso Product Extended
  deriving (Eq, Show)

data Iso
  = I_Set [(Value, Extended)]
  | I_Lam String Iso
  | I_Mu String Iso
  | I_Name String
  | I_App Iso Iso
  deriving (Eq, Show)

data Term
  = T_Unit
  | T_Var String
  | T_Inl Term
  | T_Inr Term
  | T_Sum Term Term
  | T_App Iso Term
  | T_Ext Product Term Term
  deriving (Eq, Show)

data Def
  = Def_Iso String IsoType Iso
  | Def_Term String Type Term
  | Def_Type String Type
    deriving(Eq, Show)

type Defs = [Def]

parseError ((token, AlexPn ab line col, lit):xs) 
  = error $ "\nParsing error:" 
    ++ "\nAt line : " ++ show line
    ++ "\nAt position: " ++ show col
    ++ "\nCaused by literal: " ++ lit
}
