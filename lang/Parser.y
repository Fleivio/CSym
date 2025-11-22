{
{-# LANGUAGE DeriveGeneric, PatternSynonyms, StandaloneDeriving #-}
module Parser(parseTokens) where
import Lexer
import Ast
import Data.Complex
import Data.Number.CReal
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  'U'       { (TK_UNIT_TYPE, _, _)}
  'unit'    { (TK_UNIT_TERM, _, _)}
  '('       { (TK_LPAREN, _, _)}
  ')'       { (TK_RPAREN, _, _)}
  '['       { (TK_LBRACK, _, _)}
  ']'       { (TK_RBRACK, _, _)}
  '<'       { (TK_LANG, _, _) }
  '>'       { (TK_RANG, _, _)} 
  ','       { (TK_COMMA, _, _)}
  '.'       { (TK_DOT, _, _)}
  '+'       { (TK_SUM, _, _)}
  '*'       { (TK_PROD, _, _)}
  'inl'     { (TK_INL, _, _)}
  'inr'     { (TK_INR, _, _)}
  '<->'     { (TK_RL_ARROW, _, _)}
  '<=>'     { (TK_RL_FAT_ARROW, _, _)}
  '->'      { (TK_RARROW, _, _)}
  '=>'      { (TK_FAT_ARROW, _, _)}
  '|'       { (TK_BAR, _, _)}
  '='       { (TK_EQ, _, _)}
  ':'       { (TK_COLON, _, _)}
  '::'      { (TK_DOUB_COLON, _, _)}
  'lam'     { (TK_LAM, _, _)}
  'mu'      { (TK_MU, _, _)}
  'let'     { (TK_LET, _, _)}
  'in'      { (TK_IN, _, _)}
  ';'       { (TK_DELIM, _, _)}
  'def'     { (TK_DEF, _, _) }
  '\^'      { (TK_ADJ, _, _) }


  'sqrt'    { (TK_SQRT, _, _)}
  '/'       { (TK_DIV, _, _)}
  '**'      { (TK_POW, _, _)}
  '-'       { (TK_SUB, _, _)}
  'pi'      { (TK_PI, _, _)}
  'e'       { (TK_E, _, _)}
  'i'       { (TK_I, _, _)}

  NUM       { ((TK_NUM $$), _, _)}
  VAR_ID    { ((TK_ID_LOW $$), _, _)}
  TYPE_ID   { ((TK_ID_UP $$), _, _)}

%nonassoc '<->' '<=>' '=' '|' '(' ')' '[' ']' '<' '>' ':' ';' 'in' 'let' 'lam' 'mu' 'def' 'typedef' 'U' 'unit' '\^' 
%left '+' '-'
%left '*' '/'
%left '**' 'sqrt'
%right '.' ','
%right '->' '=>' '::'
%%



Program : Body {$1}

Body : 'def' IsoDef ';' Body {$2 : $4}
     | 'def' TermDef ';' Body {$2 : $4}
     | 'def' TypeDef ';' Body {$2 : $4}
     | { [] }

TypeDef : TYPE_ID '=' Type {Def_Type $1 $3} 

TermDef : VAR_ID ':' Type '=' Term {Def_Term $1 $3 $5}

IsoDef : VAR_ID ':' IsoType '=' Iso  {Def_Iso $1 $3 $5}

IsoType : Type '<->' Type {IT_Iso $1 $3}
        | IsoType '->' IsoType {IT_Func $1 $3}
        | '(' IsoType ')' {$2}

Type : 'U' {Ty_U}
     | Type '+' Type {Ty_Sum $1 $3}
     | Type '*' Type {Ty_Prod $1 $3}
     | '[' Type ']' {Ty_List $2}
     | TYPE_ID {Ty_Var $1}
     | '(' Type ')' {$2} 

Value : 'unit' {V_Unit}
      | '[' ']' {V_List_Empty}
      | VAR_ID {V_Var $1}
      | 'inl' Value {V_Inl $2}
      | 'inr' Value {V_Inr $2}
      | Value '::' Value {V_List_Cons $1 $3} -- LOOKUP: Not present in original paper
      | '<' Value ',' Value '>' {V_Pair $2 $4}
      | '(' Value ')' { $2 }
  
CombValue : Value {CV_Value $1}
          | CombValue '+' CombValue {CV_Add $1 $3}
          | Scalar '*' CombValue {CV_Mult $1 $3}
          | '(' CombValue ')' { $2 }

Product : 'unit' {P_Unit}
        | VAR_ID {P_Var $1}
        | '<' Product ',' Product '>' {P_Pair $2 $4}
        | '(' Product ')' {$2}

Extended : CombValue {E_Value $1} -- LOOKUP: Maybe it should be CombTerm here instead of CombValue?
         | 'let' Product '=' Iso Product 'in' Extended {E_Assign $2 $4 $5 $7}

Iso : '|' IsoList {I_Set $2}
    | 'lam' VAR_ID '=>' Iso {I_Lam $2 $4}
    | 'mu' VAR_ID '=>' Iso {I_Mu $2 $4}
    | VAR_ID {I_Name $1}
    | Iso '.' Iso {I_App $1 $3} -- LOOKUP: Original was Iso Iso, '.' added to remove ambiguity
    | Iso '\^' {I_Adj $1} -- LOOKUP: Added adjoint operator
    | '(' Iso ')' {$2}

IsoList : IsoRule {[$1]}
        | IsoRule '|' IsoList {$1 : $3}

IsoRule : Value '<=>' Extended {($1, $3)}

Term : 'unit' {T_Unit}
     | '[' TermListItems ']' {T_List $2}
     | VAR_ID {T_Var $1}
     | 'inl' Term {T_Inl $2}
     | 'inr' Term {T_Inr $2}
     | '<' Term ',' Term '>' {T_Sum $2 $4}
     | Iso Term {T_App $1 $2}
     | Term '+' Term {T_Add $1 $3}
     | Scalar '*' Term {T_Mult $1 $3}
     | 'let' Product '=' Term 'in' Term {T_Ext $2 $4 $6}
     | Term '::' Term {T_List_Cons $1 $3}
     | '(' Term ')' {$2}

TermListItems : Term ',' TermListItems {$1 : $3}
               | { [ ] }
    
Scalar : NUM {(fromIntegral $1) :+ 0}
       | Scalar '+' Scalar {$1 + $3}
       | Scalar '-' Scalar {$1 - $3}
       | Scalar '*' Scalar {$1 * $3}
       | Scalar '/' Scalar {$1 / $3}
       | Scalar '**' Scalar {$1 ** $3}
       | '-' Scalar { negate $2}
       | 'sqrt' Scalar {$2 ** 0.5}
       | 'e' {exp 1}
       | 'i' {0 :+ 1}
       | 'pi' {pi}
       | '(' Scalar ')' {$2}

{



parseError ((tkn, AlexPn ab line col, lit):xs) 
  = error $ "\nParsing error:" 
    ++ "\nAt line : " ++ show line
    ++ "\nAt position: " ++ show col
    ++ "\nUnexpected: " ++ show tkn
    ++ "\nOn literal: " ++ show lit
}
