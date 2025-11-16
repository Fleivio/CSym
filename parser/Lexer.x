{
module Lexer where
}
%wrapper "posn"
$char = [a-zA-Z0-9]

tokens :-

  $white+                ;
  "--".*                 ;

  "()"                   { tokenMk TK_UNIT }
  "Unit"                 { tokenMk TK_1 }

  "("                    { tokenMk TK_LPAREN }
  ")"                    { tokenMk TK_RPAREN }
  ","                    { tokenMk TK_COMMA }


  "+"                    { tokenMk TK_SUM }
  "*"                    { tokenMk TK_PROD }

  "inl"                  { tokenMk TK_INL }
  "inr"                  { tokenMk TK_INR }

  "let"                  { tokenMk TK_LET }
  "in"                   { tokenMk TK_IN }
  "="                    { tokenMk TK_EQ }
  ":"                    { tokenMk TK_COLON }
  "mu"                   { tokenMk TK_MU}
  "def"                  { tokenMk TK_DEF}
  "typedef"              { tokenMk TK_TYPEDEF}

  "lam"                   { tokenMk TK_LAM }
  "~>"                    { tokenMk TK_CURLYARROW }
  "->"                    { tokenMk TK_RARROW }

  "{"                    { tokenMk TK_LBRACE }
  "}"                    { tokenMk TK_RBRACE }
  "<->"                  { tokenMk TK_ISO }
  "|"                    { tokenMk TK_BAR }

  $char+                 { \i c -> tokenMk (TK_LIT c) i c }
  ";"                    { tokenMk TK_DELIM }
  "\n"                   ;

  .                      { \i c -> error $ "Token error: " ++ show c ++ "\nAt Position:" ++ show i }

{
data TokenId
  = TK_UNIT
  | TK_1
  | TK_LPAREN | TK_RPAREN | TK_COMMA
  | TK_SUM | TK_PROD
  | TK_INL | TK_INR
  | TK_LAM
  | TK_COLON
  | TK_MU
  | TK_DEF
  | TK_TYPEDEF
  | TK_CURLYARROW
  | TK_LET | TK_IN | TK_EQ
  | TK_LBRACE | TK_RBRACE
  | TK_LBRACK | TK_RBRACK
  | TK_ISO
  | TK_RARROW
  | TK_BAR
  | TK_LIT String
  | TK_DELIM
  deriving (Show, Eq)

type Token = (TokenId, AlexPosn, String)

tokenMk :: TokenId -> AlexPosn -> String -> Token
tokenMk a b c = (a, b, c)

}