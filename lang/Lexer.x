{
module Lexer where
}
%wrapper "posn"
$char = [a-zA-Z0-9\']
$digit = [0-9]
$backslash = [\\]
$adj = [\^]


tokens :-

  $white+                ;
  "--".*                 ;

  "U"                    { tokenMk TK_UNIT_TYPE }
  "unit"                 { tokenMk TK_UNIT_TERM } 

  "("                    { tokenMk TK_LPAREN }
  ")"                    { tokenMk TK_RPAREN }
  "["                    { tokenMk TK_LBRACK }
  "]"                    { tokenMk TK_RBRACK }
  "<"                    { tokenMk TK_LANG }
  ">"                    { tokenMk TK_RANG }
  ","                    { tokenMk TK_COMMA }

  "+"                    { tokenMk TK_SUM }
  "*"                    { tokenMk TK_PROD }

  "inl"                  { tokenMk TK_INL }
  "inr"                  { tokenMk TK_INR }

  "let"                  { tokenMk TK_LET }
  "in"                   { tokenMk TK_IN }
  "="                    { tokenMk TK_EQ }
  ":"                    { tokenMk TK_COLON }
  "::"                   { tokenMk TK_DOUB_COLON }
  "."                    { tokenMk TK_DOT}
  $adj                   { tokenMk TK_ADJ }
  "def"                  { tokenMk TK_DEF}

  "lam" | $backslash     { tokenMk TK_LAM }
  "mu" | "rec"           { tokenMk TK_MU}
  "=>"                   { tokenMk TK_FAT_ARROW }
  "->"                   { tokenMk TK_RARROW }

  "<->"                  { tokenMk TK_RL_ARROW }
  "<=>"                  { tokenMk TK_RL_FAT_ARROW }
  "|"                    { tokenMk TK_BAR }
  
  "/"                    { tokenMk TK_DIV}
  "sqrt" | "~/"          { tokenMk TK_SQRT}
  "**"                   { tokenMk TK_POW}
  "-"                    { tokenMk TK_SUB}
  "pi"                   { tokenMk TK_PI}
  "e"                    { tokenMk TK_E}
  "i"                    { tokenMk TK_I}
--TODO: trig functions

  [a-z]$char*            { \i c -> tokenMk (TK_ID_LOW c) i c }
  [A-Z]$char*            { \i c -> tokenMk (TK_ID_UP c) i c }
  $digit+                { \i c -> tokenMk (TK_NUM (read c)) i c }
  ";"                    { tokenMk TK_DELIM }
  "\n"                   ;

  .                      { \i c -> error $ "Token error: " ++ show c ++ "\nAt Position:" ++ show i }

{
data TokenId
  = TK_UNIT_TYPE
  | TK_UNIT_TERM
  | TK_LPAREN | TK_RPAREN | TK_COMMA
  | TK_LBRACK | TK_RBRACK 
  | TK_SUM | TK_PROD
  | TK_INL | TK_INR
  | TK_LANG | TK_RANG
  | TK_ADJ
  | TK_LAM
  | TK_COLON
  | TK_DOUB_COLON
  | TK_DOT
  | TK_MU
  | TK_DEF
  | TK_FAT_ARROW
  | TK_LET | TK_IN | TK_EQ
  | TK_RL_ARROW
  | TK_RL_FAT_ARROW
  | TK_RARROW
  | TK_BAR
  | TK_ID_LOW String
  | TK_ID_UP String
  | TK_NUM Int
  | TK_DELIM
  | TK_DIV
  | TK_SQRT
  | TK_POW
  | TK_SUB
  | TK_PI
  | TK_I
  | TK_E
  deriving (Show, Eq)

type Token = (TokenId, AlexPosn, String)

tokenMk :: TokenId -> AlexPosn -> String -> Token
tokenMk a b c = (a, b, c)

}