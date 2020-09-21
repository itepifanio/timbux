{
    module Lexer where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$op = [\#\=\+\-\*]      -- operacoes
$whitespace = [\ \t\b]
$equal = [\=\=]

tokens :-

  $white+                                                   ;
  "--".*                                                    ;
  $digit+                                                   { \p s -> Int p (read s) }
  $op                                                       { \p s -> Op p (head s) }
  $digit+\.$digit+                                          { \p s -> Float p (read s) }
  (int|float|string|array)$whitespace[$alpha $digit \_ \']* { \p s -> Var p s}
  $alpha+                                                   { \p s -> Name p s }
  \"$alpha+\"                                               { \p s -> String p s}
  ($digit+|$digit+\.$digit+)                                { \p s -> Number p (read s)} -- nao referenciado pelo alex
  $alpha+\=\=$alpha+                                        { \p s -> Logic p s} -- nao consegui fazer com espacos (pega valor name)
  ($digit|\+|\-|\*|\-|\#)+                                  { \p s -> Expr p s} -- sem espacos tambem
  -- (int|float|string|array)\w+\s?\=?\s?(?:(\d+|\*+|\++)*)?  { \p s -> TypeDef p s} regex pronto, mas o alex nao reconhece
  
  -- funtion regex function\((\w+|,)+\)\{
{

data Token =
	  In      AlexPosn		    |
	  Op      AlexPosn Char	  |
	  Int     AlexPosn Int    |
    Var     AlexPosn String |
    Name    AlexPosn String |
    String  AlexPosn String |
    Number  AlexPosn String |
    Logic   AlexPosn String |
    Expr    AlexPosn String |
    -- TypeDef AlexPosn String |
    Float  AlexPosn Double
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
