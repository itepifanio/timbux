{
    module Lexer where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$op = [\#\=\+\-\*]      -- operacoes
$whitespace = [\ \t\b]

tokens :-

  $white+          ;
  "--".*           ;
  $digit+          { \p s -> Int p (read s) }
  $op              { \p s -> Op p (head s) }
  $digit+\.$digit+ { \p s -> Float p (read s) }
  (int|float|string|array)$whitespace[$alpha $digit \_ \']* { \p s -> Var p s}
  $alpha+ { \p s -> Name p s }
  \"$alpha+\" { \p s -> String p s}


{

data Token =
	In    AlexPosn		  |
	Op    AlexPosn Char	  |
	Int   AlexPosn Int    |
    Var   AlexPosn String |
    Name  AlexPosn String |
    String AlexPosn String |
    Float AlexPosn Double
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
