{
  module Main (main) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$op = [\#\=\+\-\*]      -- operacoes
$whitespace = [\ \t\b]
$blockBegin = [\(\[\{]
$blockEnd = [\)\]\}]
$comma = [\,\"\'\;]
tokens :-

  $white+                                                   ;
  "--".*                                                    ;
  Let                                                       { \p s -> Let p s } 
  Ghbc                                                      { \p s -> Ghbc p s } 
  $blockBegin                                               { \p s -> BlockBegin p (head s) } 
  $blockEnd                                                 { \p s -> BlockEnd p (head s) } 
  (\=+\=|\>+\=|\<+\=|\>|\<)                                 { \p s -> ComparativeOp p s}
  (\&+\&|\|+\||\!)                                          { \p s -> LogicalOp p s}
  $op                                                       { \p s -> Op p (head s) } 
  $digit+                                                   { \p s -> Int p (read s) }
  $digit+\.$digit+                                          { \p s -> Float p (read s) }
  (true|false)                                              { \p s -> Boolean p s }
  (int|float|string|array|boolean)                          { \p s -> PrimitiveType p s}
  (if|else|for|continue|break|while|const|var)              { \p s -> Keyword p s}
  $comma                                                    { \p s -> Comma p (head s)}        
  $alpha+                                                   { \p s -> Name p s }

{

data Token =
	  Op              AlexPosn Char	  |
    Comma           AlexPosn Char   |
	  Int             AlexPosn Int    |
    Name            AlexPosn String |
    String          AlexPosn String |
    Number          AlexPosn String |
    Boolean         AlexPosn String |
    Float           AlexPosn Double |
    Let             AlexPosn String |
    Ghbc            AlexPosn String |
    PrimitiveType   AlexPosn String |
    BlockBegin      AlexPosn Char	  |
    BlockEnd        AlexPosn Char	  |
    Keyword         AlexPosn String |
    ComparativeOp   AlexPosn String |
    LogicalOp       AlexPosn String 
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
