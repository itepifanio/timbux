{
module Lexer where
import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$op = [\#\+\-\*]      -- operacoes
$whitespace = [\ \t\b]
$blockBegin = [\(\[\{]
$blockEnd = [\)\]\}]
$comma = [\,\"\']
$stringCommas = [\'\,\.\;\:\=\>\<\\\/\|\!\$\%\@]
tokens :-

  \"+($alpha|$digit|$whitespace|$blockBegin|$blockEnd|$op|$stringCommas|\&+\&|\|+\||\=+\=|\>+\=|\<+\=|true|false|int|float|string|array|boolean|matrixif|else|for|continue|break|while|const|var|"Let"|"Ghbc")+\"  { \p -> String p }
  $white+                                      ;
  "--".*                                       ;
  "Let"                                        { \p -> Let  } 
  "Ghbc"                                       { \p -> Ghbc }
  "="                                          { \p -> Assign }
  ";"                                          { \p -> Semicolon }
  (\( | \[ | \{)                               { \p -> BlockBegin p} 
  (\) | \] | \})                               { \p -> BlockEnd p} 
  (\=+\=|\>+\=|\<+\=|\>|\<)                    { \p -> ComparativeOp p}
  (\&+\&|\|+\||\!)                             { \p -> LogicalOp p}
  $op                                          { \p -> Op (head p) } 
  \-$digit+|$digit+                            { \p -> Int (read p) }
  $digit+\.$digit+                             { \p -> Float (read p) }
  (true|false)                                 { \p -> Boolean p }
  (int|float|string|array|boolean|matrix)      { \p -> PrimitiveType p}
  (if|else|for|continue|break|while|const|var) { \p -> Keyword p}
  (\, | \" | \')                               { \p -> Comma p}        
  $alpha+                                      { \p -> Name p }

{

data Token =
	  Op              Char	  |
    Comma           String  |
	  Int             Int     |
    Name            String  |
    Number          String  |
    Boolean         String  |
    Float           Double  |
    Array                   |
    Matrix                  |
    Let                     |
    Ghbc                    |
    Assign                  |
    Semicolon               |
    Quote                   |
    OpenBrackets            |
    ClosedBrackets          |
    PrimitiveType   String  |
    BlockBegin      String  |
    BlockEnd        String  |
    Keyword         String  |
    ComparativeOp   String  |
    String          String  |
    LogicalOp       String 
	deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
