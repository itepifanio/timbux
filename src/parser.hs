module Main (main) where

import Lexer
import Token
import Memory
import Statement
import Text.Parsec
import Data.Functor.Identity
import System.IO.Unsafe
import System.Environment

program :: ParsecT [Token] [Type] IO [Token]
program = do
        updateState(symtableInsert (MyType (MyInt 1) "asdasldj==@#!" ""))
        a <- beginToken 
        b <- idToken -- nome do programass
        c <- stmts
        eof
        return ([a] ++ [b] ++ c)

endProgram :: ParsecT [Token] [Type] IO [Token]
endProgram = do
           a <- endToken
           eof
           return ([a])

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = do {
          file <- getArgs;
          case unsafePerformIO (parser (getTokens (file!!0))) of
          { Left err -> print err; 
            Right ans -> putStr ""
          }
}