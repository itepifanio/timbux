module Main (main) where

import Lexer
import Token
import Memory
import Statement
import Text.Parsec
import Data.Functor.Identity
import System.IO.Unsafe

program :: ParsecT [Token] [Type] IO [Token]
program = do
        updateState(symtableInsert (MyType (MyInt 1) "asdasldj==@#!" ""))
        a <- beginToken 
        b <- idToken -- nome do programa
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
main = case unsafePerformIO (parser (getTokens "./program/problema2.pe")) of
            { Left err -> print err; 
              Right ans -> print ans
            }