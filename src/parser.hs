module Main (main) where

import Lexer
import Token
import Memory
import Statement
import Text.Parsec
import Data.Functor.Identity

program :: ParsecT [Token] [Type] Identity [Token]
program = do
        a <- beginToken 
        b <- idToken -- nome do programa
        c <- stmts
        eof
        return ([a] ++ [b] ++ c)

endProgram :: ParsecT [Token] st Identity [Token]
endProgram = do
           a <- endToken
           eof
           return ([a]) 

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program [] "Error message" tokens

main :: IO ()
main = case parser (getTokens "./program/programv0.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }