module Main (main) where

import Lexer
import Token
import Text.Parsec
import Data.Functor.Identity

program :: Parsec [Token] st [Token]
program = do
        a <- beginToken 
        b <- idToken -- nome do programa
        c <- stmts
        d <- endToken
        eof
        return ([a] ++ [b] ++ c ++ [d])

stmts :: Parsec [Token] st [Token]
stmts = do
          first <- assign
          next  <- remaining_stmts
          return (first ++ next)

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do a <- semicolonToken
                      b <- assign <|> ifStatement
                      return (a:b)) <|> (return [])

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "programaV0.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }