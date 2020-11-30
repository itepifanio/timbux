module Evaluation where

import Lexer
import Token
import Text.Parsec

bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   n1 <- intToken <|> floatToken    -- aqui adicionar pra receber qualquer tipo
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining n1 = do
                      op <- opToken
                      n2 <- intToken <|> floatToken   -- aqui adicionar pra receber qualquer tipo
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1)     

eval :: Token -> Token -> Token -> Token -- aqui adicionar pra receber qualquer tipo
eval (Int x) (Add) (Int y) = Int (x + y)
eval (Int x) (Sub) (Int y) = Int (x - y)
eval (Int x) (Mult) (Int y) = Int (x * y)