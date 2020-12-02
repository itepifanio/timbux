module Evaluation where

import Lexer
import Token
import Text.Parsec
import Memory

import Control.Monad.IO.Class
import System.IO.Unsafe

-- funções para o avaliador de expressões

expression :: ParsecT [Token] [Type] IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] [Type] IO(Token)
una_expression = una_expression_values <|> una_expression_names

una_expression_values :: ParsecT [Token] [Type] IO(Token)
una_expression_values =  do
                    -- op <- addToken <|> subToken <|> multToken
                    a <- intToken <|> floatToken <|> stringToken
                    return (a) 

una_expression_names :: ParsecT [Token] [Type] IO(Token)
una_expression_names =  do
                    -- op <- addToken <|> subToken <|> multToken
                    a <- idToken
                    s1 <- getState
                    return (fromTypeX ( symtableSearch s1 (getVariableName a) "" )) 

bin_expression :: ParsecT [Token] [Type] IO(Token)
bin_expression = do
                   n1 <- intToken <|> floatToken <|> stringToken
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] [Type] IO(Token)
eval_remaining n1 = do
                      op <- addToken <|> subToken <|> multToken
                      n2 <- intToken <|> floatToken <|> stringToken <|> idToken
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1) 

-- symtableShow :: ParsecT [Token] [Type] IO [Type] --TODO
-- symtableShow = do
--                   s1 <- getState
--                   liftIO (print s1)
--                   return(s1)

eval :: Token -> Token -> Token -> Token
eval (Int x)    (Add)   (Int y)   = Int (x + y)
eval (Int x)    (Sub)   (Int y)   = Int (x - y)
eval (Int x)    (Mult)  (Int y)   = Int (x * y)
eval (Float x)  (Add)   (Float y) = Float (x + y)
eval (Float x)  (Sub)   (Float y) = Float (x - y)
eval (Float x)  (Mult)  (Float y) = Float (x * y)
eval (Float x)  (Add)   (Int y)   = Float (x + fromIntegral y)
eval (Float x)  (Sub)   (Int y)   = Float (x - fromIntegral y)
eval (Float x)  (Mult)  (Int y)   = Float (x * fromIntegral y)
eval (Int x)    (Add)   (Float y) = Float (fromIntegral x + y)
eval (Int x)    (Sub)   (Float y) = Float (fromIntegral x - y)
eval (Int x)    (Mult)  (Float y) = Float (fromIntegral x * y)
eval (String x) (Add)   (String y)= String (x ++ y)
-- eval (Name x)   (Add)   (Name y)  = 
--   eval (fromTypeX ( symtableSearch symtableShow x "" )) (Add) (fromTypeX (MyInt 1))
    
-- procurar pelo nome na tabela de simbolos e receber o typex (com a symtableSearch)
-- pegar o typex e traduzir para tipo haskell (com a fromTypex)
-- realizar a operação entre os valores traduzidos 