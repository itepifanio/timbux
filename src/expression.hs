module Expression where

import Lexer
import Token
import Text.Parsec
import Memory

import Control.Monad.IO.Class
import System.IO.Unsafe

-- funções para o avaliador de expressões

expression :: ParsecT [Token] [Type] IO [Token]
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] [Type] IO [Token]
una_expression = literal_values <|> literal_from_name

literal_values :: ParsecT [Token] [Type] IO [Token]  -- TODO
literal_values =  do
                    a <- many $ intToken <|> floatToken <|> stringToken
                    liftIO (print "literal_values")
                    return (a)

literal_from_name :: ParsecT [Token] [Type] IO [Token] -- TODO
literal_from_name =  do
                    a <- idToken
                    s1 <- getState
                    return (fromType ( symtableSearch s1 (getVariableName a) "" )) 

bin_expression :: ParsecT [Token] [Type] IO [Token]
bin_expression = do
                   n1 <- many (intToken <|> floatToken <|> stringToken) <|> literal_from_name
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: [Token] -> ParsecT [Token] [Type] IO [Token]
eval_remaining n1 = do
                      op <- many $ addToken <|> subToken <|> multToken
                      n2 <- many(intToken <|> floatToken <|> stringToken) <|> literal_from_name
                      result <- eval_remaining (map (\x -> eval (first x) (second x) (third x)) (zip3 n1 op n2))
                      liftIO (print "eval_remaining")
                      return (result) 
                    <|> return (n1) 

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

logicExpression :: ParsecT [Token] [Type] IO [Token]
logicExpression = do
    a <- many(floatToken <|> intToken) <|> expression
    b <- many $ comparativeOpToken
    c <- many(floatToken <|> intToken) <|> expression
    result <- logic_remaining $ all (== True) (map (\x -> logicComparative (first x) (second x) (third x)) (zip3 a b c))
    return result

logic_remaining :: Bool -> ParsecT [Token] [Type] IO [Token]
logic_remaining bool = (do
    a <- many $ logicalOpToken 
    b <- many(floatToken <|> intToken)  <|> expression
    c <- many $ comparativeOpToken
    d <- many (floatToken <|> intToken) <|> expression
    result <- logic_remaining  (logicOperation bool (a!!0) (all (==True) (map (\x -> logicComparative (first x) (second x) (third x)) (zip3 b c d))))
    return (result)) <|> (return [boolToToken bool])

boolToToken :: Bool -> Token 
boolToToken True = (Boolean "True")
boolToToken False = (Boolean "False")

tokenToBool :: Token -> Bool
tokenToBool (Boolean "True") = True
tokenToBool (Boolean "False") = False

logicOperation :: Bool -> Token -> Bool -> Bool
logicOperation a (LogicalOp "&&") b = a && b
logicOperation a (LogicalOp "||") b = a || b

logicComparative :: Token -> Token -> Token -> Bool
logicComparative (Int x) (ComparativeOp ">") (Int y) = x > y
logicComparative (Float x) (ComparativeOp ">") (Float y) = x > x
logicComparative (Int x) (ComparativeOp ">") (Float y) = fromIntegral x > y
logicComparative (Float x) (ComparativeOp ">") (Int y) = x > fromIntegral y
logicComparative (Int x) (ComparativeOp ">=") (Int y) = x >= y
logicComparative (Float x) (ComparativeOp ">=") (Float y) = x >= x
logicComparative (Int x) (ComparativeOp ">=") (Float y) = fromIntegral x >= y
logicComparative (Float x) (ComparativeOp ">=") (Int y) = x >= fromIntegral y
logicComparative (Int x) (ComparativeOp "<") (Int y) = x < y
logicComparative (Float x) (ComparativeOp "<") (Float y) = x < x
logicComparative (Int x) (ComparativeOp "<") (Float y) = fromIntegral x < y
logicComparative (Float x) (ComparativeOp "<") (Int y) = x < fromIntegral y
logicComparative (Int x) (ComparativeOp "<=") (Int y) = x <= y
logicComparative (Float x) (ComparativeOp "<=") (Float y) = x <= x
logicComparative (Int x) (ComparativeOp "<=") (Float y) = fromIntegral x <= y
logicComparative (Float x) (ComparativeOp "<=") (Int y) = x <= fromIntegral y
logicComparative (Int x) (ComparativeOp "==") (Int y) = x == y
logicComparative (Float x) (ComparativeOp "==") (Float y) = x == x
logicComparative (Int x) (ComparativeOp "==") (Float y) = fromIntegral x == y
logicComparative (Float x) (ComparativeOp "==") (Int y) = x == fromIntegral y

-- TODO::mover daqui
first  (a, _, _) = a
second (_, a, _) = a
third  (_, _, a) = a