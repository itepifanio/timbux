module Expression where

import Lexer
import Token
import Text.Parsec
import Memory


import Control.Monad.IO.Class
import System.IO.Unsafe

-- funções para o avaliador de expressões

expression :: ParsecT [Token] [Type] IO(Token)
expression = try bin_expression  <|> una_expression

una_expression :: ParsecT [Token] [Type] IO(Token)
una_expression = literal_values <|> try literal_from_array <|> literal_from_name

literal_values :: ParsecT [Token] [Type] IO(Token)  -- TODO
literal_values =  do
                    a <- intToken <|> floatToken <|> stringToken
                    return (a)

literal_from_name :: ParsecT [Token] [Type] IO(Token) -- TODO
literal_from_name =  do
                    a <- idToken
                    s1 <- getState
                    return (fromTypeX ( fst (symtableSearch s1 (getVariableName a) "" ))) 

arrayAccess :: ParsecT [Token] [Type] IO [Token]
arrayAccess = 
    (do
    a <- idToken
    b <- blockBeginToken "["
    c <- intToken 
    d <- blockEndToken "]"
    e <- semicolonToken
    return (a:b:c:d:e:[])) <|>
    (do
    a <- idToken
    b <- blockBeginToken "["
    c <- intToken
    d <- blockEndToken "]"
    return (a:b:c:d:[]))

positionSequence = do
        b <- blockBeginToken "["
        c <- intToken <|> literal_from_name
        d <- blockEndToken "]"
        next <- remaining_positions
        return(b:c:[d]++next)

-- remaining_positions :: ParsecT [Token] u IO [Token]
remaining_positions = (do 
                       a <- positionSequence
                       return (a)) <|> (return [])

literal_from_array:: ParsecT [Token] [Type] IO(Token)
literal_from_array =  do
                    a <- idToken
                    b <- positionSequence
                    s1 <- getState
                    return (fromTypeX ( fst (symtableArraySearch s1 (getIndexes b []) (getVariableName a) "" ))) 
                    
getIndexes:: [Token] -> [Int] -> [Int]
getIndexes [] indexes = indexes
getIndexes ((index):t) indexes = 
    if isInt index then indexes ++ (getValueInt index) : getIndexes t indexes
    else getIndexes t indexes

isInt :: Token -> Bool
isInt (Lexer.Int a) = True
isInt _ = False

getValueInt :: Token -> Int
getValueInt (Lexer.Int a) = a

bin_expression :: ParsecT [Token] [Type] IO(Token)
bin_expression = do
                   n1 <- intToken <|> floatToken <|> stringToken <|> try literal_from_array <|> literal_from_name
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] [Type] IO(Token)
eval_remaining n1 = do
                      op <- addToken <|> subToken <|> multToken
                      n2 <- intToken <|> floatToken <|> stringToken <|> try literal_from_array <|> literal_from_name
                      result <- eval_remaining (eval n1 op n2)
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

logicExpression :: ParsecT [Token] [Type] IO([Token])
logicExpression = do
    a <- floatToken <|> intToken <|> expression <|> booleanToken
    b <- comparativeOpToken
    c <- floatToken <|> intToken <|> expression <|> booleanToken
    result <- logic_remaining (logicComparative a b c)
    return result

logic_remaining :: Bool -> ParsecT [Token] [Type] IO([Token])
logic_remaining bool = (do
    a <- logicalOpToken 
    b <- floatToken <|> intToken <|> expression <|> booleanToken
    c <- comparativeOpToken
    d <- floatToken <|> intToken <|> expression <|> booleanToken
    result <- logic_remaining  (logicOperation bool a (logicComparative b c d))
    return (result)) <|> (return [boolToToken bool])

boolToToken :: Bool -> Token 
boolToToken True = (Boolean "true")
boolToToken False = (Boolean "false")

tokenToBool :: Token -> Bool
tokenToBool (Boolean "true") = True
tokenToBool (Boolean "false") = False

logicOperation :: Bool -> Token -> Bool -> Bool
logicOperation a (LogicalOp "&&") b = a && b
logicOperation a (LogicalOp "||") b = a || b

logicComparative :: Token -> Token -> Token -> Bool
logicComparative (Int x) (ComparativeOp ">") (Int y) = x > y
logicComparative (Float x) (ComparativeOp ">") (Float y) = x > y
logicComparative (Int x) (ComparativeOp ">") (Float y) = fromIntegral x > y
logicComparative (Float x) (ComparativeOp ">") (Int y) = x > fromIntegral y
logicComparative (Int x) (ComparativeOp ">=") (Int y) = x >= y
logicComparative (Float x) (ComparativeOp ">=") (Float y) = x >= y
logicComparative (Int x) (ComparativeOp ">=") (Float y) = fromIntegral x >= y
logicComparative (Float x) (ComparativeOp ">=") (Int y) = x >= fromIntegral y
logicComparative (Int x) (ComparativeOp "<") (Int y) = x < y
logicComparative (Float x) (ComparativeOp "<") (Float y) = x < y
logicComparative (Int x) (ComparativeOp "<") (Float y) = fromIntegral x < y
logicComparative (Float x) (ComparativeOp "<") (Int y) = x < fromIntegral y
logicComparative (Int x) (ComparativeOp "<=") (Int y) = x <= y
logicComparative (Float x) (ComparativeOp "<=") (Float y) = x <= y
logicComparative (Int x) (ComparativeOp "<=") (Float y) = fromIntegral x <= y
logicComparative (Float x) (ComparativeOp "<=") (Int y) = x <= fromIntegral y
logicComparative (Int x) (ComparativeOp "==") (Int y) = x == y
logicComparative (Float x) (ComparativeOp "==") (Float y) = x == y
logicComparative (Int x) (ComparativeOp "==") (Float y) = fromIntegral x == y
logicComparative (Float x) (ComparativeOp "==") (Int y) = x == fromIntegral y
logicComparative (Boolean x) (ComparativeOp "==") (Boolean y) = stringToBool x == stringToBool y
logicComparative (Boolean x) (ComparativeOp "!=") (Boolean y) = stringToBool x /= stringToBool y
