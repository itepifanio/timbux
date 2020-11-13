module Statement where

import Lexer
import Token
import Text.Parsec

stmts :: Parsec [Token] st [Token]
stmts = do
        first <- assign <|> ifStatement <|> whileStatement <|> forStatement
        next  <- remaining_stmts
        return (first ++ next) <|> (return [])

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do a <- stmts <|> endProgram
                      return a) <|> (return [])

endProgram :: Parsec [Token] st [Token]
endProgram = do
           a <- endToken
           eof
           return ([a])

generalStatement :: String -> Parsec [Token] st [Token]
generalStatement stmt = do
    a <- keywordToken stmt
    b <- blockBeginToken "("
    c <- idToken <|> floatToken <|> intToken
    d <- comparativeOpToken
    e <- idToken <|> floatToken <|> intToken
    f <- blockEndToken    ")"
    g <- blockBeginToken "{"
    h <- stmts
    i <- blockEndToken   "}"
    return (a:b:c:d:e:f:g:h ++ [i])

whileStatement :: Parsec [Token] st [Token]
whileStatement = generalStatement "while"

ifStatement :: Parsec [Token] st [Token]
ifStatement = do
              a <- try ifElseStatement <|> onlyIfStatement
              return a

onlyIfStatement :: Parsec [Token] st [Token]
onlyIfStatement = generalStatement "if"

ifElseStatement :: Parsec [Token] st [Token]
ifElseStatement = do
    a <- onlyIfStatement
    b <- keywordToken "else"
    c <- blockBeginToken "{"
    d <- stmts
    e <- blockEndToken "}"
    return (a ++ b:c:d ++ [e])

logicStatement :: Parsec [Token] st [Token]
logicStatement = do
    a <- idToken <|> floatToken <|> intToken
    b <- comparativeOpToken
    c <- idToken <|> floatToken <|> intToken
    return (a:b:[c])

forStatement :: Parsec [Token] st [Token]
forStatement = do
    a <- keywordToken "for"
    b <- blockBeginToken "("
    c <- assign
    d <- logicStatement
    e <- semicolonToken
    f <- logicStatement
    l <- blockEndToken  ")"
    m <- blockBeginToken "{"
    n <- stmts
    o <- blockEndToken  "}"
    return ((a:b:c) ++ d ++ [e] ++ f ++ (l:m:n ++ [o]))

singletonToken:: Parsec [Token] st [Token]
singletonToken = do
            a <- intToken <|> floatToken <|> booleanToken <|> stringToken <|> idToken
            return([a])

assign :: Parsec [Token] st [Token]
assign = do
        a <- instAssign <|> justAssign
        return a;

instAssign :: Parsec [Token] st [Token]
instAssign = do
          a <- primitiveTypeToken
          b <- idToken
          c <- assignToken
          d <- operation <|> singletonToken <|> array
          e <- semicolonToken
          return (a:b:c:d ++ [e])

justAssign :: Parsec [Token] st [Token]
justAssign = do
          a <- idToken
          b <- assignToken
          c <- operation <|> singletonToken <|> array
          d <- semicolonToken
          return (a:b:c ++ [d])

justInst :: Parsec [Token] st [Token]
justInst = do
          a <- primitiveTypeToken
          b <- idToken
          return (a:[b])

operation :: Parsec [Token] st [Token]
operation = do
    a <- singletonToken <|> array
    b <- remaining_operations
    return (a ++ b) <|> (return []) 

--argument :: Parsec [Token] st [Token]
--argument = (do a <- justInst
--               b <- commaToken ","
--               return a:[b]) <|> (return [])

arguments :: Parsec [Token] st [Token]
arguments = do
        a <- justInst
        return (a++[])<|>(return [])

--function :: Parsec [Token] st [Token]
--operation = do
--    a <- primitiveTypeToken 


remaining_operations :: Parsec [Token] st [Token]
remaining_operations = (do
    a <- opToken
    b <- operation
    return (a:b)) <|> (return [])


