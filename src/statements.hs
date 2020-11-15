module Statement where

import Lexer
import Token
import Text.Parsec
import Memory
import Data.Functor.Identity

stmts :: ParsecT [Token] [Type] Data.Functor.Identity.Identity [Token]
stmts = do
        first <- assign <|> ifStatement <|> whileStatement <|> forStatement
        next  <- remaining_stmts
        return (first ++ next) <|> (return [])

remaining_stmts :: ParsecT [Token] [Type] Data.Functor.Identity.Identity [Token]
remaining_stmts = (do a <- stmts <|> endProgram
                      return a) <|> (return [])

endProgram :: ParsecT [Token] st Identity [Token]
endProgram = do
           a <- endToken
           eof
           return ([a])

generalStatement :: String -> ParsecT [Token] [Type] Identity [Token]
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

whileStatement :: ParsecT [Token] [Type] Identity [Token]
whileStatement = generalStatement "while"

ifStatement :: ParsecT [Token] [Type] Identity [Token]
ifStatement = do
              a <- try ifElseStatement <|> onlyIfStatement
              return a

onlyIfStatement :: ParsecT [Token] [Type] Identity [Token]
onlyIfStatement = generalStatement "if"

ifElseStatement :: ParsecT [Token] [Type] Identity [Token]
ifElseStatement = do
    a <- onlyIfStatement
    b <- keywordToken "else"
    c <- blockBeginToken "{"
    d <- stmts
    e <- blockEndToken "}"
    return (a ++ b:c:d ++ [e])

logicStatement :: ParsecT [Token] u Identity [Token]
logicStatement = do
    a <- idToken <|> floatToken <|> intToken
    b <- comparativeOpToken
    c <- idToken <|> floatToken <|> intToken
    return (a:b:[c])

forStatement :: ParsecT [Token] [Type] Identity [Token]
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

assign :: ParsecT [Token] [Type] Identity [Token]
assign = do
        a <- instAssign <|> justAssign
        return a

instAssign :: ParsecT [Token] [Type] Identity [Token]
instAssign = do
          a <- primitiveTypeToken
          b <- idToken
          c <- assignToken
          d <- operation <|> singletonToken <|> array
          e <- semicolonToken
        --   setArgument(a b [d])
          updateState (symtableInsert (MyArray [(MyInt 1, [1])] "a" "escopo" [1]))
          return (a:b:c:d ++ [e])

justAssign :: Parsec [Token] st [Token]
justAssign = do
          a <- idToken
          b <- assignToken
          c <- operation <|> singletonToken <|> array
          d <- semicolonToken
          return (a:b:c ++ [d])

operation :: Parsec [Token] st [Token]
operation = do
    a <- singletonToken <|> array
    b <- remaining_operations
    return (a ++ b) <|> (return []) 

remaining_operations :: Parsec [Token] st [Token]
remaining_operations = (do
    a <- opToken
    b <- operation
    return (a:b)) <|> (return [])