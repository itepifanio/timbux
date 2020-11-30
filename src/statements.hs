module Statement where

import Lexer
import Token
import Text.Parsec
import Memory
import Data.Functor.Identity

import Control.Monad.IO.Class
import System.IO.Unsafe

stmts :: ParsecT [Token] [Type] IO [Token]
stmts = do
        first <- assign <|> ifStatement <|> whileStatement <|> forStatement <|> function
        next  <- remaining_stmts
        return (first ++ next) <|> (return [])

remaining_stmts :: ParsecT [Token] [Type] IO [Token]
remaining_stmts = (do a <- stmts <|> endProgram
                      return a) <|> (return [])

endProgram :: ParsecT [Token] [Type] IO([Token])
endProgram = do
           a <- endToken
           eof
           return ([a])

generalStatement :: String -> String -> ParsecT [Token] [Type] IO([Token])
generalStatement stmt endstmt = do
    a <- keywordToken stmt
    b <- blockBeginToken "("
    c <- idToken <|> floatToken <|> intToken
    d <- comparativeOpToken
    e <- idToken <|> floatToken <|> intToken
    f <- blockEndToken    ")"
    g <- stmts
    h <- keywordToken endstmt
    return (a:b:c:d:e:f:g++[h])

whileStatement :: ParsecT [Token] [Type] IO([Token])
whileStatement = generalStatement "while" "endwhile"

ifStatement :: ParsecT [Token] [Type] IO([Token])
ifStatement = do
              a <- try ifElseStatement <|> onlyIfStatement
              return a

onlyIfStatement :: ParsecT [Token] [Type] IO([Token])
onlyIfStatement = generalStatement "if" "endif"

ifElseStatement :: ParsecT [Token] [Type] IO([Token])
ifElseStatement = do
    a <- generalStatement "if" "else"
    b <- stmts
    c <- keywordToken "endif"
    return (a ++ b++[c])

logicStatement :: ParsecT [Token] [Type] IO([Token])
logicStatement = do
    a <- idToken <|> floatToken <|> intToken
    b <- comparativeOpToken
    c <- idToken <|> floatToken <|> intToken
    return (a:b:[c])

forStatement :: ParsecT [Token] [Type] IO([Token])
forStatement = do
    a <- keywordToken "for"
    b <- blockBeginToken "("
    c <- assign
    d <- logicStatement
    e <- semicolonToken
    f <- logicStatement
    l <- blockEndToken  ")"
    m <- stmts
    n <- keywordToken "endfor"
    return ((a:b:c) ++ d ++ [e] ++ f ++ (l:m++[n]))

singletonToken:: ParsecT [Token] [Type] IO([Token])
singletonToken = do
            a <- intToken <|> floatToken <|> booleanToken <|> stringToken <|> idToken
            return([a])

assign :: ParsecT [Token] [Type] IO([Token])
assign = do
        a <- instAssign <|> justAssign
        return a

instAssign :: ParsecT [Token] [Type] IO([Token])
instAssign = do
          a <- primitiveTypeToken
          b <- idToken
          c <- assignToken
          d <- operation <|> singletonToken <|> array
          e <- semicolonToken
        --   updateState (symtableInsert (MyArray [(MyInt 1, [1])] "a" "escopo" [1]))
          updateState (symtableInsert (fromToken d (getVariableName b) "")) -- TODO::recuperar escopo
          s <- getState
          liftIO (print s)
          return (a:b:c:d ++ [e])

justAssign :: ParsecT [Token] [Type] IO [Token]
justAssign = do
          a <- idToken
          b <- assignToken
          c <- operation <|> singletonToken <|> array
          d <- semicolonToken
          updateState (symtableUpdate (fromToken c (getVariableName a) "")) -- TODO::recuperar escopo
          s <- getState
          liftIO (print s)
          return (a:b:c ++ [d])

operation :: ParsecT [Token] [Type] IO [Token]
operation = do
    a <- singletonToken <|> array
    b <- remaining_operations
    return (a ++ b) <|> (return []) 

remaining_operations :: ParsecT [Token] [Type] IO [Token]
remaining_operations = (do
    a <- opToken
    b <- operation
    return (a:b)) <|> (return [])

function :: ParsecT [Token] [Type] IO [Token]
function = do
    a <- funToken
    b <- idToken
    c <- blockBeginToken "("
    d <- arguments
    e <- blockEndToken ")"
    f <- colonToken
    g <- primitiveTypeToken
    h <- stmts
    i <- returnStatement
    j <- endFunToken
    k <- semicolonToken
    return (a:b:c:d ++ [e] ++ (f:g:h) ++ i ++ (j:k:[]))

returnStatement :: ParsecT [Token] [Type] IO [Token]
returnStatement = (do
        a <- keywordToken "return"
        b <- idToken <|> floatToken <|> intToken
        c <- semicolonToken
        return (a:b:[c])) <|> (return [])

arguments :: ParsecT [Token] [Type] IO [Token]
arguments = (do
        a <- primitiveTypeToken
        b <- idToken
        c <- remainingArguments
        return (a:b:c)) <|> (return [])

remainingArguments :: ParsecT [Token] [Type] IO [Token]
remainingArguments = (do
    a <- commaToken ","
    b <- arguments
    return (a:b)) <|> (return [])

