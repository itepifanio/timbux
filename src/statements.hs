module Statement where

import Lexer
import Token
import Text.Parsec

stmts :: Parsec [Token] st [Token]
stmts = do
          first <- assign <|> ifStatement
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

-- general statements
ifStatement :: Parsec [Token] st [Token]
ifStatement = do
    a <- keywordToken
    b <- blockBeginToken <?> "("
    c <- idToken <|> floatToken <|> intToken
    d <- comparativeOpToken
    e <- idToken <|> floatToken <|> intToken
    f <- blockEndToken   <?> ")"
    g <- blockBeginToken <?> "{"
    h <- stmts -- adicionar as outras estruturas posteriormente
    i <- blockEndToken   <?> "}"
    return (a:b:c:d:e:f:g:h ++ [i])

assign :: Parsec [Token] st [Token]
assign = do
          a <- primitiveTypeToken
          b <- idToken
          c <- assignToken
          d <- (intToken <|> floatToken <|> booleanToken)
          e <- semicolonToken
          return (a:b:c:d:[e])