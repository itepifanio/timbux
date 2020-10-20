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

generalStatement :: String -> Parsec [Token] st [Token]
generalStatement stmt = do
    a <- keywordToken <?> stmt
    b <- blockBeginToken <?> "("
    c <- idToken <|> floatToken <|> intToken
    d <- comparativeOpToken
    e <- idToken <|> floatToken <|> intToken
    f <- blockEndToken   <?> ")"
    g <- blockBeginToken <?> "{"
    h <- stmts
    i <- blockEndToken   <?> "}"
    return (a:b:c:d:e:f:g:h ++ [i])

{-elseStatement :: Parsec [Token] st [Token]
elseStatement = do
    a <- keywordToken
    b <- blockBeginToken <?> "{"
    c <- stmts
    d <- blockEndToken   <?> "}"
    return (a:b:c ++ [d])-}

whileStatement :: Parsec [Token] st [Token]
whileStatement = generalStatement "while"

ifStatement :: Parsec [Token] st [Token]
ifStatement = generalStatement "if"

ifElseStatement :: Parsec [Token] st [Token]
ifElseStatement = do
    a <- keywordToken <?> "if"
    b <- blockBeginToken <?> "("
    c <- idToken <|> floatToken <|> intToken
    d <- comparativeOpToken
    e <- idToken <|> floatToken <|> intToken
    f <- blockEndToken   <?> ")"
    g <- blockBeginToken <?> "{"
    h <- stmts
    i <- blockEndToken   <?> "}"
    j <- keywordToken <?> "else"
    k <- blockBeginToken <?> "{"
    l <- stmts
    m <- blockEndToken   <?> "}"
    return (a:b:c:d:e:f:g:h++[i]++(i:j:k:l++[m] ))



-- forStatement :: Parsec [Token] st [Token]
-- forStatement = do
--     a <- keywordToken <?> "for"
--     b <- blockBeginToken <?> "("
--     c <- primitiveTypeToken
--     d <- idToken
--     e <- assignToken
--     f <- (intToken <|> floatToken <|> booleanToken)
--     g <- semicolonToken
--     h <- idToken <|> floatToken <|> intToken
--     i <- comparativeOpToken
--     j <- idToken <|> floatToken <|> intToken 
--     k <- semicolonToken
--     l <- idToken <|> floatToken <|> intToken
--     m <- comparativeOpToken
--     n <- idToken <|> floatToken <|> intToken 
--     o <- blockEndToken   <?> ")"
--     p <- blockBeginToken <?> "{"
--     q <- stmts
--     r <- blockEndToken   <?> "}"
--     return (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:q ++ [r])

-- Pq isso n rola?
-- logicStatement :: Parsec [Token] st [Token]
-- logicStatement = do
--     a <- idToken <|> floatToken <|> intToken
--     b <- comparativeOpToken
--     c <- idToken <|> floatToken <|> intToken
--     return (a:b:[c])

-- forStatement :: Parsec [Token] st [Token]
-- forStatement = do
--     a <- keywordToken <?> "for"
--     b <- blockBeginToken <?> "("
--     c <- assign
--     d <- logicStatement
--     e <- semicolonToken
--     f <- logicStatement
--     l <- blockEndToken   <?> ")"
--     m <- blockBeginToken <?> "{"
--     n <- stmts
--     o <- blockEndToken   <?> "}"
--     return ((a:b:c) ++ d ++ [e] ++ f ++ (l:m:n ++ [o]))

singletonToken:: Parsec [Token] st [Token]
singletonToken = do
            a <- intToken <|> floatToken <|> booleanToken
            return([a])

assign :: Parsec [Token] st [Token]
assign = do
          a <- primitiveTypeToken
          b <- idToken
          c <- assignToken
          d <- singletonToken <|> array 
          e <- semicolonToken
          return (a:b:c:d ++ [e])