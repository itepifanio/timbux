module Statement where

import Lexer
import Token
import Text.Parsec
import Memory
import Expression
import Data.Functor.Identity
import Control.Monad.IO.Class
import System.IO
import System.IO.Unsafe

stmts :: ParsecT [Token] [Type] IO [Token]
stmts = do
        first <- assign <|> ifStatement <|> whileStatement <|> forStatement <|> function <|> printStmt
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

inputStmt :: ParsecT [Token] [Type] IO([Token])
inputStmt = do 
    a <- keywordToken "input"
    b <- blockBeginToken "("
    c <- primitiveTypeToken -- o usuário deve informar o tipo a ser lido
    d <- blockEndToken ")"
    s1 <- getState
    if canOperate s1 then
        do
            input <- liftIO (hGetLine stdin)
            return ([convert c input])
    else return [(genericValue c)]

printStmt :: ParsecT [Token] [Type] IO([Token])
printStmt = do 
    a <- keywordToken "print"
    b <- blockBeginToken "("
    c <- expression
    d <- blockEndToken ")"
    e <- semicolonToken
    s1 <- getState
    if canOperate s1 
        then liftIO (putStrLn (getValue c))
    else updateState (symtableUpdateFlag 0)
    return (a:b:c:[d] ++ [e])

generalStatement :: String -> String -> ParsecT [Token] [Type] IO([Token])
generalStatement stmt endstmt = do
    a <- keywordToken stmt
    b <- blockBeginToken "("
    c <- logicExpression
    d <- blockEndToken    ")"
    s1 <- getState
    if canOperate s1 && tokenToBool (c!!0)
        then updateState ( symtableUpdateFlag 1 )
    else updateState ( symtableUpdateFlag 0)
    e <- stmts
    f <- keywordToken endstmt
    if canOperate s1 then 
        updateState ( symtableUpdateFlag 1)
    else updateState ( symtableUpdateFlag 0)
    return ((a:b:c) ++ [d] ++ e ++ [f])

whileStatement :: ParsecT [Token] [Type] IO([Token])
whileStatement = do
                    z <- getInput
                    a <- keywordToken "while"
                    b <- blockBeginToken "("
                    c <- logicExpression
                    if tokenToBool (c!!0)
                        then updateState (symtableUpdateFlag 1)
                    else updateState ( symtableUpdateFlag 0)
                    d <- blockEndToken  ")"
                    e <- stmts
                    f <- keywordToken "endwhile"
                    y <- getState
                    if isExecuting y then
                        do 
                            setInput z
                            aaaaaa <- whileStatement
                            return ((a:b:c) ++ [d] ++ e ++ [f]) <|> (return [])
                    else 
                        do 
                            updateState (symtableUpdateFlag 1)
                            return ((a:b:c) ++ [d] ++ e ++ [f]) <|> (return [])

ifStatement :: ParsecT [Token] [Type] IO([Token])
ifStatement = do
              a <- try ifElseStatement <|> onlyIfStatement
              return a

onlyIfStatement :: ParsecT [Token] [Type] IO([Token])
onlyIfStatement = generalStatement "if" "endif"

ifElseStatement :: ParsecT [Token] [Type] IO([Token])
ifElseStatement = do
    a <- generalStatement "if" "else"
    b <- logicExpression
    c <- keywordToken "endif"
    return (a ++ b++[c])

forStatement :: ParsecT [Token] [Type] IO([Token])
forStatement = do
    z <- getInput
    a <- keywordToken "for"
    b <- blockBeginToken "("
    c <- assign
    d <- logicExpression
    if tokenToBool (d!!0)
        then updateState (symtableUpdateFlag 1)
    else updateState (symtableUpdateFlag 0)
    e <- semicolonToken
    f <- justAssign
    l <- blockEndToken  ")"
    m <- stmts
    n <- keywordToken "endfor"
    y <- getState
    
    if canOperate y then
        do 
            setInput z
            updateState (symtableUpdateFlag 0) -- para não executar o assign e justAssign a cada intereção
            aaaaaa <- forStatement
            return ((a:b:c) ++ d ++ [e] ++ f ++ (l:m++[n])) <|> (return [])
    else 
        do 
            updateState (symtableUpdateFlag 1)
            return ((a:b:c) ++ d ++ [e] ++ f ++ (l:m++[n])) <|> (return [])

singletonToken:: ParsecT [Token] [Type] IO([Token])
singletonToken = do
            a <- expression <|> booleanToken
            return([a])

assign :: ParsecT [Token] [Type] IO([Token])
assign = do
        a <- justAssignArray <|> instAssign <|> justAssign
        b <- semicolonToken
        return (a++[b])

instAssign :: ParsecT [Token] [Type] IO([Token])
instAssign = do
          a <- primitiveTypeToken
          b <- idToken
          c <- assignToken
          d <- singletonToken <|> array <|> inputStmt
          s1 <- getState
        --   if snd (symtableSearch s1 (getVariableName b) (lookupLastScope s1)) then
        --     updateState (symtableCanUpdate (fromToken d (getVariableName b) (lookupLastScope s1)))
        --   else 
          updateState (symtableInsert (fromToken d (getVariableName b) (lookupLastScope s1)))  
          return (a:b:c:d)
-- Token -> [Int] -> String -> String -> [Type] -> [Type]
justAssignArray :: ParsecT [Token] [Type] IO [Token]
justAssignArray = do
          a <- idToken
          b <- positionSequence 
          c <- assignToken
          d <- singletonToken <|> inputStmt
          s1 <- getState
          updateState (symtableInsertIndexArray d (getIndexes b []) (getVariableName a) "")
          s2 <- getState
          liftIO (print s2)
        --   if validarTipo (getType s1 (getVariableName a) (lookupLastScope s1)) c 
        --       then updateState (symtableCanUpdate (fromToken c (getVariableName a) (lookupLastScope s1)))
        --   else fail ("Type don't match with type of variable " ++ getVariableName a)    
          return (a:b ++ [c] ++ d)

justAssign :: ParsecT [Token] [Type] IO [Token]
justAssign = do
          a <- idToken 
          b <- assignToken
          c <- singletonToken <|> array <|> inputStmt
          s1 <- getState
          if validarTipo (getType s1 (getVariableName a) (lookupLastScope s1)) c 
              then updateState (symtableCanUpdate (fromToken c (getVariableName a) (lookupLastScope s1)))
          else fail ("Type don't match with type of variable " ++ getVariableName a)    
          s2 <- getState
          return (a:b:c)

function :: ParsecT [Token] [Type] IO [Token]
function = do
    a <- funToken
    b <- idToken
    c <- blockBeginToken "("
    d <- arguments
    e <- blockEndToken ")"
    f <- colonToken
    g <- primitiveTypeToken
    updateState (symtableInsertMany $ (map (\x -> fromTokenX x (getVariableName x) (getVariableName b)) (filter isIdToken d)))
    h <- stmts
    i <- returnStatement
    s <- getState
    -- precisamos forçar o return na function. Aqui pegamos a variavel retornada e mudamos o valor dela na tabela de símbolos.
    updateState (symtableInsert $ fromToken [i!!1] (getVariableName (i!!1)) (lookupLastScopeFrom (getVariableName b) s))
    j <- endFunToken
    k <- semicolonToken
    return (a:b:c:d ++ [e] ++ (f:g:h) ++ i ++ (j:k:[]))

returnStatement :: ParsecT [Token] [Type] IO [Token]
returnStatement = (do
        a <- keywordToken "return"
        b <- idToken -- <|> floatToken <|> intToken -- TODO::Por enquanto só permite retornar uma coisa: return var;
        c <- semicolonToken
        return (a:b:[c])) <|> (return [])

arguments :: ParsecT [Token] [Type] IO [Token]
arguments = (do
        a <- primitiveTypeToken
        b <- idToken
        c <- remainingArguments
        return (a:b:c)) <|> (return [])

-- arrayAccess :: ParsecT [Token] [Type] IO [Token]
-- arrayAccess = 
--     (do
--     a <- idToken
--     b <- blockBeginToken "["
--     c <- intToken
--     d <- blockEndToken "]"
--     e <- semicolonToken
--     return (a:b:c:d:e:[])) <|>
--     (do
--     a <- idToken
--     b <- blockBeginToken "["
--     c <- intToken
--     d <- blockEndToken "]"
--     return (a:b:c:d:[]))

remainingArguments :: ParsecT [Token] [Type] IO [Token]
remainingArguments = (do
    a <- commaToken ","
    b <- arguments
    return (a:b)) <|> (return [])

-- TODO::mover desse arquivo
retornarLexerTipo :: Token -> String 
retornarLexerTipo (Lexer.Int  a)   = "int"
retornarLexerTipo (Lexer.Float a)  = "float"
retornarLexerTipo (Lexer.String a) = "string"
retornarLexerTipo (Lexer.Boolean a) = "boolean"
retornarLexerTipo (Lexer.Array) = "array"

-- TODO::mover desse arquivo
retornarPrimitiveType :: Token -> String
retornarPrimitiveType (PrimitiveType a) = a

-- TODO::mover desse arquivo
validarTipo :: Token -> [Token] -> Bool
validarTipo t x = 
                if retornarPrimitiveType t == retornarLexerTipo (last x)
                    then True
                else if (retornarPrimitiveType t == "float") && (retornarLexerTipo (last x) == "int") 
                    then True
                else False

-- TODO::mover daqui
isKeywordToken :: Token -> Bool
isKeywordToken (Lexer.Keyword k) = True
isKeywordToken _           = False

-- TODO::mover daqui
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then []
                         else takeUntil p xs

convert :: Token -> [Char] -> Token
convert (PrimitiveType "int") x = (Lexer.Int (read x::Int))
convert (PrimitiveType "float") x = (Lexer.Float (read x::Double))
convert (PrimitiveType "string") x = (Lexer.String x)