module Statement where

import Lexer
import Token
import Text.Parsec
import Memory
import Expression
import Data.Functor.Identity

import Data.List
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
    c <- logicExpression
    d <- blockEndToken    ")"
    if tokenToBool (c!!0)
        then updateState ( symtableUpdateFlag 1 )
    else updateState ( symtableUpdateFlag 0)
    e <- stmts
    f <- keywordToken endstmt
    updateState ( symtableUpdateFlag 1)
    return ((a:b:c) ++ [d] ++ e ++ [f])

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
        then updateState ( symtableUpdateFlag 1 )
    else updateState ( symtableUpdateFlag 0)
    e <- semicolonToken
    f <- justAssign
    l <- blockEndToken  ")"

    -- liftIO (print (z \\ takeUntil isKeywordToken z)) -- \\\\ é diferença total entre listas
    -- setInput <- z ++ (z \\ takeUntil isKeywordToken z)
    m <- stmts
    liftIO (print z)
    n <- keywordToken "endfor"
    setInput z
    return ((a:b:c) ++ d ++ [e] ++ f ++ (l:m++[n]))

singletonToken:: ParsecT [Token] [Type] IO([Token])
singletonToken = do
            a <- expression <|> booleanToken
            return([a])

assign :: ParsecT [Token] [Type] IO([Token])
assign = do
        a <- instAssign <|> justAssign
        b <- semicolonToken
        return (a++[b])

instAssign :: ParsecT [Token] [Type] IO([Token])
instAssign = do
          a <- primitiveTypeToken
          b <- idToken
          c <- assignToken
          d <- singletonToken <|> array
          s1 <- getState
          updateState (symtableInsert (fromToken d (getVariableName b) (lookupLastScope s1)))
        --   if validarTipo a d then updateState (symtableInsert (fromToken d (getVariableName b) (lookupLastScope s1)))
        --   else fail ("Type don't match with type of variable " ++ getVariableName b)
          s2 <- getState
          liftIO (print s2)
          return (a:b:c:d)

justAssign :: ParsecT [Token] [Type] IO [Token]
justAssign = do
          a <- idToken
          b <- assignToken
          c <- singletonToken <|> array
          s1 <- getState
          if validarTipo (getType s1 (getVariableName a) (lookupLastScope s1)) c 
              then updateState (symtableCanUpdate (fromToken c (getVariableName a) (lookupLastScope s1)))
          else fail ("Type don't match with type of variable " ++ getVariableName a)    
          s2 <- getState
          liftIO (print s2)
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
validarTipo t (x:xs) = 
                if retornarPrimitiveType t == retornarLexerTipo x then True
                else if (retornarPrimitiveType t == "float") && (retornarLexerTipo x == "int") then True
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