module Token where

import Lexer
import Text.Parsec
import Data.Functor.Identity

-- general statements programing
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos

idToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
idToken = tokenPrim show update_pos get_token where
    get_token (Name x) = Just (Name x)
    get_token _        = Nothing

digitSequence::Parsec [Token] st [Token]
digitSequence = do
        first <- (intToken <|> floatToken)
        next <- remaining_digits
        return([first]++next)

arraySequence::Parsec [Token] st [Token]
arraySequence = do
        first <- array
        next <- remaining_arrays
        return(first++next)

-- nameSequence::Parsec [Token] st [Token]
-- nameSequence = do
--         first <- stmts
--         next <- remaining_names
--         return([first]++next) 

remaining_digits :: Parsec [Token] st [Token]
remaining_digits = (do a <- commaToken <?> ","
                       b <- digitSequence
                       return (a:b)) <|> (return [])

remaining_arrays :: Parsec [Token] st [Token]
remaining_arrays = (do a <- commaToken <?> ","
                       b <- arraySequence
                       return (a:b)) <|> (return [])

-- remaining_names :: Parsec [Token] st [Token]
-- remaining_names = (do  b <- nameSequence
--                        return (b)) <|> (return [])

-- language types
floatToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
floatToken = tokenPrim show update_pos get_token where
    get_token (Float x) = Just (Float x)
    get_token _         = Nothing 

intToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
intToken = tokenPrim show update_pos get_token where
    get_token (Int x) = Just (Int x)
    get_token _       = Nothing


booleanToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
booleanToken = tokenPrim show update_pos get_token where
    get_token (Boolean x) = Just (Boolean x)
    get_token _           = Nothing

-- stringTeste :: Parsec [Token] st [Token]
-- stringTeste = do
--           open <- commaToken <?> "\""
--           values <- nameSequence
--           close <- commaToken <?> "\""
--           return (open:values++[close])

array :: Parsec [Token] st [Token]
array = do
          open <- blockBeginToken <?> "["
          values <- digitSequence <|> arraySequence
          close <- blockEndToken <?> "]"
          return (open:values++[close])

commaToken :: Parsec [Token] st Token
commaToken = tokenPrim show update_pos get_token where
    get_token (Comma x) = Just (Comma x)
    get_token _         = Nothing
    
keywordToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
keywordToken = tokenPrim show update_pos get_token where
    get_token (Keyword x) = Just (Keyword x)
    get_token _         = Nothing

blockBeginToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
blockBeginToken = tokenPrim show update_pos get_token where
    get_token (BlockBegin x) = Just (BlockBegin x)
    get_token _         = Nothing    

blockEndToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
blockEndToken = tokenPrim show update_pos get_token where
    get_token (BlockEnd x) = Just (BlockEnd x)
    get_token _         = Nothing

assignToken :: Parsec [Token] st Token
assignToken = tokenPrim show update_pos get_token where
    get_token Assign = Just Assign
    get_token _      = Nothing

comparativeOpToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
comparativeOpToken = tokenPrim show update_pos get_token where
    get_token (ComparativeOp x) = Just (ComparativeOp x)
    get_token _                 = Nothing

primitiveTypeToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
primitiveTypeToken = tokenPrim show update_pos get_token where
    get_token (PrimitiveType x) = Just (PrimitiveType x)
    get_token _                 = Nothing

semicolonToken :: Parsec [Token] st Token
semicolonToken = tokenPrim show update_pos get_token where
    get_token Semicolon = Just Semicolon
    get_token _         = Nothing

-- o programa inicializa com o "let"
beginToken :: Parsec [Token] st Token
beginToken = tokenPrim show update_pos get_token where
    get_token Let = Just Let
    get_token _   = Nothing

-- e termina com o ghbc
endToken :: Parsec [Token] st Token
endToken = tokenPrim show update_pos get_token where
    get_token Ghbc = Just Ghbc
    get_token _    = Nothing
