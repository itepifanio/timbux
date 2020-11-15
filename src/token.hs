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

stringToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
stringToken = tokenPrim show update_pos get_token where
    get_token (String x) = Just (String x)
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

remaining_digits :: Parsec [Token] st [Token]
remaining_digits = (do a <- commaToken ","
                       b <- digitSequence
                       return (a:b)) <|> (return [])

remaining_arrays :: Parsec [Token] st [Token]
remaining_arrays = (do a <- commaToken ","
                       b <- arraySequence
                       return (a:b)) <|> (return [])

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

array :: Parsec [Token] st [Token]
array = do
          open <- blockBeginToken "["
          values <- digitSequence <|> arraySequence
          close <- blockEndToken "]"
          return (open:values++[close])

commaToken :: String -> Parsec [Token] st Token
commaToken stmt = tokenPrim show update_pos get_token where
    get_token (Comma x) = if x == stmt then Just (Comma x) else Nothing
    get_token _         = Nothing
    
keywordToken :: String -> ParsecT [Token] st Data.Functor.Identity.Identity Token
keywordToken stmt = tokenPrim show update_pos get_token where
    get_token (Keyword x) = if x == stmt then Just (Keyword x) else Nothing
    get_token _         = Nothing

blockBeginToken :: String -> ParsecT [Token] st Data.Functor.Identity.Identity Token
blockBeginToken stmt = tokenPrim show update_pos get_token where
    get_token (BlockBegin x) = if x == stmt then Just (BlockBegin x) else Nothing
    get_token _         = Nothing    

blockEndToken :: String -> ParsecT [Token] st Data.Functor.Identity.Identity Token
blockEndToken stmt = tokenPrim show update_pos get_token where
    get_token (BlockEnd x) = if x == stmt then Just (BlockEnd x) else Nothing
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

opToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
opToken = tokenPrim show update_pos get_token where
    get_token (Op x) = Just (Op x)
    get_token _      = Nothing

-- e termina com o ghbc
endToken :: Parsec [Token] st Token
endToken = tokenPrim show update_pos get_token where
    get_token Ghbc = Just Ghbc
    get_token _    = Nothing

funToken :: Parsec [Token] st Token
funToken = tokenPrim show update_pos get_token where
    get_token Fun = Just Fun
    get_token _   = Nothing

endFunToken :: Parsec [Token] st Token
endFunToken = tokenPrim show update_pos get_token where
    get_token Endfun = Just Endfun
    get_token _      = Nothing

colonToken :: Parsec [Token] st Token
colonToken = tokenPrim show update_pos get_token where
    get_token Colon = Just Colon
    get_token _      = Nothing