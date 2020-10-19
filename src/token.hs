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
    get_token _      = Nothing

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
    get_token _         = Nothing

-- general statements
commaToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
commaToken = tokenPrim show update_pos get_token where
    get_token (Comma x) = Just (Comma x)
    get_token _       = Nothing

assignToken :: Parsec [Token] st Token
assignToken = tokenPrim show update_pos get_token where
    get_token Assign = Just Assign
    get_token _      = Nothing

primitiveTypeToken :: ParsecT [Token] st Data.Functor.Identity.Identity Token
primitiveTypeToken = tokenPrim show update_pos get_token where
    get_token (PrimitiveType x) = Just (PrimitiveType x)
    get_token _      = Nothing

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
