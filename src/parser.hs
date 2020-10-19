module Main (main) where

import Lexer
import Text.Parsec

-- o programa inicializa com o "let"
beginToken = tokenPrim show update_pos get_token where
    get_token Let = Just Let
    get_token _   = Nothing

-- e termina com o ghbc
endToken = tokenPrim show update_pos get_token where
    get_token Ghbc = Just Ghbc
    get_token _    = Nothing

idToken = tokenPrim show update_pos get_token where
    get_token (Name x) = Just (Name x)
    get_token _      = Nothing

-- language types
floatToken = tokenPrim show update_pos get_token where
  get_token (Float x) = Just (Float x)
  get_token _       = Nothing 

intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _       = Nothing

-- general statements programing
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos

-- general tokens
semicolonToken :: Parsec [Token] st Token
semicolonToken = tokenPrim show update_pos get_token where
    get_token Semicolon = Just Semicolon
    get_token _         = Nothing

commaToken :: Parsec [Token] st Token
commaToken = tokenPrim show update_pos get_token where
    get_token Comma = Just Comma
    get_token _         = Nothing

openBracketsToken :: Parsec [Token] st Token
openBracketsToken = tokenPrim show update_pos get_token where
    get_token OpenBrackets = Just OpenBrackets
    get_token _    = Nothing

closedBracketsToken :: Parsec [Token] st Token
closedBracketsToken = tokenPrim show update_pos get_token where
    get_token ClosedBrackets = Just ClosedBrackets
    get_token _    = Nothing

assignToken = tokenPrim show update_pos get_token where
    get_token Assign = Just Assign
    get_token _      = Nothing

primitiveTypeToken = tokenPrim show update_pos get_token where
    get_token (PrimitiveType x) = Just (PrimitiveType x)
    get_token _      = Nothing

program :: Parsec [Token] st [Token]
program = do
        a <- beginToken 
        b <- idToken -- nome do programa
        c <- stmts
        d <- endToken
        eof
        return ([a] ++ [b] ++ c ++ [d])  -- Como o programa é estruturado: 'a' e 'c' início e fim dos códigos, 'd' os statements

stmts :: Parsec [Token] st [Token]
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)
digitSequence::Parsec [Token] st [Token]
digitSequence = do
        first <- (intToken <|> floatToken)
        next <- remaining_digits
        return([first]++next)

array :: Parsec [Token] st [Token]
array = do
          open <- openBracketsToken
          values <- digitSequence
          close <- closedBracketsToken
          return (values)

assign :: Parsec [Token] st [Token]
assign = do
          a <- primitiveTypeToken
          b <- idToken
          c <- assignToken
          d <- array
          return (b:c:d)
        --   d <- (intToken <|> (floatToken <|> array))
        --   return (b:c:[d])

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do a <- semicolonToken
                      b <- assign
                      return (a:b)) <|> (return [])
remaining_digits :: Parsec [Token] st [Token]
remaining_digits = (do 
                    a <- commaToken
                    b <- (intToken <|> floatToken)
                    return (a:[b])) <|> (return [])

-- invocação do parser para o símbolo de partida 
parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "programaV0.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }

-- Consertar o problema com o ;, só tá aceitando duas operações.