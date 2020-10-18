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

semicolonToken :: Parsec [Token] st Token
semicolonToken = tokenPrim show update_pos get_token where
  get_token Semicolon = Just Semicolon
  get_token _         = Nothing

assignToken = tokenPrim show update_pos get_token where
    get_token Assign = Just Assign
    get_token _      = Nothing


-- ajustar essa função para que possa assinar pra todos os tipos
intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _       = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria (Não sei o que é isso.)
update_pos pos _ []      = pos

program :: Parsec [Token] st [Token]
program = do
        a <- beginToken 
        b <- stmts
        c <- endToken
        eof
        return ([a] ++ b ++ [c])  -- Como o programa é estruturado: 'a' e 'c' início e fim dos códigos, 'd' os statements

stmts :: Parsec [Token] st [Token]
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

assign :: Parsec [Token] st [Token]
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken
          return (a:b:[c])

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do a <- semicolonToken
                      b <- assign
                      return (a:b)) <|> (return [])

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program () "Error message" tokens

main :: IO ()
main = case parser (getTokens "programaV0.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }

-- Consertar o problema com o ;, só tá aceitando duas operações.