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

backslashNToken :: Parsec [Token] st Token
backslashNToken = tokenPrim show update_pos get_token where
  get_token BackslashN = Just backslashN
  get_token _         = Nothing

assignToken = tokenPrim show update_pos get_token where
    get_token Assign = Just Assign
    get_token _      = Nothing

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria (Não sei o que é isso.)
update_pos pos _ []      = pos

stmt :: Parsec [Token] st [Token]
stmt = do
        a <- beginToken 
        b <- stmts
        c <- endToken
        eof
        return ([a] ++ d ++ [c])  -- Como o programa é estruturado: 'a' e 'c' início e fim dos códigos, 'd' os statements

stmts :: Parsec [Token] st [Token]
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

assign :: Parsec [Token] st [Token]
assign = do
          a <- assignToken
          b <- intToken
          return (a:[b])

remaining_stmts :: Parsec [Token] st [Token]
remaining_stmts = (do a <- backslashNToken
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