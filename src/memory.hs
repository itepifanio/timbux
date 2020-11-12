module Memory where

-- import Lexer
-- import Token

-- type MemoryCell = (Token,Token)
-- type Memory = [MemoryCell]       

-- essa definição de array seria algo como
-- meu_array = [7 8]
-- Array ([(7,[0 0]), (8, [0 1])], "escopo", "meu_array", 2, 1)
-- meu_array =  6 7
--              8 9 
-- Array ([(6, [0 0 0]), (7, [0 0 1]), (8, [1 0]), (9, [1 1])], "escopo", "meu_array", 2, 2)
-- Array [()]
-- | MyArray [(Type, [Int])] String String Int Int

data Typex = MyInt Int       |
             MyFloat Float   |
             MyString String 
             deriving (Show)
                   
data Type = MyType Typex String String                   |
            MyArray [(Typex, [Int])] String String [Int]
            deriving (Show)

symtableInsert :: Type -> [Type] -> [Type]
symtableInsert symbol [] = [symbol]
symtableInsert symbol table = table++[symbol]

symtableUpdate :: Type -> [Type] -> [Type]
symtableUpdate _ [] = fail "Not found"
symtableUpdate (MyType (_ v1) id1 es) ((MyType (_ v2) id2 es2):t) =  
                            if id1 == id2 && es == es2 then ((MyType (_ v1) id1 es) : t)
                            else (MyType (_ v2) id2 es2) : symtableUpdate (MyType (_ v1) id1 es) t

symtableDelete :: Type -> [Type] -> [Type]
symtableDelete _ [] = []
symtableDelete (_ _ nome escopo) ((_ _ id2 es2):t) =  
                            if escopo == es2 && nome == id2 then t
                            else (_ _ id2 es2) : symtableDelete escopo t

-- TODO::adicionar o token 'function' no lexer.x, criar no arquivo token.hs o Token em si, 
--       criar no stmts.hs a estrutura da função.
-- symtableDeleteScope :: String -> [Type] -> [Type]