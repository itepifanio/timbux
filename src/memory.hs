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
-- Array ([(6,[0 0]), (7, [0 1]), (8, [1 0]), (9, [1 1])], "escopo", "meu_array", 2, 2)

-- | MyArray [(Type -> [Int])] String String Int Int

data Type = MyInt Int String String | MyFloat Float String String | MyString String String String deriving (Show)

symtableInsert :: Type -> [Type] -> [Type]
symtableInsert symbol [] = [symbol]
symtableInsert symbol table = table++[symbol]


