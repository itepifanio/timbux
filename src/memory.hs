module Memory where      

import Lexer
-- Definição do array
-- Seja uma matriz identidade 2x2, sua representação será:
-- MyArray [(MyInt 1,[0,0]),(MyInt 0,[0,1]),(MyInt 0,[1,0]),(MyInt 1,[1,1])] "nomeDaVariavel" "escopo" [2,2]

-- Definição dos demais tipos
-- Seja um tipo inteiro, sua represetnação será:
-- MyType (MyInt 1) "nomeDaVariavel" "escopo"

data Typex = MyInt Int       |
             MyFloat Float   |
             MyString String 
             deriving (Show)
                   
data Type = MyType Typex String String                   |
            MyArray [(Typex, [Int])] String String [Int]
            deriving (Show)

symtableInsertMany :: [Type] -> [Type] -> [Type]
symtableInsertMany []     a = a
symtableInsertMany (x:xs) a = symtableInsertMany xs (symtableInsert x a)

symtableInsert :: Type -> [Type] -> [Type]
symtableInsert symbol [] = [symbol]
symtableInsert symbol table = table++[symbol]

symtableUpdate :: Type -> [Type] -> [Type]
symtableUpdate _ [] = fail "Not found"
symtableUpdate (MyType a id1 es1) ((MyType b id2 es2):t) =
                            if id1 == id2 && es1 == es2 then ((MyType a id1 es1) : t)
                            else (MyType b id2 es2) : symtableUpdate (MyType a id1 es1) t
symtableUpdate (MyType a id1 es1) ((MyArray _ _ _ _):t) = symtableUpdate (MyType a id1 es1) t
symtableUpdate (MyArray a id1 es1 b) ((MyArray c id2 es2 d):t) =
                            if id1 == id2 && es1 == es2 then ((MyArray a id1 es1 b) : t)
                            else (MyArray c id2 es2 d) : symtableUpdate (MyArray a id1 es1 b) t
symtableUpdate (MyArray a id1 es1 b) ((MyType _ _ _):t) = symtableUpdate (MyArray a id1 es1 b) t

symtableDelete :: String -> [Type] -> [Type]
symtableDelete _ [] = []
symtableDelete es1 ((MyType typex id2 es2):t) =  
                            if es1 == es2 then t
                            else (MyType typex id2 es2) : symtableDelete es1 t
symtableDelete es1 ((MyArray a id es2 s):t) =
                            if es1 == es2 then symtableDelete es1 t
                            else (MyArray a id es2 s) : symtableDelete es1 t

fromToken :: Token -> String -> String -> Type
fromToken (Lexer.Int a) nome escopo = MyType (MyInt a) nome escopo

-- TODO::adicionar o token 'function' no lexer.x, criar no arquivo token.hs o Token em si, 
--       criar no stmts.hs a estrutura da função.
-- symtableDeleteScope :: String -> [Type] -> [Type]

-- Exemplo de uso do symtableUpdate atualizando o MyType para 2
-- symtableUpdate
--    (MyType (MyInt 2) "nomeDaVariavel" "escopo") 
--    ([
--        MyArray [(MyInt 1,[0,0]),(MyInt 0,[0,1]),(MyInt 0,[1,0]),(MyInt 1,[1,1])] "nomeDaVariavel" "escopo" [2,2],
--        MyType (MyInt 1) "nomeDaVariavel" "escopo"
--    ])

-- Exemplo de uso do symtableDelete removendo MyType 1
-- symtableDelete
--    "escopo"
--    ([
--        MyArray [(MyInt 1,[0,0]),(MyInt 0,[0,1]),(MyInt 0,[1,0]),(MyInt 1,[1,1])] "nomeDaVariavel" "escopo" [2,2],
--        MyType (MyInt 1) "nomeDaVariavel" "escopo"
--    ])