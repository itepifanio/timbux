module Memory where      

import Lexer
-- Definição do array
-- Seja uma matriz identidade 2x2, sua representação será:
-- MyArray [(MyInt 1,[0,0]),(MyInt 0,[0,1]),(MyInt 0,[1,0]),(MyInt 1,[1,1])] "nomeDaVariavel" "escopo" [2,2]

-- Definição dos demais tipos
-- Seja um tipo inteiro, sua represetnação será:
-- MyType (MyInt 1) "nomeDaVariavel" "escopo"

data Typex = MyInt Int       |
             MyFloat Double  |
             MyString String 
             deriving (Show)
                   
data Type = MyType Typex String String                   |
            MyArray [(Typex, [Int])] String String [Int]
            deriving (Show)

symtableInsertMany :: [Type] -> [Type] -> [Type]
symtableInsertMany []     a = a
symtableInsertMany (x:xs) a = symtableInsertMany xs (symtableInsert x a)

symtableUpdateMany :: [Type] -> [Type] -> [Type]
symtableUpdateMany []     a = a
symtableUpdateMany (x:xs) a = symtableUpdateMany xs (symtableUpdate x a)

symtableInsert :: Type -> [Type] -> [Type]
symtableInsert symbol [] = [symbol]
symtableInsert symbol table = table++[symbol]

symtableUpdate :: Type -> [Type] -> [Type]
symtableUpdate _ [] = fail "Not found"
symtableUpdate (MyType a id1 es1) ((MyType b id2 es2):t) =
                            if id1 == id2 && es1 == es2 then ((MyType a id1 es1) : t)
                            else (MyType b id2 es2) : symtableUpdate (MyType a id1 es1) t

symtableUpdate (MyType a id1 es1) ((MyArray b id2 es2 d):t) = (MyArray b id2 es2 d) : symtableUpdate (MyType a id1 es1) t
symtableUpdate (MyArray a id1 es1 b) ((MyArray c id2 es2 d):t) =
                            if id1 == id2 && es1 == es2 then ((MyArray a id1 es1 b) : t)
                            else (MyArray c id2 es2 d) : symtableUpdate (MyArray a id1 es1 b) t

symtableUpdate (MyArray a id1 es1 b) ((MyType c id2 es2 ):t) = (MyType c id2 es2 ) : symtableUpdate (MyArray a id1 es1 b) t

symtableDelete :: String -> [Type] -> [Type]
symtableDelete _ [] = []
symtableDelete es1 ((MyType typex id2 es2):t) =  
                            if es1 == es2 then t
                            else (MyType typex id2 es2) : symtableDelete es1 t
symtableDelete es1 ((MyArray a id es2 s):t) =
                            if es1 == es2 then symtableDelete es1 t
                            else (MyArray a id es2 s) : symtableDelete es1 t

fromToken :: [Token] -> String -> String -> Type
fromToken (x:xs) nome escopo = 
    if xs == [] then fromTokenX x nome escopo          -- Modifica tipos simples: Int, String, Float
    else convertArrayStmtsToMyArray (x:xs) nome escopo -- Pega os tokens e volta o MyArray

fromTokenX :: Token -> String -> String -> Type
fromTokenX (Lexer.Int  a)  nome escopo = MyType (MyInt a)    nome escopo
fromTokenX (Lexer.Name a)  nome escopo = MyType (MyString a) nome escopo
fromTokenX (Lexer.Float a) nome escopo = MyType (MyFloat a)  nome escopo

convertArrayStmtsToMyArray :: [Token] -> String -> String  -> Type
convertArrayStmtsToMyArray (x:xs) nome escopo = 
    MyArray arrayDeTuplasTypexInt nome escopo [(snd (last arrayDeTuplasTypexInt) !! 0) + 1, 1] -- Fixado arrays unidimencionais por enquanto
    where arrayDeTuplasTypexInt = (convertTypeTokensToArray (x:xs) nome escopo 0)

convertTypeTokensToArray :: [Token] -> String -> String -> Int -> [(Typex, [Int])]
convertTypeTokensToArray [] nome escopo i = []
convertTypeTokensToArray (t:ts) nome escopo i
    | isBracketToken(t) || isCommaToken(t) = convertTypeTokensToArray ts nome escopo i --ignora os tokens de comma e blockbegin, blockend
    | otherwise = [((fromTypeToTypex $ fromTokenX t nome escopo), [i])] ++ (convertTypeTokensToArray ts nome escopo (i+1))

getVariableName :: Token -> String
getVariableName (Lexer.Name n) = n

isBracketToken :: Token -> Bool
isBracketToken (Lexer.BlockBegin _) = True
isBracketToken (Lexer.BlockEnd   _) = True
isBracketToken _                    = False

isCommaToken :: Token -> Bool
isCommaToken (Lexer.Comma _) = True
isCommaToken _               = False

fromTypeToTypex :: Type -> Typex
fromTypeToTypex (MyType t _ _) = t

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