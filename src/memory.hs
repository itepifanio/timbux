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

convertTypexToPrimitiveType :: Typex -> Token
convertTypexToPrimitiveType (MyInt _) = PrimitiveType "int"
convertTypexToPrimitiveType (MyFloat _) = PrimitiveType "float"
convertTypexToPrimitiveType (MyString _) = PrimitiveType "string"

getType :: [Type] -> String -> String -> Token
getType ((MyArray a id es sla):ts) v es3 = getType ts v es3
getType ((MyType a id es):ts) variavel es2 = 
    if id == variavel && es == es2 then convertTypexToPrimitiveType a
    else getType ts variavel es2

lookupLastScope :: [Type] -> String
lookupLastScope [] = ""
lookupLastScope ts = getScope $ last ts

lookupLastScopeFrom :: String -> [Type] -> String
lookupLastScopeFrom _      []    = ""
lookupLastScopeFrom string (t:ts) = 
    if getScope t == string then lookupLastScopeFrom string ts
    else getScope t

-- return only one [Type] or []
lookupNameFrom :: String -> [Type] -> [Type]
lookupNameFrom _      []    = []
lookupNameFrom name (t:ts)  = 
    if getName t == name then [t]
    else lookupNameFrom name ts

-- receive a state and check if is executing
isExecuting :: [Type] -> Bool
isExecuting types = canOperate $ lookupNameFrom "asdasldj==@#!" types

-- lookupNameFrom name = 

symtableInsertMany :: [Type] -> [Type] -> [Type]
symtableInsertMany []     a = a
symtableInsertMany (x:xs) a = symtableInsertMany xs (symtableInsert x a)

symtableUpdateMany :: [Type] -> [Type] -> [Type]
symtableUpdateMany []     a = a
symtableUpdateMany (x:xs) a = symtableUpdateMany xs (symtableCanUpdate x a)

symtableSearch :: [Type] -> String -> String -> (Typex, Bool)
symtableSearch [] a b = ((MyInt 0), False)
symtableSearch ((MyType a id es):ts) variavel es2 = 
    if id == variavel && es == es2 then (a, True)
    else symtableSearch ts variavel es2
symtableSearch ((MyArray a id es sla):ts) variavel es2 = symtableSearch ts variavel es2

-- searchIndex :: [Int] -> Int -> Bool
-- searchIndex [] _ = False
-- searchIndex (i:idx) index = 
--     if i == index then True
--     else searchIndex idx index

-- MyArray [(Typex, [Int])] String String [Int]
arraySearch :: [(Typex, [Int])] -> [Int] -> (Typex, Bool)
arraySearch [] _ = ((MyInt 0), False)
arraySearch (((MyInt value), index):t) idxs = 
    if index == idxs then ((MyInt value), True) 
    else arraySearch t idxs

-- MyArray [(Typex, [Int])] String String [Int]
symtableArraySearch :: [Type] -> [Int] -> String -> String -> (Typex, Bool)
symtableArraySearch [] _ _ _ = ((MyInt 0), False)
symtableArraySearch ((MyType _ _ _):ts) indexes variavel es2 = symtableArraySearch ts indexes variavel es2
symtableArraySearch ((MyArray a id es sla):ts) indexes variavel es2 = 
    if id == variavel && es == es2 then arraySearch a indexes 
    else symtableArraySearch ts indexes variavel es2

symtableInsert :: Type -> [Type] -> [Type]
symtableInsert symbol [] = [symbol]
symtableInsert symbol table = if canOperate table then table++[symbol] else table

-- Pra não ter fazer essa verificação no symtableUpdate,
-- achei melhor fazer aqui, não muda nada na estrutura do código
symtableCanUpdate :: Type -> [Type] -> [Type]
symtableCanUpdate t table = 
    if canOperate table
        then symtableUpdate t table
    else table

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

symtableCanDelete :: String -> [Type] -> [Type]
symtableCanDelete es table = 
    if canOperate table 
        then symtableDelete es table
    else table

symtableDelete :: String -> [Type] -> [Type]
symtableDelete _ [] = []
symtableDelete es1 ((MyType typex id2 es2):t) =  
                            if es1 == es2 then t
                            else (MyType typex id2 es2) : symtableDelete es1 t
symtableDelete es1 ((MyArray a id es2 s):t) =
                            if es1 == es2 then symtableDelete es1 t
                            else (MyArray a id es2 s) : symtableDelete es1 t

----- FLAG -----
-- a flag sempre é o primeiro elemento, por isso as funções desta forma
symtableUpdateFlag :: Int -> [Type] -> [Type]
symtableUpdateFlag int ((MyType b id2 es2):table) = ((MyType (MyInt int) "asdasldj==@#!" ""):table)

canOperate :: [Type] -> Bool
canOperate ((MyType (MyInt a) v e):ts) = a == 1

-- Converte um array de tokens em nosso datatype Type
fromToken :: [Token] -> String -> String -> Type
fromToken (x:xs) nome escopo
    | xs == []         = fromTokenX x nome escopo               -- Modifica tipos simples: Int, String, Float
    | otherwise = convertArrayStmtsToMyArray (x:xs) nome escopo -- Pega os tokens e volta o MyArray

-- Converte um token em um datatype Type sem ser o array
fromTokenX :: Token -> String -> String -> Type
fromTokenX (Lexer.Int  a)    nome escopo = MyType (MyInt a)    nome escopo
fromTokenX (Lexer.Name a)    nome escopo = MyType (MyString a) nome escopo
fromTokenX (Lexer.Float a)   nome escopo = MyType (MyFloat a)  nome escopo
fromTokenX (Lexer.String a)  nome escopo = MyType (MyString a) nome escopo

-- Converte um typex para um token
fromTypeX :: Typex -> Token
fromTypeX (MyInt a) = Int a
fromTypeX (MyFloat a) = Float a
fromTypeX (MyString a) = String a
-- MyArray [(Typex, [Int])] String String [Int]
-- Converte um array de tokens em um datatype Type MyArray
convertArrayStmtsToMyArray :: [Token] -> String -> String  -> Type
convertArrayStmtsToMyArray (x:xs) nome escopo = 
    if null arrayDeTuplasTypexInt then MyArray [] nome escopo [] else MyArray arrayDeTuplasTypexInt nome escopo [(snd (last arrayDeTuplasTypexInt) !! 0) + 1, 1] -- Fixado arrays unidimencionais por enquanto
    where arrayDeTuplasTypexInt = (convertTypeTokensToArray (x:xs) nome escopo 0)

-- Função auxiliar que converte o datatype para MyArray
convertTypeTokensToArray :: [Token] -> String -> String -> Int -> [(Typex, [Int])]
convertTypeTokensToArray [] nome escopo i = []
convertTypeTokensToArray (t:ts) nome escopo i
    | isBracketToken(t) || isCommaToken(t) = convertTypeTokensToArray ts nome escopo i --ignora os tokens de comma e blockbegin, blockend
    | otherwise = [((fromTypeToTypex (fromTokenX t nome escopo), [i]))] ++ (convertTypeTokensToArray ts nome escopo (i+1))
-- MyArray [((MyInt 99), [0]), ((MyInt 99), [0]) ]
-- ((MyInt 100), [0]) ++ [((MyInt 99), [0]), ((MyInt 99), [0])] = [((MyInt 100), [0]) , ((MyInt 99), [0]), ((MyInt 99), [0])]  
-- a[0][1] = 1 |           (Lexer.Int a) [0,1] variável escopo memória 

symtableInsertIndexArray :: [Token] -> [Int] -> String -> String -> [Type] -> [Type] 
symtableInsertIndexArray _ _ _ _ [] = []
symtableInsertIndexArray (token:t) indexes nome es ((MyType typex nome2 es2):ts) = (MyType typex nome2 es2) : symtableInsertIndexArray (token:t) indexes nome es ts
symtableInsertIndexArray (token:t) indexes nome es ((MyArray list nome2 es2 d):ts) = 
    if nome == nome2 && es2 == es then ((MyArray ([(fromTokenToTypex token, indexes)] ++ list) nome es d):ts)
    else (MyArray list nome2 es2 d) : symtableInsertIndexArray (token:t) indexes nome es ts

-- TODO::mover as funções abaixo para um novo arquivo posteriormente

getScope :: Type -> String
getScope (MyType _ _ e)    = e
getScope (MyArray _ _ e _) = e

getName :: Type -> String
getName (MyType _ n _)    = n
getName (MyArray _ n _ _) = n

getValue :: Token -> String
getValue (Lexer.Int n)    = show n
getValue (Lexer.Float n)  = show n
getValue (Lexer.String n) = filter (/='"') n

getVariableName :: Token -> String
getVariableName (Lexer.Name n) = n

isBracketToken :: Token -> Bool
isBracketToken (Lexer.BlockBegin _) = True
isBracketToken (Lexer.BlockEnd   _) = True
isBracketToken _                    = False

isCommaToken :: Token -> Bool
isCommaToken (Lexer.Comma _) = True
isCommaToken _               = False

isIdToken :: Token -> Bool
isIdToken (Lexer.Name _)  = True
isIdToken _               = False

fromTypeToTypex :: Type -> Typex
fromTypeToTypex (MyType t _ _) = t

fromTokenToTypex :: Token -> Typex
fromTokenToTypex (Lexer.Int a) = (MyInt a)
fromTokenToTypex (Lexer.Float a) = (MyFloat a)
fromTokenToTypex (Lexer.String a) = (MyString a)

genericValue :: Token -> Token
genericValue (PrimitiveType "int") = (Lexer.Int 0)
genericValue (PrimitiveType "float") = (Lexer.Float 0.0)
genericValue (PrimitiveType "string") = (Lexer.String "")

-- symtableDeleteScope :: String -> [Type] -> [Type]

-- Exemplo de uso do symtableCanUpdate atualizando o MyType para 2
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