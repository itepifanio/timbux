module Typedata.Matrix where

import Data.List

data Matrix a = Matrix [[a]]
    deriving (Eq)

instance Num a => Num (Matrix a)
    where 
        (Matrix a) + (Matrix b) = Matrix $ sumMatrix a b
        (Matrix a) - (Matrix b) = Matrix $ diffMatrix a b
        (Matrix a) * (Matrix b) = Matrix $ multMatrix a b
        abs (Matrix a)          = Matrix $ mapMatrix abs a
        signum (Matrix a)       = Matrix $ mapMatrix signum a
--      fromInteger (Matrix a)  = Matrix $ fromIntegerMatrix a

-- Ver como sobrescrever o prelude nesse caso, além de sem usar o typeclass
-- (*) :: Number -> Matrix -> Matrix
-- (*) a xss = Matrix $ mapMatrix (\x -> x * a) xss

instance Show a => Show (Matrix a)
    where
        show (Matrix [[]]) = "[[]]"
        show (Matrix a)    = intercalate "\n" $ map (intercalate " " . map show) a

sumMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrix = zipWith (zipWith (+))

diffMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
diffMatrix = zipWith (zipWith (-))

multMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
multMatrix us vs = map (mult [] vs) us
    where
        mult xs [] _ = xs
        mult xs _ [] = xs
        mult [] (zs:zss) (y:ys) = mult (map (y *) zs) zss ys
        mult xs (zs:zss) (y:ys) = mult (zipWith (\u v -> u + v * y) xs zs) zss ys

mapMatrix :: Num a => (a -> a) -> [[a]] -> [[a]]
mapMatrix f xss = map (map f) xss

-- fromIntegerMatrix :: a -> [[a]]
-- fromIntegerMatrix 0 = map (\x -> x ++ []) (replicate 2 [])
-- fromIntegerMatrix n = map (\x -> x : []) (replicate 2 n)

-- Implementar essa operação
-- (#) :: Num a => [[a]] -> [[a]] -> [[a]]
-- (#) = zipWith (zipWith (*))

-- Nossos caracteres serão do tipo haskell Text
-- Float, Integer, obederão ao default haskell