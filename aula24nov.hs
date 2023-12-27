module Ficha8 where

data Exp a = Const a
		| Simetrico (Exp a)
		| Mais (Exp a) (Exp a)
		| Menos (Exp a) (Exp a)
		| Mult (Exp a) (Exp a)
		deriving Show


infixa :: Show a => Exp a -> String
infixa (Const x) = show x
infixa (Simetrico e) = "(-" ++ (infixa e) ++ ")"
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos y1 y2) = "(" ++ infixa y1 ++ "+" ++ infixa y2 ++ ")"
infixa (Mult z1 z2) = "(" ++ infixa z1 ++ "+" ++ infixa z2 ++ ")"


calcula :: Num a => Exp a -> a 
calcula (Const x) = x
calcula (Simetrico e) = - (calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos y1 y2) = calcula y1 - calcula y2
calcula (Mult z1 z2) = calcula z1 * calcula z2 


signumExp :: Num a => Exp a -> Exp a 
signumExp e = Const $ signum (calcula e)

fromIntegerExp :: Num a => Integer -> Exp a 
fromIntegerExp i = Const (fromInteger i)