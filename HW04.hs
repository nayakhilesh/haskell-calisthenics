{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P as) (P bs) = length as == length bs && foldr (\(a, b) acc -> acc && a == b) True (zip as bs) 
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P as) = if toString == "" 
                  then "0" 
                  else toString
                  where toString = show' as 0

show' :: (Num a, Eq a, Show a) => [a] -> Int -> String 
show' [] _ = ""
show' [a] expo = coefficientAndExpoToString a expo
show' (a:as) expo = if prefix /= "" && suffix /= ""
                    then prefix ++ " + " ++ suffix
                    else if prefix /= ""
                          then prefix
                          else suffix
                    where 
                      prefix = show' as (expo + 1)
                      suffix = coefficientAndExpoToString a expo

coefficientAndExpoToString :: (Num a, Eq a, Show a) => a -> Int -> String
coefficientAndExpoToString coefficient expo
                                      | coefficient == 0 = ""
                                      | coefficient == 1 = if expo > 0 
                                                            then expoToVarString expo 
                                                            else show coefficient
                                      | otherwise = show coefficient ++ expoToVarString expo

expoToVarString :: Int -> String
expoToVarString expo 
                | expo <= 0 = ""
                | expo == 1 = "x"
                | otherwise = "x^" ++ show expo

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

