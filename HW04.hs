{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P as) (P bs) = length normalizedAs == length normalizedBs 
                          && foldr (\(a, b) acc -> acc && a == b) True (zip normalizedAs normalizedBs)
                          where normalizedAs = normalize as
                                normalizedBs = normalize bs

normalize :: (Num a, Eq a) => [a] -> [a]
normalize = reverse . dropWhile (== 0) . reverse 
 
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
plus (P as) (P bs) = P (plus' as bs)

plus' [] bs = bs
plus' as [] = as
plus' (a:as) (b:bs) = (a + b) : plus' as bs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P [0]
times _ (P []) = P [0]
times (P as) (P bs) = foldr (+) (P [0]) (times' as bs)

times' as bs = map (\(a, index) -> 
                          P (map (\b -> a * b) 
                                  (zeros index ++ bs)
                             )
                    )
                    asWithIndex
                where 
                  asWithIndex = zip as [0,1..]

zeros :: Num a => Int -> [a]
zeros num
        | num <= 0 = []
        | otherwise = 0 : zeros (num - 1)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P as) = P (map (\a -> -a) as)
    fromInteger integer = P [fromInteger integer]
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

