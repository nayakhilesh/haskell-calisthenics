{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons first rest) = first : streamToList rest

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons first rest) = Cons (f first) (fmap f rest)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f seed = Cons seed (sIterate f (f seed))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons first rest) secondStream = Cons first (sInterleave secondStream rest)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons first rest) = first : sTake (n - 1) rest

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = foldr1 sInterleave (map sRepeat [0..])

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = Cons random (rand random)
            where random = (1103515245 * seed + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 221 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 213 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (first:rest) = 
    case (minMax rest) of 
                    Nothing -> Just (first, first)
                    Just (restMin, restMax) -> Just (min first restMin, max first restMax)

{- Total Memory in use: 203 MB -}
minMaxFast :: [Int] -> Maybe (Int, Int)
minMaxFast [] = Nothing
minMaxFast xs = minMaxFastHelper minBound maxBound xs

minMaxFastHelper :: Int -> Int -> [Int] -> Maybe (Int, Int)
minMaxFastHelper prevMin prevMax [] = Just (prevMin, prevMax)
minMaxFastHelper prevMin prevMax (first:rest) = minMaxFastHelper newMin newMax rest
                                                 where 
                                                    newMin = min prevMin first
                                                    newMax = max prevMax first

main :: IO ()
main = print $ minMaxFast $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
