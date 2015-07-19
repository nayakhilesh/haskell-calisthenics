{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = flip mod 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = flip quot 10 

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
            | n <= 0 = []
            | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = zipWith (curry doubleEven) list ([1 ..] :: [Integer])
                        where doubleEven (integer, index)
                                            | even index = integer * 2
                                            | otherwise = integer

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
(.>) = flip (.)
sumDigits :: [Integer] -> Integer
sumDigits = foldr (toRevDigits .> sum .> (+)) 0 

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = toRevDigits .> doubleEveryOther .> sumDigits .> lastDigit .> (== 0)

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n sourcePeg usingPeg targetPeg
      | n < 1 = error "cannot move non-positive number of discs"
      | n == 1 = [(sourcePeg, targetPeg)]
      | otherwise =
        let initialMoves = hanoi (n - 1) sourcePeg targetPeg usingPeg
            intermediateMove = (sourcePeg, targetPeg)
            finalMoves = hanoi (n - 1) usingPeg sourcePeg targetPeg
        in initialMoves ++ [intermediateMove] ++ finalMoves

