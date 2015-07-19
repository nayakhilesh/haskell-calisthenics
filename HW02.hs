{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches (_:_) [] = lengthOfCodesIsNotEqual
exactMatches [] (_:_) = lengthOfCodesIsNotEqual
exactMatches [] [] = 0
exactMatches (actualHead:actualRest) (guessHead:guessRest) = if actualHead == guessHead
                                                             then 1 + exactMatches actualRest guessRest
                                                             else exactMatches actualRest guessRest

lengthOfCodesIsNotEqual = error "length of actual and guess codes is not equal"

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map countColor colors
                   where countColor color = length $ filter (== color) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum $ zipWith min (countColors actual) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = Move guess numExactMatches numNonExactMatches
                        where 
                            numExactMatches = exactMatches actual guess
                            numNonExactMatches = (matches actual guess) - numExactMatches

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess numExactMatches numNonExactMatches) providedCode =
                        let Move _ providedCodeExactMatches providedCodeNonExactMatches = getMove guess providedCode
                        in numExactMatches == providedCodeExactMatches && numNonExactMatches == providedCodeNonExactMatches

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter $ isConsistent move

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [ color : shorterCode | color <- colors, shorterCode <- allCodes (n - 1) ]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve [] = []
solve secretCode = solve' secretCode initialMove possibilities
                where
                    initialMove = getMove secretCode guess
                    guess = head possibilities
                    possibilities = allCodes $ length secretCode 

solve' :: Code -> Move -> [Code] -> [Move]
solve' _ _ [] = error "Impossible!"
solve' _ _ [_] = []
solve' secretCode move possibilities = 
    let 
        newPossibilities = filterCodes move possibilities
        guess = head newPossibilities
    in move : solve' secretCode (getMove secretCode guess) newPossibilities

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
