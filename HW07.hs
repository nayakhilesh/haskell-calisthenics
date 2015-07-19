{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
              a <- ma
              return (f a)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j vector = liftM2 (\elementI elementJ -> vector // [(i, elementJ), (j, elementI)]) (vector !? i) (vector !? j)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence (map f as)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indexes vector = mapM (\index -> vector !? index) indexes

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vector = case V.length vector of
                          0 -> return Nothing
                          len -> do
                                  rnd <- getRandomR (0, len - 1)
                                  return (Just (vector ! rnd))

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec len = V.replicateM len getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR len bounds = V.replicateM len (getRandomR bounds) 

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vector = if V.null vector then return V.empty
                 else shuffleHelper ((V.length vector) - 1) (return vector)

shuffleHelper :: Int -> Rnd (Vector a) -> Rnd (Vector a)
shuffleHelper index randVector
                | index == 0 = randVector
                | otherwise = shuffleHelper (index - 1) swappedVector
                where swappedVector = swapRndV index (getRandomR (0, index)) randVector

swapRndV :: Int -> Rnd Int -> Rnd (Vector a) -> Rnd (Vector a)
swapRndV i randomIndex randVector = do
                                    vector <- randVector
                                    j <- randomIndex
                                    return (vector // [(i, vector ! j), (j, vector ! i)])

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vector pivotIndex = let pivot = vector ! pivotIndex
                                    numPivots = V.length $ V.filter (== pivot) vector
                                in ((V.filter (< pivot) vector), pivot, (V.replicate (numPivots - 1) pivot) V.++ (V.filter (> pivot) vector))

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vector = if V.null vector then V.empty
               else (qsort left) V.++ (pivot `V.cons` (qsort right))
                where (left, pivot, right) = partitionAt vector 0

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vector = if V.null vector then return V.empty
                else do 
                      randomIndex <- getRandomR (0, (V.length vector) - 1)
                      let (left, pivot, right) = partitionAt vector randomIndex
                      sortedLeft <- qsortR left 
                      sortedRight <- qsortR right
                      return (sortedLeft V.++ (pivot `V.cons` sortedRight))

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i vector
          | i < 0 = return Nothing
          | i >= V.length vector = return Nothing
          | otherwise = do
                        randomIndex <- getRandomR (0, (V.length vector) - 1)
                        let (left, pivot, right) = partitionAt vector randomIndex
                        let leftLength = V.length left
                        if i < leftLength then select i left
                        else if i == leftLength then return $ Just pivot
                        else select (i  - leftLength - 1) right

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card label suit | suit <- suits, label <- labels ]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
          | V.null deck = Nothing
          | otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards numCards deck
                    | numCards <= 0 = Just ([], deck)
                    | V.length deck < numCards = Nothing
                    | otherwise = do
                                    (card, deck') <- nextCard deck
                                    (cards, remainingDeck) <- getCards (numCards - 1) deck' 
                                    return (card:cards, remainingDeck)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
