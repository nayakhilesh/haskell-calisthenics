-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (input, outputList) = toRevDigits input == outputList

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits test" testToRevDigits
             [(1234, [4, 3, 2, 1]), (5, [5]), (0, []), (-5, [])]
           ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (input, output) = doubleEveryOther input == output

ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
             [([], []), ([1], [1]), ([1, 2],[1, 4]), ([1, -2, 3, 4], [1, -4, 3, 8]),
              ([0, 0], [0, 0])]
           ]

-- Exercise 4 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (input, output) = sumDigits input == output

ex4Tests :: [Test]
ex4Tests = [ Test "sumDigits test" testSumDigits
             [([], 0), ([0], 0), ([-1], 0), ([10, 5, 18, 4], 19), ([10, -5, 18], 10)]
           ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (input, output) = luhn input == output

ex5Tests :: [Test]
ex5Tests = [ Test "luhn test" testLuhn
             [(5594589764218858, True), (1234567898765432, False)]
           ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, sourcePeg, usingPeg, targetPeg, moves) = hanoi n sourcePeg usingPeg targetPeg == moves

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi test" testHanoi
             [(1, "p", "q", "r", [("p", "r")]), 
              (2, "a", "b", "c", [("a","b"), ("a","c"), ("b","c")])]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
