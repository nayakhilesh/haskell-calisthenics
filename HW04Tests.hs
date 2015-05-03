module HW04Tests where

import HW04
import Testing

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [  testF2 "== test" (==)
              [ (P [1, 2, 3], P [1, 2, 3], True)
              , (P [1, 2, 3, 4], P [1, 2, 3], False)
              , (P [1, 2, 3], P [1, 2, 3, 4], False)
              , (P [1.0, 2.0, 3], P [1, 2, 3.0], True)
              , (P [-1.0, 2.0, 3], P [-1, 2, 3.0], True)
              ]
           ,  
              testF2 "/= test" (/=)
              [ (P [1, 2, 3], P [1, 2, 3], False)
              , (P [1, 2, 3, 4], P [1, 2, 3], True)
              , (P [1, 2, 3], P [1, 2, 3, 4], True)
              , (P [1.0, 2.0, 3], P [1, 2, 3.0], False)
              , (P [-1.0, 2.0, 3], P [-1, 2, 3.0], False)
              ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF1 "show test" show
             [ (P [], "0")
             , (P [0], "0")
             , (P [0, 0, 0], "0")
             , (P [0, 5], "5x")
             , (P [4, 5], "5x + 4")
             , (P [1, 2, 0], "2x + 1")
             , (P [0, 1, 2], "2x^2 + x")
             , (P [0, 1, 1], "x^2 + x")
             , (P [3, 5, 2], "2x^2 + 5x + 3")
             , (P [-1, 0, 0, 2], "2x^3 + -1")
             ]
           ]

-- Exercise 4 -----------------------------------------
-- Exercise 5 -----------------------------------------
-- All Tests ------------------------------------------

{-allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  ]-}
