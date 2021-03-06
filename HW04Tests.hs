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
              , (P [0], P [], True)
              , (P [0, 0], P [0], True)
              , (P [1, 2, 3, 0], P [1, 2, 3, 0, 0], True)
              , (P [1, 0, 3, 0], P [1, 0, 3, 0, 0], True)
              ]
           ,  
              testF2 "/= test" (/=)
              [ (P [1, 2, 3], P [1, 2, 3], False)
              , (P [1, 2, 3, 4], P [1, 2, 3], True)
              , (P [1, 2, 3], P [1, 2, 3, 4], True)
              , (P [1.0, 2.0, 3], P [1, 2, 3.0], False)
              , (P [-1.0, 2.0, 3], P [-1, 2, 3.0], False)
              , (P [0], P [], False)
              , (P [0, 0], P [0], False)
              , (P [1, 2, 3, 0], P [1, 2, 3, 0, 0], False)
              , (P [1, 0, 3, 0], P [1, 0, 3, 0, 0], False)
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

ex4Tests :: [Test]
ex4Tests = [ testF2 "plus test" plus
             [ (P [], P [], P [])
             , (P [0], P[1], P [1])
             , (P [1, 2], P [3, 4, 5], P [4, 6, 5])
             , (P [1, 2.1, -3.2], P [2.1, -1.1], P [3.1, 1, -3.2])
             , (P [1, 2, 0, 0], P [3, 4, 5, 0, 0], P [4, 6, 5, 0, 0])
             ]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF2 "times test" times
             [ (P [], P [], P [0])
             , (P [1], P[], P [0])
             , (P [], P[1], P [0])
             , (P [0], P[1], P [0])
             , (P [1, 2], P [3, 4, 5], P [3, 10, 13, 10])
             , (P [1, 1, 1], P [2, 2], P [2, 4, 4, 2])
             ]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ testF1 "negate test" negate
             [ (P [], P [])
             , (P [1], P[-1])
             , (P [-1], P[1])
             , (P [1, -2], P[-1, 2])
             , (P [-1, 2], P[1, -2])
             ]
           ,
             testF1 "fromInteger test" fromInteger
             [ (4, P [4])
             , (5, P [5.0])
             ]
           ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = [ testF2 "applyP test" applyP
             [ (P [], 1, 0)
             , (P [], 2, 0)
             , (3*x^2 + 2*x + 4, 2, 20)
             , (3*x^2 - 2*x + 4, 3, 25)
             ]
           ]

-- Exercise 8 -----------------------------------------

ex8Tests :: [Test]
ex8Tests = [ testF2 "nderiv test" nderiv
             [ (0, x, x)
             , (1, x, 1)
             , (1, 2*x + 1, 2)
             , (2, 2*x + 1, 0)
             , (1, 3*x^2 + 2*x + 4, 6*x + 2)
             , (2, 3*x^2 + 2*x + 4, 6)
             ]
           ]

-- Exercise 9 -----------------------------------------

ex9Tests :: [Test]
ex9Tests = [ testF1 "deriv test" deriv
             [ (P [], 0)
             , (P [0], 0)
             , (2*x + 1, 2)
             , (3*x^2 + 2*x + 4, 6*x + 2)
             , (7*x^2 - 5*x + 6, 14*x - 5)
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , ex8Tests
                  , ex9Tests
                  ]
