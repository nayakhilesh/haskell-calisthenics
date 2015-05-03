module HW03Tests where

import HW03
import Testing


-- Exercise 1 -----------------------------------------

instance Show (a -> b) where
  show _ = "<function>"

testExtend :: (State, String, Int) -> Bool
testExtend (state, varName, varValue) = state varName == varValue

ex1Tests :: [Test]
ex1Tests = [  testF1 "empty test" empty
              [ ("A", 0)
              , ("B", 0)
              ]
           ,  Test "extend test" testExtend
              [ (extend empty "A" 1, "A", 1)
              , (extend (extend empty "A" 1) "B" 2, "B", 2)
              , (extend (extend empty "A" 1) "B" 2, "C", 0)
              ]
           ]

-- Exercise 2 -----------------------------------------

testState = extend (extend empty "A" 2) "B" 3

ex2Tests :: [Test]
ex2Tests = [ testF2 "evalE test" evalE
             [ (testState, Var "A", 2)
             , (testState, Val 5, 5)
             , (testState, Op (Var "A") Plus (Var "B"), 5)
             , (testState, Op (Var "A") Minus (Var "B"), -1)
             , (testState, Op (Var "A") Times (Var "B"), 6)
             , (testState, Op (Var "B") Divide (Var "A"), 1)
             , (testState, Op (Var "A") Divide (Var "B"), 0)
             , (testState, Op (Var "A") Gt (Var "B"), 0)
             , (testState, Op (Var "A") Ge (Val 2), 1)
             , (testState, Op (Var "A") Lt (Var "B"), 1)
             , (testState, Op (Var "A") Le (Val 2), 1)
             , (testState, Op (Var "A") Eql (Val 2), 1)
             ]
           ]

-- Exercise 3 -----------------------------------------

statement = Incr "C"

ex3Tests :: [Test]
ex3Tests = [ testF1 "desugar test" desugar
             [ (Assign "A" (Val 2), DAssign "A" (Val 2))
             , (Incr "B", DAssign "B" (Op (Var "B") Plus (Val 1)))
             , (If (Var "C") statement statement, 
                  DIf (Var "C") (DAssign "C" (Op (Var "C") Plus (Val 1))) (DAssign "C" (Op (Var "C") Plus (Val 1))))
             , (While (Var "C") statement, 
                  DWhile (Var "C") (DAssign "C" (Op (Var "C") Plus (Val 1))))
             , (For (Assign "A" (Val 0)) (Op (Var "A") Lt (Var "N")) (Incr "A") (Incr "C"), 
                  DSequence (DAssign "A" (Val 0)) 
                            (DWhile (Op (Var "A") Lt (Var "N")) 
                              (DSequence (DAssign "C" (Op (Var "C") Plus (Val 1))) 
                                          (DAssign "A" (Op (Var "A") Plus (Val 1))))))
             , (Sequence (Incr "A") (Incr "B"),
                  DSequence (DAssign "A" (Op (Var "A") Plus (Val 1)))
                            (DAssign "B" (Op (Var "B") Plus (Val 1))))
             , (Skip, DSkip)
             ]
           ]

-- Exercise 4 -----------------------------------------

testEvalSimple :: (State, DietStatement, String, Int) -> Bool
testEvalSimple (currentState, statement, expectedVarName, expectedVarValue) = 
                              (evalSimple currentState statement) expectedVarName == expectedVarValue 

testRun :: (State, Statement, String, Int) -> Bool
testRun (currentState, statement, expectedVarName, expectedVarValue) =
                              (run currentState statement) expectedVarName == expectedVarValue

ex4Tests :: [Test]
ex4Tests = [ Test "evalSimple test" testEvalSimple
             [ (testState, DAssign "C" (Val 4), "C", 4)
             , (testState, DIf (Op (Var "A") Eql (Val 2)) (DAssign "C" (Val 4)) (DAssign "C" (Val 5)), "C", 4)
             , (testState, DIf (Op (Var "A") Eql (Val 3)) (DAssign "C" (Val 4)) (DAssign "C" (Val 5)), "C", 5)
             , (testState, DWhile (Op (Var "A") Le (Val 6)) (DAssign "A" (Op (Var "A") Plus (Val 1))), "A", 7)
             , (testState, DSequence (DAssign "C" (Val 4)) (DAssign "A" (Var "C")), "A", 4)
             , (testState, DSkip, "A", 2)
             , (testState, DSkip, "B", 3)
             ]
           ,
             Test "run test" testRun
             [ (extend empty "In" 4, factorial, "Out", 24)
             , (extend empty "In" 5, factorial, "Out", 120)
             , (extend empty "A" 4, squareRoot, "B", 2)
             , (extend empty "A" 9, squareRoot, "B", 3)
             , (extend empty "A" 15, squareRoot, "B", 3)
             , (extend empty "In" 0, fibonacci, "Out", 1)
             , (extend empty "In" 1, fibonacci, "Out", 1)
             , (extend empty "In" 2, fibonacci, "Out", 2)
             , (extend empty "In" 3, fibonacci, "Out", 3)
             , (extend empty "In" 4, fibonacci, "Out", 5)
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  ]
