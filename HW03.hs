module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend currentState varName varValue = (\lookupVarName -> if lookupVarName == varName then varValue else currentState lookupVarName) 

empty :: State
empty = (\_ -> 0)

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE currentState (Var varName) = currentState varName
evalE _ (Val intLiteral) = intLiteral
evalE currentState (Op expr1 bop expr2) = case bop of
                                            Plus -> evaluatedExpr1 + evaluatedExpr2
                                            Minus -> evaluatedExpr1 - evaluatedExpr2
                                            Times -> evaluatedExpr1 * evaluatedExpr2
                                            Divide -> evaluatedExpr1 `div` evaluatedExpr2
                                            Gt -> boolToInt $ evaluatedExpr1 > evaluatedExpr2
                                            Ge -> boolToInt $ evaluatedExpr1 >= evaluatedExpr2
                                            Lt -> boolToInt $ evaluatedExpr1 < evaluatedExpr2
                                            Le -> boolToInt $ evaluatedExpr1 <= evaluatedExpr2
                                            Eql -> boolToInt $ evaluatedExpr1 == evaluatedExpr2
                                          where
                                            evaluatedExpr1 = evalE currentState expr1
                                            evaluatedExpr2 = evalE currentState expr2 

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0 

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign varName expression) = DAssign varName expression
desugar (Incr varName) = DAssign varName (Op (Var varName) Plus (Val 1))
desugar (If condition trueStatement falseStatement) = DIf condition (desugar trueStatement) (desugar falseStatement)
desugar (While condition statement) = DWhile condition (desugar statement)
desugar (For initialization condition update statement) = DSequence (desugar initialization) (DWhile condition (DSequence (desugar statement) (desugar update)))
desugar (Sequence statement1 statement2) = DSequence (desugar statement1) (desugar statement2)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple currentState (DAssign varName expression) = extend currentState varName $ evalE currentState expression
evalSimple currentState (DIf condition trueStatement falseStatement) = if intToBool $ evalE currentState condition
                                                                       then evalSimple currentState trueStatement
                                                                       else evalSimple currentState falseStatement
evalSimple currentState (DWhile condition statement) = evalWhile currentState condition statement
evalSimple currentState (DSequence statement1 statement2) = let newState = evalSimple currentState statement1
                                                            in evalSimple newState statement2
evalSimple currentState DSkip = currentState

evalWhile :: State -> Expression -> DietStatement -> State
evalWhile currentState condition statement = if intToBool $ evalE currentState condition
                                             then let newState = evalSimple currentState statement
                                                  in evalWhile newState condition statement
                                             else currentState

intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

run :: State -> Statement -> State
run currentState statement = evalSimple currentState $ desugar statement 

-- handy function

createStateWithVariable :: String -> Int -> State
createStateWithVariable varName varValue = extend empty varName varValue

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
