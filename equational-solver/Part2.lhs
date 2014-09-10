> module Part2 where

> {- ########################################## -}

         Predefined equations for testing

> {- ########################################## -}

> eq1 :: String
> eq1 = "x^2-2x-1=0"

> eq2 :: String
> eq2 = "(1-3x)*(5x+2)=0"

> eq3 :: String 
> eq3 = "9x*(2x-1)=0"

> eq4 :: String
> eq4 = "x^2-~12)x-9=0"

> eq5 :: String
> eq5 = "(x-7)^2=4*(x+8)"

> eq6 :: String 
> eq6 = "(x-4)^2-3*(x-1)=1"

> eq7 :: String
> eq7 = "(2+x)^2-(x-7)^2=x^2"

> eq8 :: String
> eq8 = "(5x-3)^2-(3x-4)^2=13x^2+17"


> {- ########################################## -}

               Data type definitons

> {- ########################################## -}

Arithmetic expression.

> data ArExpr = Nat Integer
>             | Neg ArExpr
>             | Bra ArExpr
>             | Add ArExpr ArExpr
>             | Sub ArExpr ArExpr
>             | Mul ArExpr ArExpr
>             | Pow ArExpr ArExpr
>             | Var Integer String
>             | VarR ArExpr String 
>             | Root Integer
>      deriving(Eq,Show)

Data type definition and instance declaration for an equation.

> data Equation = Eq { lPart, rPart :: ArExpr }

> instance Show Equation where
>   show (Eq l r) = (show l) ++ "=" ++ (show r)

Data type definiton and instance declaration for an equation in NF.
quadratic polynomial : a.x^2 + b.x + c

> data EquationNF = EqNF { a,b,c :: Double }
>   deriving (Eq,Show)

> data EquationSol = Sol (Double,Double)

> instance Show EquationSol where
>   show (Sol (s1,s2)) 
>     | ((s1 ==0) && (s1 == 0)) = "The equation has no solution"
>     | (s1 == 0)               = "The equation has the solution x1="++so2
>     | (s2 == 0)               = "The equation has the solution x1="++so1
>     | otherwise               = "The equation has the solution x1="++so1++" and the solution x2="++so2
>    where so1 = (show s1)
>          so2 = (show s2)

> {- ########################################## -}

               getX/isX functions

> {- ########################################## -}

> getNat :: ArExpr -> Integer 
> getNat (Nat i) = i

> getNeg :: ArExpr -> ArExpr
> getNeg (Neg a) = a

> isBra :: ArExpr -> Bool
> isBra (Bra _) = True
> isBra _       = False

> isAdd :: ArExpr -> Bool
> isAdd (Add _ _) = True
> isAdd _         = False

> isSub :: ArExpr -> Bool
> isSub (Sub _ _) = True
> isSub _         = False

> isNeg :: ArExpr -> Bool
> isNeg (Neg _)       = True
> isNeg _             = False

> isVar :: ArExpr -> Bool
> isVar (Var _ _)       = True
> isVar (Neg (Var _ _)) = True 
> isVar _               = False

> getVarNum :: ArExpr -> Integer
> getVarNum (Var i _) = i

> getVarStr :: ArExpr -> String
> getVarStr (Var _ s) = s

> getBra :: ArExpr -> ArExpr
> getBra (Bra a) = a

> {- ########################################## -}

             Equation Solving functions

> {- ########################################## -}

The following functions just returns the lhr (rhs) of an equation. 

> leftSide :: Equation -> ArExpr 
> leftSide (Eq l r) = l

> rightSide :: Equation -> ArExpr
> rightSide (Eq l r) = r

This function resolves a given quadratic equation.

> solveEquation :: Equation -> EquationSol
> solveEquation eq 
>  | rightSide eq == Nat 0 = 
>      case (isSimplified lhs) of 
>       True -> applyFormula (leftSide eq)
>       _    -> applyFormula (simplify (leftSide eq))
>  | otherwise             = 
>       applyFormula (simplify (rightToLeft eq))
>    where lhs   = leftSide eq

The following function generates the solution according to the well
known formular.
x1 = (p/2) + sqrt((p/2) -q)
x2 = (p/2) - sqrt((p/2) -q)
p = (b/a)
q = (c/a) 
(0,0,0) is the start value for the accumulator.

> applyFormula :: ArExpr -> EquationSol
> applyFormula a = applyFormula' eqNf
>  where eqNf = collectPar a (0,0,0)

> applyFormula' :: EquationNF -> EquationSol
> applyFormula' (EqNF a b c) = Sol (s1,s2) 
>   where p        = (b/a)
>         ph       = (p/2)
>         q        = (c/a)
>         s1       = (-ph) + sqrt ((square ph) - q)
>         s2       = (-ph) - sqrt ((square ph) - q)
>         square x = x*x

> {- ########################################## -}

       Simplification functions for Equations

> {- ########################################## -}

The function rightToLeft brings an equation into the form ArExpr = 0. 
This is the very first step when an equation is simplified if the right 
hand side is not equal to 0.  

> rightToLeft :: Equation -> ArExpr
> rightToLeft eq = case r of 
>                    (Neg x)           -> Add l x
>                    (Pow (Var _ _) _) -> Sub l r
>                    (Var _ _)         -> Sub l r
>                    (Add _ _)         -> Sub l r
>                    (Sub x y)         -> Sub l (Sub y x)
>                    _                 -> Sub l (simplify r)
>  where (l,r) = (leftSide eq,rightSide eq)

This function simply returns the values for a,b,c from an equation of the 
form a*x^2+b*x+c=0
It consumes the quadratical equation in normal form and an accumulator that 
we carry through the recursion. It returns a triple containing (a,b,c).

> collectPar :: ArExpr -> (Double,Double,Double) -> EquationNF 
> collectPar (Neg x) (i1,i2,i3)                 = 
>    case x of
>     (Nat i)                 -> EqNF i1 i2 (i3-(fromIntegral i))  
>     (Var i _)               -> EqNF i1 (i2-(fromIntegral i)) i3
>     (VarR (Root i) _)       -> EqNF i1 (i2-(sqrt(fromIntegral i))) i3
>     (Pow (Var i _) (Nat 2)) -> EqNF (i1-(fromIntegral i)) i2 i3
>     (Add i j)               -> collectPar (Neg j) (getTriple (collectPar (Neg x) (i1,i2,i3)))
>     (Sub i j)               -> collectPar (Neg i) (getTriple (collectPar j (i1,i2,i3))) 
>     _                       -> collectPar x (i1,i2,i3)
> collectPar (Nat i) (i1,i2,i3)                 = EqNF i1 i2 (i3+(fromIntegral i))
> collectPar (Var i _) (i1,i2,i3)               = EqNF i1 (i2+(fromIntegral i)) i3
> collectPar (VarR (Root i) _) (i1,i2,i3)       = EqNF i1 (i2+(sqrt(fromIntegral i))) i3
> collectPar (Pow (Var i _) (Nat 2)) (i1,i2,i3) = EqNF (i1+(fromIntegral i)) i2 i3
> collectPar a (i1,i2,i3) = case a of 
>                            (Add x y) -> collectPar y (getTriple (collectPar x (i1,i2,i3)))
>                            (Sub x y) -> 
>                               case y of 
>                                (Add i j) -> collectPar x (getTriple (collectPar (Neg j) (getTriple (collectPar (Neg i) (i1,i2,i3)))))
>                                _         -> collectPar (Neg y) (getTriple (collectPar x (i1,i2,i3)))

> getTriple :: EquationNF -> (Double,Double,Double)
> getTriple (EqNF a b c) = (a,b,c)

> addTriples :: EquationNF -> EquationNF -> EquationNF
> addTriples (EqNF a b c) (EqNF d e f) = EqNF (a+d) (b+e) (c+f)

> subTriples :: EquationNF -> EquationNF -> EquationNF
> subTriples (EqNF a b c) (EqNF d e f) = EqNF (a-d) (b-e) (c-f)

This function checks if a given arithmetic expression is simplified. 
Arithmetic expressions that contain an expression like x*y or (x+y)^2 
must be simplified first. 

> isSimplified :: ArExpr -> Bool
> isSimplified a = 
>  case a of  
>    (Nat x)   -> True
>    (Neg x)   -> True
>    (Bra x)   -> isSimplified x
>    (Pow a b) -> case a of 
>                 (Var i s) -> True
>                 _         -> isSimplified a
>    (Var i s) -> True
>    (Add x y) -> (isSimplified x) && (isSimplified y)
>    (Sub x y) -> (isSimplified x) && (isSimplified y)
>    otherwise -> False

This function simplifies an arithmetic expression.
It pattern matches on the value constructors for arithmetic expressions. 
Based on that, it calls the respective function that performs further steps. 

> simplify :: ArExpr -> ArExpr
> simplify a = case a of
>               (Nat i)               -> a
>               (Var i s)             -> a
>               (VarR _ _)            -> a
>               (Pow _ (Nat i))       -> a
>               (Add x y)             -> case y of 
>                                         (Neg i) -> (Sub (simplify x) (simplify i))
>                                         _       -> (Add (simplify x) (simplify y))
>               (Sub x y)             -> case y of 
>                                         (Neg i)   -> (Add (simplify x) (simplify i)) 
>                                         _         -> (Sub (simplify x) (simplify y))
>               (Mul x y)             -> simplify (simplifyMul x y) 
>               (Bra (Pow _ _))       -> simplify (simplifySquare a)
>               (Bra x)               -> simplify x

> {- ########################################## -}

             Simplify Multiplication

> {- ########################################## -}
 
> simplifyMul :: ArExpr -> ArExpr -> ArExpr
> simplifyMul a b 
>  | (isBra a) && (isBra b) = mulBrac a b
>  | (isBra a)              = mulOneBrac b a 
>  | (isBra b)              = mulOneBrac a b
>  | otherwise              = error "invalid multiplication simplifyMul"

This function simply handels all cases when two arithmetic expressions 
(sorrouned by brackets) are multiplied. 

> mulBrac :: ArExpr -> ArExpr -> ArExpr
> mulBrac a b = res 
>  where (a',a'')  = case (getBra a) of 
>                     (Add x y) -> (x,y)
>                     (Sub x y) -> (x,Neg y)
>        (b',b'')  = case (getBra b) of
>                     (Add x y) -> (x,y)  
>                     (Sub x y) -> (x,Neg y)
>        res       = Add (simpleAdd fst snd) (simpleAdd thr fou)
>        fst       = simpleMul a' b' 
>        snd       = simpleMul a' b''
>        thr       = simpleMul a'' b'
>        fou       = simpleMul a'' b''

> mulOneBrac :: ArExpr -> ArExpr -> ArExpr
> mulOneBrac a b = case a of 
>                   (Nat x)   -> mulOneBracNat x (a',a'') 
>                   (Var i s) -> mulOneBracVar a (a',a'')
>                   _         -> error "invalid argument inside mulOneBrac"
>  where (a',a'') = case (getBra b) of 
>                    (Add x y) -> (x,y) 
>                    (Sub x y) -> (x,Neg y)

> mulOneBracNat :: Integer -> (ArExpr,ArExpr) -> ArExpr
> mulOneBracNat n (a',a'') = 
>   case (isVar a',isVar a'') of 
>    (False,False) -> if (isNeg a'') 
>                     then Sub (f n a') (f n (getNeg a''))
>                     else Add (f n a') (f n a'')
>    (False,True)  -> if (isNeg a'') 
>                     then Sub (f n a') (g n (getNeg a''))
>                     else Add (f n a') (g n a'')
>    (True,False)  -> if (isNeg a'') 
>                     then Sub (g n a') (f n (getNeg a''))
>                     else Add (g n a') (f n a'') 
>    (True,True)   -> if (isNeg a'') 
>                     then Sub (g n a') (g n (getNeg a''))
>                     else Add (g n a') (g n a'') 
>    where f x y = Nat (x*(getNat y))
>          g x y = Var (x*(getVarNum y)) (getVarStr y) 

> mulOneBracVar :: ArExpr -> (ArExpr,ArExpr) -> ArExpr
> mulOneBracVar v (a',a'') = 
>   case (isVar a',isVar a'') of
>    (False,False) -> if (isNeg a'')
>                     then Sub (g a' v) (g (getNeg a'') v)
>                     else Add (g a' v) (g a'' v)
>    (False,True)  -> if (isNeg a'')
>                     then Sub (g a' v) (g (getNeg a'') v)
>                     else Add (g a' v) (g a'' v)
>    (True,False)  -> if (isNeg a'')
>                     then Sub (g a' v) (g (getNeg a'') v)
>                     else Add (g a' v) (g a'' v)
>    (True,True)   -> if (isNeg a'')
>                     then Sub (g a' v) (g (getNeg a'') v)
>                     else Add (g a' v) (g a'' v) 
>    where g x y = case x of 
>                    (Var i s) -> Pow (Var (i*(getVarNum y)) (getVarStr y)) (Nat 2)
>                    _         -> Var ((getNat x)*(getVarNum y)) (getVarStr y)

This function multiplies two arithemic expressions and returns the result. 

> simpleMul :: ArExpr -> ArExpr -> ArExpr
> simpleMul a b = case (isNeg a,isNeg b) of
>                   (False,False) -> f a b
>                   (False,True)  -> Neg (f a (getNeg b))
>                   (True,False)  -> Neg (f (getNeg a) b)
>                   (True,True)   -> f (getNeg a) (getNeg b)
>  where f x y = case (isVar x,isVar y) of
>                  (False,False) -> Nat ((getNat x)*(getNat y))
>                  (False,True)  -> Var ((getNat x) * (getVarNum y)) (getVarStr y) 
>                  (True,False)  -> Var ((getVarNum x) * (getNat y)) (getVarStr x)
>                  (True,True)   -> Pow (Var ((getVarNum x) * (getVarNum y)) (getVarStr x)) (Nat 2)

This function adds two arithmetic expressions and returns the result

> simpleAdd :: ArExpr -> ArExpr -> ArExpr
> simpleAdd a b = case (isNeg a,isNeg b) of
>                  (False,False) -> Add a b
>                  (False,True)  -> Sub a (getNeg b)
>                  (True,False)  -> Sub b (getNeg a)
>                  (True,True)   -> Neg(Bra(Add (getNeg a) (getNeg b)))

> {- ########################################## -}

             Simplify ArExpr^2

> {- ########################################## -}

This function simplifies an arithmetic expression of the form (x+y)^2.
If the index of the pow is not equal 2, then it is not a valid quadratical
equation. 

> simplifySquare :: ArExpr -> ArExpr
> simplifySquare (Bra (Pow a (Nat i))) = if (i /= 2) 
>                                        then error "index of pow not equal two!"
>                                        else calcSquare a 
> simplifySquare _                     = error "you are calling simplifySquare incorrectly!"

> calcSquare :: ArExpr -> ArExpr 
> calcSquare a = mulBrac (Bra a) (Bra a)
