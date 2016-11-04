-- A try to solve well-formed formula by tablueau

import           Data.List

-- |Variable name
-- |example) P, Q, R
data VarName = VarName String deriving (Eq)

-- | Show for VarName
instance Show VarName where
  show (VarName n) = n

-- |Well-formed formula
data Formula  =
      Var VarName |
      Not Formula |
      And Formula Formula |
      Or  Formula Formula
      deriving (Show, Eq)

-- |Implication
-- | A => B
impl :: Formula -> Formula -> Formula
impl a b = Not a `Or` b

-- |Tabueau result
-- |example) `TabRes True (VarName "P")` represens "if P is true, a formula satisfies."
data TabRes = TabRes Bool VarName  deriving (Eq)

-- |Show for TabRes
instance Show TabRes where
  show (TabRes True v)  = show v ++ "==" ++ "t"
  show (TabRes False v) = show v ++ "==" ++ "f"


-- |Judges two TabRes are consistent or not
isConsistent :: TabRes -> TabRes -> Bool
isConsistent (TabRes b1 n1) (TabRes b2 n2) = b1 == b2 || n1 /= n2

-- |Judges a list of `TabRes`s is consistent or not
areConsistent :: [TabRes] -> Bool
areConsistent []     = False
areConsistent [_]    = True
areConsistent (x:xs) =
  all (isConsistent x) xs && areConsistent xs

-- |Solve by tabueau
-- |
-- |return meaning)
-- | example) [[TabRes True Q, TabRes True P], [TabRes False P]]
-- | it means "the given fomula is satisfied, if (Q == true && P == true) || (P is false)"
-- | so, if return is [], meaning is inconsisitent or no satisfiable
solve :: Formula -> [[TabRes]]
solve (Var name)             = [[TabRes True name]]
solve (Not (Var name))       = [[TabRes False name]]
solve (Not (Not e))          = solve e                         -- Double Negation
solve (Not (And e1 e2))      = solve $ Or  (Not e1) (Not e2)   -- De Morgan's laws
solve (Not (Or  e1 e2))      = solve $ And (Not e1) (Not e2)   -- De Morgan's laws
solve (Or  e1 e2)            = solve e1 ++ solve e2
solve (And e1 e2)            = do
  r1  <- solve e1
  r2  <- solve e2
  [r1 ++ r2 | areConsistent (r1 ++ r2) {- areConsistent is for pruning -} ]

-- | test
test1 = do
  let p = VarName "P"
      q = VarName "Q"
      r = VarName "R"

  -- !P
  print $ solve $ Not (Var p)
  -- P ^ Q
  print $ solve $ And (Var p) (Var q)
  -- P v Q
  print $ solve $ Or  (Var p) (Var q)
  print $ solve $ Not (And (Var p) (Not (Var q)))
  -- !(P ^ Q) ^ ((P v Q) ^ !Q)
  print $ solve $ And (Not (And (Var p) (Not (Var q)))) (And (Or (Var p) (Var q)) (Not (Var q)))

test2 = do
  let p = VarName "P"
      q = VarName "Q"
      r = VarName "R"

  print $ isConsistent (TabRes True p) (TabRes True p)
  print $ isConsistent (TabRes True p) (TabRes False p)
  print $ isConsistent (TabRes True p) (TabRes False q)

  print $ areConsistent [TabRes True p, TabRes False q]
  print $ areConsistent [TabRes True p, TabRes False q, TabRes False p]


-- |Solve: all abc patterns in a + b = c (a, b, c are element of {0, 1, 2})
-- |examples)
-- | one example: a = 1, b = 1, c = 2
-- | one example: a = 0, b = 0, c = 0
aPlusBisC = do
  let a0 = Var $ VarName "a=0"
      a1 = Var $ VarName "a=1"
      a2 = Var $ VarName "a=2"
      b0 = Var $ VarName "b=0"
      b1 = Var $ VarName "b=1"
      b2 = Var $ VarName "b=2"
      c0 = Var $ VarName "c=0"
      c1 = Var $ VarName "c=1"
      c2 = Var $ VarName "c=2"
      r0 = Var $ VarName "res=0"
      r1 = Var $ VarName "res=1"
      r2 = Var $ VarName "res=2"
      r3 = Var $ VarName "res=3"
      r4 = Var $ VarName "res=4"


      anyA = (a0 `And` Not a1 `And` Not a2) `Or` (Not a0 `And` a1 `And` Not a2) `Or` (Not a0 `And` Not a1 `And` a2)
      anyB = (b0 `And` Not b1 `And` Not b2) `Or` (Not b0 `And` b1 `And` Not b2) `Or` (Not b0 `And` Not b1 `And` b2)
      anyC = (c0 `And` Not c1 `And` Not c2) `Or` (Not c0 `And` c1 `And` Not c2) `Or` (Not c0 `And` Not c1 `And` c2)
      anyR = (r0 `And` Not r1 `And` Not r2 `And` Not r3 `And` Not r4) `Or`
             (Not r0 `And` r1 `And` Not r2 `And` Not r3 `And` Not r4) `Or`
             (Not r0 `And` Not r1 `And` r2 `And` Not r3 `And` Not r4) `Or`
             (Not r0 `And` Not r1 `And` Not r2 `And` r3 `And` Not r4) `Or`
             (Not r0 `And` Not r1 `And` Not r2 `And` Not r3 `And` r4)


      a0b0 = (a0 `And` b0) `impl` r0
      a0b1 = (a0 `And` b1) `impl` r1
      a0b2 = (a0 `And` b2) `impl` r2
      a1b0 = (a1 `And` b0) `impl` r1
      a1b1 = (a1 `And` b1) `impl` r2
      a1b2 = (a1 `And` b2) `impl` r3
      a2b0 = (a2 `And` b0) `impl` r2
      a2b1 = (a2 `And` b1) `impl` r3
      a2b2 = (a2 `And` b2) `impl` r4
      abOpeRule = a0b0 `And` a0b1 `And` a0b2 `And` a1b0 `And` a1b1 `And` a1b2 `And` a2b0 `And` a2b1 `And` a2b2

      r0c0 = r0 `And` c0
      r1c1 = r1 `And` c1
      r2c2 = r2 `And` c2

      problem =
        (anyA `And` anyB `And` anyC `And` anyR `And` abOpeRule) `And` (r0c0 `Or` r1c1 `Or` r2c2)

  -- solve a+b=c problem
  mapM_ print (nub (map nub (solve problem)))


-- |Solve: all abc patterns in a + b = a (a, b are a element of {0, 1, 2})
-- |examples)
-- | one example: a = 0, b = 0
-- | one example: a = 1, b = 0
aPlusBisA = do
  let a0 = Var $ VarName "a=0"
      a1 = Var $ VarName "a=1"
      a2 = Var $ VarName "a=2"
      b0 = Var $ VarName "b=0"
      b1 = Var $ VarName "b=1"
      b2 = Var $ VarName "b=2"
      r0 = Var $ VarName "res=0"
      r1 = Var $ VarName "res=1"
      r2 = Var $ VarName "res=2"
      r3 = Var $ VarName "res=3"
      r4 = Var $ VarName "res=4"

      -- if a = 0, then a /= 1 and a /= 2 ...
      anyA = (a0 `And` Not a1 `And` Not a2) `Or` (Not a0 `And` a1 `And` Not a2) `Or` (Not a0 `And` Not a1 `And` a2)
      anyB = (b0 `And` Not b1 `And` Not b2) `Or` (Not b0 `And` b1 `And` Not b2) `Or` (Not b0 `And` Not b1 `And` b2)
      anyR = (r0 `And` Not r1 `And` Not r2 `And` Not r3 `And` Not r4) `Or`
             (Not r0 `And` r1 `And` Not r2 `And` Not r3 `And` Not r4) `Or`
             (Not r0 `And` Not r1 `And` r2 `And` Not r3 `And` Not r4) `Or`
             (Not r0 `And` Not r1 `And` Not r2 `And` r3 `And` Not r4) `Or`
             (Not r0 `And` Not r1 `And` Not r2 `And` Not r3 `And` r4)


      -- if a = 0 and b = 0, then result = 0 ...
      a0b0 = (a0 `And` b0) `impl` r0
      a0b1 = (a0 `And` b1) `impl` r1
      a0b2 = (a0 `And` b2) `impl` r2
      a1b0 = (a1 `And` b0) `impl` r1
      a1b1 = (a1 `And` b1) `impl` r2
      a1b2 = (a1 `And` b2) `impl` r3
      a2b0 = (a2 `And` b0) `impl` r2
      a2b1 = (a2 `And` b1) `impl` r3
      a2b2 = (a2 `And` b2) `impl` r4
      abOpeRule = a0b0 `And` a0b1 `And` a0b2 `And` a1b0 `And` a1b1 `And` a1b2 `And` a2b0 `And` a2b1 `And` a2b2

      r0c0 = r0 `And` a0
      r1c1 = r1 `And` a1
      r2c2 = r2 `And` a2

      problem =
        (anyA `And` anyB `And` anyR `And` abOpeRule) `And` (r0c0 `Or` r1c1 `Or` r2c2)

  -- solve a+b=a problem
  mapM_ print (nub (map nub (solve problem)))



main :: IO ()
main = do
  aPlusBisA
