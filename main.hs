-- A try to solve well-formed formula by tablueau

-- |Variable name
-- |example) P, Q, R
data VarName = VarName String deriving (Eq)

-- | Show for VarName
instance Show VarName where
  show (VarName n) = n

-- |Well-formed formula
data Formula  = Var VarName |
                Not Formula |
                And Formula Formula |
                Or  Formula Formula
                deriving (Show, Eq)

-- |Tabueau result
-- |example) `EqTrue (VarName "P")` represens "if P is true, a formula satisfies."
data TabRes = EqTrue VarName | EqFalse VarName deriving (Eq)

-- |Show for TabRes
instance Show TabRes where
  show (EqTrue v)  = show v ++ "==" ++ "1"
  show (EqFalse v) = show v ++ "==" ++ "0"


-- |Judges two TabRes are consistent or not
isConsistent :: TabRes -> TabRes -> Bool
isConsistent (EqTrue _)   (EqTrue _)   = True
isConsistent (EqFalse _)   (EqFalse _) = True
isConsistent (EqTrue n1)  (EqFalse n2) = n1 /= n2
isConsistent (EqFalse n1) (EqTrue n2)  = n1 /= n2

-- |Judges a list of `TabRes`s is consistent or not
areConsistent :: [TabRes] -> Bool
areConsistent []     = True
areConsistent [_]    = True
areConsistent (x:xs) =
  all (isConsistent x) xs && areConsistent xs

-- |Solve by tabueau
-- |
-- |return meaning)
-- | example) [[EqTrue Q, EqTrue P], [EqFalse P]]
-- | it means "the given fomula is satisfied, if (Q == true && P == true) || (P is false)"
-- | so, if return is [], meaning is inconsisitent or no satisfiable
solve :: Formula -> [[TabRes]]
solve (Var name)             = [[EqTrue name]]
solve (Not (Var name))       = [[EqFalse name]]
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

  print $ isConsistent (EqTrue p) (EqTrue p)
  print $ isConsistent (EqTrue p) (EqFalse p)
  print $ isConsistent (EqTrue p) (EqFalse q)

  print $ areConsistent [EqTrue p, EqFalse q]
  print $ areConsistent [EqTrue p, EqFalse q, EqFalse p]



main :: IO ()
main = do
  test1
