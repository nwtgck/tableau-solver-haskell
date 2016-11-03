-- A try to solve well-formed formula by tablueau

-- |Variable name
-- |example) P, Q, R
data VarName = VarName String deriving (Show, Eq)

-- |Well-formed formula
data Formula  = Var VarName |
                Not Formula |
                And Formula Formula |
                Or  Formula Formula
                deriving (Show, Eq)

-- |Tabueau result
-- |example) `EqTrue (VarName "P")` represens "if P is true, a formula satisfies."
data TabRes = EqTrue VarName | EqFalse VarName deriving (Show, Eq)

-- |Solve by tabueau
-- |
-- |return meaning)
-- | example) [[EqTrue Q, EqTrue P], [EqFalse P]]
-- | it means "the given fomula is satisfied, if (Q == true && P == true) || (P is false)"
solve :: Formula -> [[TabRes]]
solve (Var name)             = [[EqTrue name]]
solve (Not (Var name))       = [[EqFalse name]]
solve (Not (Not e))          = solve e                         -- Double Negation
solve (Not (And e1 e2))      = solve $ Or  (Not e1) (Not e2)   -- De Morgan's laws
solve (Not (Or  e1 e2))      = solve $ And (Not e1) (Not e2)   -- De Morgan's laws
solve (And e1 e2)            = (++) <$> solve e1 <*> solve e2
solve (Or  e1 e2)            = solve e1 ++ solve e2

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
  -- !(P ^ Q) ^ ((P v Q) ^ !Q)
  print $ solve $ And (Not (And (Var p) (Var q))) (And (Or (Var p) (Var q)) (Not (Var q)))

main :: IO ()
main = do
  test1
