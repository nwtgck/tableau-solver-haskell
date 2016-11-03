-- A try to solve well-formed formula by tablueau

-- |Variable name
-- |example) P, Q, R
data VarName = VarName String deriving (Show, Eq)

-- |Well-formed formula
data Formula  = Var VarName |
                Not Formula Formula |
                And Formula Formula |
                Or  Formula Formula
                deriving (Show, Eq)

-- |Tabueau result
-- |example) `EqTrue (VarName "P")` represens "if P is true, a formula satisfies."
data TabRes = EqTrue VarName | EqFalse VarName deriving (Show, Eq)

main :: IO ()
main = return ()
