{-# OPTIONS_GHC -O2 #-}

-- A try to solve well-formed formula by tableau

module TableauSolver(
  VarName(..),
  Formula(..),
  impl,
  TabRes(..),
  isConsistent,
  areConsistent,
  solve,
  satisfy
) where


import           Data.List
import qualified Data.Set as Set
import           Data.Set (Set)

-- |Variable name
-- |example) P, Q, R
data VarName = VarName String deriving (Eq, Ord)

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

-- |Tableau result
-- |example) `TabRes True (VarName "P")` represens "if P is true, a formula satisfies."
data TabRes = TabRes Bool VarName  deriving (Eq, Ord)

-- |Show for TabRes
instance Show TabRes where
  show (TabRes True v)  = show v ++ "==" ++ "t"
  show (TabRes False v) = show v ++ "==" ++ "f"


-- |Judges two TabRes are consistent or not
isConsistent :: TabRes -> TabRes -> Bool
isConsistent (TabRes b1 n1) (TabRes b2 n2) = b1 == b2 || n1 /= n2

-- |Judges a list of `TabRes`s is consistent or not
areConsistent :: Set TabRes -> Bool
areConsistent set = listAreConsistent (Set.toList set) -- TODO Don't use toList
  where
    listAreConsistent []     = False
    listAreConsistent [_]    = True
    listAreConsistent (x:xs) =
      all (isConsistent x) xs && listAreConsistent xs

-- |Solve by tableau
-- |
-- |return meaning)
-- | example) [[TabRes True Q, TabRes True P], [TabRes False P]]
-- | it means "the given fomula is satisfied, if (Q == true && P == true) || (P is false)"
-- | so, if return is [], meaning is inconsisitent or no satisfiable
solve :: Formula -> Set(Set TabRes)
solve (Var name)             = Set.singleton(Set.singleton (TabRes True name))
solve (Not (Var name))       = Set.singleton(Set.singleton (TabRes False name))
solve (Not (Not e))          = solve e                         -- Double Negation
solve (Not (And e1 e2))      = solve $ Or  (Not e1) (Not e2)   -- De Morgan's laws
solve (Not (Or  e1 e2))      = solve $ And (Not e1) (Not e2)   -- De Morgan's laws
solve (Or  e1 e2)            = Set.union (solve e1) (solve e2)
solve (And f1 f2)            = let
  s1 = solve f1
  s2 = solve f2
  in setFlatMap (\r1 ->
    setFlatMap(\r2 ->
      let union = Set.union r1 r2
      in if areConsistent union
        then Set.singleton union
        else Set.empty
    ) s2
  ) s1

-- | flatMap for Set
setFlatMap :: Ord a => (a -> Set a) -> Set a -> Set a
setFlatMap f = foldl (\accum e -> Set.union accum (f e)) Set.empty

-- | Solve satisfiability
satisfy :: Formula -> Bool
satisfy formula = not $ Set.null (solve formula)