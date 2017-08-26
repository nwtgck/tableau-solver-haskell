module Main where

import           Data.List
import qualified Data.Set as Set
import           Data.Set (Set)
import           TableauSolver

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

  print $ areConsistent $ Set.fromList [TabRes True p, TabRes False q]
  print $ areConsistent $ Set.fromList [TabRes True p, TabRes False q, TabRes False p]


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
  mapM_ print (solve problem)


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
  mapM_ print (solve problem)



main :: IO ()
main = do
  test1
  test2
  aPlusBisA
