import Criterion.Main

import TableauSolver

main = let

  p = Var $ VarName "P"
  q = Var $ VarName "Q"
  r = Var $ VarName "R"

  formula1 :: Formula
  formula1 = Not (p `And` (Not q))

  formula2 :: Formula
  formula2 = Not (p `And` (Not q)) `Or` (Not r `And` p)

  in defaultMain [
       bgroup "solve" [ bench "!(p v !q)"              $ whnf solve formula1
                      , bench "!(p v !q) || (!r && p)" $ whnf solve formula2
                      ]
       ]
