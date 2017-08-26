module TableauSolverSpec where

import Test.Hspec

import qualified Data.Set as Set
import Data.Set (Set)
import TableauSolver

main :: IO ()
main = hspec spec

-- Convert a list to a set [[1, 2, 3], [2, 3, 3]] => {{1, 2, 3}, {2, 3}}
toNestSet :: Ord a => [[a]] -> Set(Set a)
toNestSet []     = Set.empty
toNestSet (x:xs) = Set.insert (Set.fromList x) (toNestSet xs)

spec :: Spec
spec = do

  describe "toNestSet, which is only for testing" $ do
    it "is {{12}} where the given list is [[12]]" $
      toNestSet [[12]] `shouldBe` Set.fromList[Set.fromList[12]]

    it "is {{1, 2, 3}, {1, 3, 5}} where the given list is [[1, 1, 2, 3, 2], [1, 3, 5, 3]]" $
      toNestSet [[1, 1, 2, 3, 2], [1, 3, 5, 3]] `shouldBe` Set.fromList[Set.fromList[1, 2, 3], Set.fromList[1, 3, 5]]


  describe "isConsistent" $ do
    let p = VarName "P"
        q = VarName "Q"
        r = VarName "R"
    it "returns True where p==t and p==t" $
      isConsistent (TabRes True p) (TabRes True p) `shouldBe` True

    it "returns False where p==t and p==f because they are not consistent" $
      isConsistent (TabRes True p) (TabRes False p) `shouldBe` False

    it "returns True where p==t and q==t" $
      isConsistent (TabRes True p) (TabRes False q) `shouldBe` True


  describe "areConsistent" $ do
      let p = VarName "P"
          q = VarName "Q"
          r = VarName "R"
      it "returns True where p==t and q==f" $
        areConsistent (Set.fromList [TabRes True p, TabRes False q]) `shouldBe` True

      it "returns False where p==t, q==f and p==f" $
        areConsistent (Set.fromList [TabRes True p, TabRes False q, TabRes False p]) `shouldBe` False


  describe "satisfy" $ do
    let x1 = Var $ VarName "X1"
        x2 = Var $ VarName "X2"
        x3 = Var $ VarName "X3"

    it "returns True where x1 v x2 is given" $
      satisfy (x1 `Or` x2) `shouldBe` True

    it "returns False where x1 ^ !x1 is given" $
      satisfy (x1 `And` Not x1) `shouldBe` False

    it "returns True where ((x1 v !x2) v x3) ^ (!x1 v x2)" $
      satisfy (((x1 `Or` Not x2) `Or` x3) `And` (Not x1 `Or` x2)) `shouldBe` True


  describe "solve" $ do
    let p = VarName "P"
        q = VarName "Q"
        r = VarName "R"
    it "returns [[p=f]] when !P is given" $
      solve (Not (Var p)) `shouldBe` toNestSet [[TabRes False p]]

    it "returns [[p=t, q=t]] when P ^ Q is given" $
      solve (And (Var p) (Var q)) `shouldBe` toNestSet[[TabRes True p, TabRes True q]]

    it "returns [[p=t], [q=t]] when P v Q is given" $
      solve (Or (Var p) (Var q)) `shouldBe` toNestSet[[TabRes True p], [TabRes True q]]

    it "returns [[p=f], [q=t]] when !(P ^ !Q) is given" $
      solve (Not (And (Var p) (Not (Var q)))) `shouldBe` toNestSet[[TabRes False p], [TabRes True q]]

    it "returns [] when !(P ^ Q) ^ ((P v Q) ^ !Q) is given" $
      solve (And (Not (And (Var p) (Not (Var q)))) (And (Or (Var p) (Var q)) (Not (Var q)))) `shouldBe` toNestSet[]