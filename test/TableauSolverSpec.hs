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

    it "enumerates the possible patterns of 'a+b=c' problem" $ do
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


          -- [TabRes False "a=0", TabRes False "a=1", ..., TabRes False "res=4"]
          allFalse = [TabRes False name | Var name <-[a0, a1, a2, b0, b1, b2, c0, c1, c2, r0, r1, r2, r3, r4]]

          expect :: Set (Set TabRes)
          expect = toNestSet[
             -- a=0, b=0, c=0 (r=0)
            [if Var name `elem` [a0, b0, c0, r0] then TabRes True name else res | res@(TabRes _ name) <- allFalse],
            -- a=1, b=0, c=1  (r=1)
            [if Var name `elem` [a1, b0, c1, r1] then TabRes True name else res | res@(TabRes _ name) <- allFalse],
            -- a=2, b=0, c=2  (r=2)
            [if Var name `elem` [a2, b0, c2, r2] then TabRes True name else res | res@(TabRes _ name) <- allFalse],
            -- a=0, b=1, c=1, (r=1)
            [if Var name `elem` [a0, b1, c1, r1] then TabRes True name else res | res@(TabRes _ name) <- allFalse],
            -- a=0, b=2, c=2 (r=2)
            [if Var name `elem` [a0, b2, c2, r2] then TabRes True name else res | res@(TabRes _ name) <- allFalse],
            -- a=1, b=1, c=2 (r=2)
            [if Var name `elem` [a1, b1, c2, r2] then TabRes True name else res | res@(TabRes _ name) <- allFalse]]

      -- solve a+b=c problem
      solve problem `shouldBe` expect

    it "enumerates the possible patterns of 'a+b=a' problem" $ do
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

          -- [TabRes False "a=0", TabRes False "a=1", ..., TabRes False "res=4"]
          allFalse = [TabRes False name | Var name <-[a0, a1, a2, b0, b1, b2, r0, r1, r2, r3, r4]]

          expect :: Set (Set TabRes)
          expect = toNestSet[
             -- a=0, b=0 (r=0)
            [if Var name `elem` [a0, b0, r0] then TabRes True name else res | res@(TabRes _ name) <- allFalse],
            -- a=1, b=0  (r=1)
            [if Var name `elem` [a1, b0, r1] then TabRes True name else res | res@(TabRes _ name) <- allFalse],
            -- a=2, b=0 (r=2)
            [if Var name `elem` [a2, b0, r2] then TabRes True name else res | res@(TabRes _ name) <- allFalse]]

      -- solve a+b=a problem
      solve problem `shouldBe` expect
