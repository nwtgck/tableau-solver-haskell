# Tableau solver in Haskell [![Build Status](https://travis-ci.org/nwtgck/tableau-solver-haskell.svg?branch=master)](https://travis-ci.org/nwtgck/tableau-solver-haskell)

Tableau method can solve whether a logical formula is satisfy or not (=satisfiability: 充足可能性), contradiction (=矛盾) and tautology.

<img src="https://upload.wikimedia.org/wikipedia/en/a/ad/Partially_built_tableau.svg" />

(from: https://www.wikiwand.com/en/Method_of_analytic_tableaux)

## How to test

```sh
$ stack test
```

### stack command

If you don't have stack command, you can install it by the following instruction.

https://docs.haskellstack.org/en/stable/README/#how-to-install


## Actual Applications

### Simple satisfiability problem

**Q.** `(x1 ∨ ¬x2 ∨ x3) ∧ (¬x1 ∨ x2)` satisfies or not

### Solution

```hs
let x1       = Var $ VarName "X1"
    x2       = Var $ VarName "X2"
    x3       = Var $ VarName "X3"
    formula  = (x1 `Or` Not x2 `Or` x3) `And` (Not x1 `Or` x2) 
    
print(solve formula)
```

### Output

```
fromList [fromList [X1==f,X2==f],fromList [X1==f,X3==t],fromList [X1==t,X2==t],fromList [X2==t,X3==t]]
```

* `t` means `True`.
* `f` means `False`

So this means that `(x1 ∨ ¬x2 ∨ x3) ∧ (¬x1 ∨ x2)` satisfies where
* `x1` == `False`, `x2` == `False` and `x3` == anything or
* `x1` == `False`, `x3` == `True` and `x2` == anything or
* `x1` == `True`, `x2` == `True` and `x2` == anything or
* `x2` == `True`, `x3` == `True` and `x` == anything


### `A + B = A` Problem

**Q.** Find `a` and `b` which satisfy `a + b = a` where `a`, `b` ϵ `{0, 1, 2}`

How to solve this problem by using `tableau-solver`?
  

### Solution:
```hs
let
  a0 = Var $ VarName "a=0"
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
```

### Output (=Answer): 
```
fromList [a=0==f,a=1==f,b=1==f,b=2==f,res=0==f,res=1==f,res=3==f,res=4==f,a=2==t,b=0==t,res=2==t]
fromList [a=0==f,a=2==f,b=1==f,b=2==f,res=0==f,res=2==f,res=3==f,res=4==f,a=1==t,b=0==t,res=1==t]
fromList [a=1==f,a=2==f,b=1==f,b=2==f,res=1==f,res=2==f,res=3==f,res=4==f,a=0==t,b=0==t,res=0==t]
```

* `t` means `True`.
* `f` means `False`

So these 3 lines means that `a + b = a` is satisfied where
* `a=2` and `b=0` or
* `a=1` and `b=0` or
* `a=0` and `b=0`.

So any problem which can be represented in logical-formula is solved by tableau!