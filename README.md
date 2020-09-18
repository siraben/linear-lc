# A Typechecker for Linear Lambda Calculus
In linear lambda calculus, every bound variable must be used exactly
once.  This, the following are well-typed in LLC.

```haskell
ex1 = \x y -> x + y
ex2 = \x -> x + 1
```

But the following are not.
```haskell
ex3 = \x -> 1     -- doesn't use x
ex4 = \x -> x + x -- uses x twice
```

There'll be three planned implementations:
- Minimal LLC
- LLC with non-linear types
- LLC with polymorphism à la Hindley-Milner
