# Notes:


## Usefull Haskell  Extensions

### Lambda Case
Allows for lambda expressions to implicitly pattern match on imput:
```
{-# LANGUAGE LambdaCase #-}

data Exp a
  = Lam a (Exp a)
  | Var a
  | App (Exp a) (Exp a)

example :: Exp a -> a
example = \case
  Lam a b -> a
  Var a   -> a
  App a b -> example a

-- vs the traditional (I think ...)

example :: Exp a -> b
example exp = case exp of
  Lam a   -> a
  Var a   -> a
  App a b -> example a
```

### Generalized Newtype Deriving
Allows for compile-time checks of 'aliased' types by letting us reference data types with a single constructor as a new distinct type.

```
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Velocity = Velocity { unVelocity :: Double }
  deriving (Eq, Ord)

v :: Velocity
v = Velocity 2.718

x :: Double
x = 6.382

-- Type error is caught at compile time depite the runtime value being identical
err = v + x
```
