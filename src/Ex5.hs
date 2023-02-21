-- https://www.cis.upenn.edu/~cis1940/spring13/hw/05-type-classes.pdf

module Ex5
    ( ExprT (Lit, Add, Mul),
      eval
    ) where


data ExprT  = Lit Integer
            | Add ExprT ExprT
            | Mul ExprT ExprT
  deriving (Show, Eq)


-- Exercise 1
-- Write Version 1 of the calculator: an evaluator for ExprT, with the signature
--   eval :: ExprT -> Integer

-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add (Lit n) (Lit m)) = n + m
eval (Mul (Lit n) (Lit m)) = n * m
