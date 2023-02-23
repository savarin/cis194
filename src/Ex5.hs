-- https://www.cis.upenn.edu/~cis1940/spring13/hw/05-type-classes.pdf

module Ex5
    ( ExprT (Lit, Add, Mul),
      eval,
      evalStr,
      Expr (lit, add, mul),
      reify
    ) where

import Parser


-- Exercise 1 Write Version 1 of the calculator: an evaluator for ExprT, with
-- the signature
--   eval :: ExprT -> Integer

-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.

data ExprT  = Lit Integer
            | Add ExprT ExprT
            | Mul ExprT ExprT
  deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add (Lit n) (Lit m)) = n + m
eval (Mul (Lit n) (Lit m)) = n * m


-- Exercise 2 The UI department has internalized the focus group data and is
-- ready to synergize with you. They have developed the front-facing
-- user-interface: a parser that handles the textual representation of the
-- selected language. They have sent you the module Parser.hs, which exports
-- parseExp, a parser for arithmetic expressions. If you pass the constructors
-- of ExprT to it as arguments, it will convert Strings representing arithmetic
-- expressions into values of type ExprT.

-- For example:
--   *Calc> parseExp Lit Add Mul "(2+3)*4"
--   Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
--   *Calc> parseExp Lit Add Mul "2+3*4"
--   Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
--   *Calc> parseExp Lit Add Mul "2+3*"
--   Nothing

-- Leverage the assets of the UI team to implement the value-added function
--   evalStr :: String -> Maybe Integer
-- which evaluates arithmetic expressions given as a String, producing Nothing
-- for inputs which are not well-formed expressions, and Just n for well-formed
-- inputs that evaluate to n.

evalStr :: String -> Maybe Integer
evalStr = (eval <$>) . parseExp Lit Add Mul


-- Exercise 3 Good news! Early customer feedback indicates that people really do
-- love the interface! Unfortunately, there seems to be some disagreement over
-- exactly how the calculator should go about its calculating business. The
-- problem the software department (i.e. you) has is that while ExprT is nice,
-- it is also rather inflexible, which makes catering to diverse demographics a
-- bit clumsy. You decide to abstract away the properties of ExprT with a type
-- class.

-- Create a type class called Expr with three methods called lit, add, and mul
-- which parallel the constructors of ExprT. Make an instance of Expr for the
-- ExprT type, in such a way that
--   mul (add (lit 2) (lit 3)) (lit 4) :: ExprT == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- Think carefully about what types lit, add, and mul should have. It may be
-- helpful to consider the types of the ExprT constructors, which you can find
-- out by typing (for example)
--   *Calc> :t Lit
-- at the ghci prompt.

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

reify :: ExprT -> ExprT
reify x = x

instance Expr ExprT where
    lit x   = Lit x
    add x y = Add x y
    mul x y = Mul x y
