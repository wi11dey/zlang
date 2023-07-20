module Zlang.Monad where
import Zlang.Expression

-- Syntactic sugar for literals is desugared here:
-- - Character literals: #\A is syntactic sugar for (character 65)
-- - String literals: "AA" is syntactic sugar for (string (character 65) (character 65))
-- - Rational literals: 1/2 is syntactic sugar for (rational 1 2)
-- - Floating-point literals: 0.5 is syntactic sugar for (rational 5 10)
-- - Complex literals 1/2+0.5i is syntactic sugar for (complex ((real part) (rational 1 2)) ((imaginary part) (rational 5 10)))

data SExpression sym  = Symbol sym
                         | Value val
                         | Pair SExpression SExpression
                         deriving Eq

instance (Show sym, Show val) => Show (SExpression sym val) where
  show (Symbol s) = show s
  show (Value  v) = show v
  showPrec (Pair car cdr) = a



data LispValue = Boolean Bool
               | Character Char
               | Vector [LispExpression]
               | Integer Integer
               | Rational Integer Integer
               | Real Double
               | Complex (Complex Double)
               | String String
               | Empty
               deriving Eq

instance Show LispValue where
  show (Boolean True ) = "#t"
  show (Boolean False) = "#f"
  show (Character c) = "#\\" ++ c
  show (Vector v) = do
    "#("
    element <- vec
    show element
    ")"

type LispExpression = SExpression String LispValue



type Object = SExpression String Function

type Closure = Environment Value

data Binding = Exact String
             | Any   String

data Pattern = SExpression (Maybe Binding) (Maybe Binding)

data Function = Function (Maybe Pattern) (Value -> Closure)

data Value = Expression (SExpression String Value)
           | Function (Maybe Pattern) (Value -> Closure)

data Pattern = SExpression String

data Value = Atom String
           | Application Value Value
           | Function (Maybe Pattern) (Value -> Closure)

data Pattern = Exactly String
             | Anything String

dispatch :: Value -> Value -> Closure
apply ()

data Value = Symbol String
           | Application Value Value
           | Closure Closure
           | NoMatch

apply :: Value -> Value -> Environment Value
apply (Symbol sym) (Closure cl) = do
  -- Ask closure to look up a function sym with 
  return
apply (Symbol sym) (Closure cl) = do
  return
