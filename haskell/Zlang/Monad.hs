module Zlang.Monad where
import Zlang.Expression

-- Syntactic sugar for literals is desugared here:
-- - Character literals: #\A is syntactic sugar for (character 65)
-- - String literals: "AA" is syntactic sugar for (string (character 65) (character 65))
-- - Rational literals: 1/2 is syntactic sugar for (rational 1 2)
-- - Floating-point literals: 0.5 is syntactic sugar for (rational 5 10)
-- - Complex literals 1/2+0.5i is syntactic sugar for (complex ((real part) (rational 1 2)) ((imaginary part) (rational 5 10)))

data SExpression = Symbol String
                 | Pair SExpression SExpression
                 | Boolean Bool
                 | Character Char
                 | Vector [LispExpression]
                 | Integer Integer
                 | Rational Integer Integer
                 | Real Double
                 | Complex (Complex Double)
                 | String String
                 | Empty
                 deriving Eq

instance Show Lisp where
  show (Symbol s) = show s
  showPrec (Pair car cdr) = show
  show (Boolean True ) = "#t"
  show (Boolean False) = "#f"
  show (Character c) = "#\\" ++ c
  show (Vector v) = do
    "#("
    element <- vec
    show element
    ")"
  show (Integer i) = show i
  show (Complex c) = show c
  show (Real    r) = show r
  show (Rational numerator denominator) = show numerator ++ "/" ++ show denominator
  show (String str) = "\"" ++ str ++ "\""
  show Empty = "()"



type Value = Atom String
           | Application Value Value
           | Function (Maybe Pattern) (Value -> Closure)

type Closure = Environment Value

data Binding = Match String
             | Any   String

data Pattern = Bind (Maybe Binding)
             | DestructuringBind (Maybe Binding) (Maybe String)

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
