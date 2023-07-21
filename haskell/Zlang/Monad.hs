module Zlang.Monad where
import Zlang.Expression

-- Syntactic sugar for literals is desugared here:
-- - Character literals: #\A is syntactic sugar for (character 65)
-- - String literals: "AA" is syntactic sugar for (string (character 65) (character 65))
-- - Rational literals: 1/2 is syntactic sugar for (rational 1 2)
-- - Floating-point literals: 0.5 is syntactic sugar for (rational 5 10)
-- - Complex literals 1/2+0.5i is syntactic sugar for (complex ((real part) (rational 1 2)) ((imaginary part) (rational 5 10)))

data Expression = Symbol String
                | Pair Expression Expression
                | Boolean Bool
                | Character Char
                | Vector [Expression]
                | Integer Integer
                | Rational Integer Integer
                | Real Double
                | Complex (Complex Double)
                | String String
                | Empty
                deriving Eq

instance Show Expression where
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

instance Read Expression where
  read = read


(Value -> Closure)
data Value = Atom String
           | Application Value Value
           | Function (Maybe Pattern) Closure

newtype Local = Local (Maybe String)

instance Eq Local where
  Local Nothing == _ = False
  _ == Local Nothing = False
  Local (Just a) == Local (Just b) = a == b

data Binding = Exactly String
             | Anything Local

data Pattern = Binding Binding
             | Destructuring Binding Local

type Closure = Environment Value

data Definition = Definition Binding Value

define :: [Definition] -> Closure ()

newtype SyntaxError = SyntaxError String

parse :: Expression -> Value

dispatch :: Value -> Value -> Closure

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
