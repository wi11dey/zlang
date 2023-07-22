module Zlang.Monad where
import Zlang.SExpression

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
                | Vector [SExpression]
                | Integer Integer
                | Rational Integer Integer
                | Real Double
                | Complex (Complex Double)
                | String String
                | Empty
                deriving Eq

instance Show SExpression where
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

instance Read SExpression where
  read = read


data Value = Atom String
           | Application Value Value
           | Function Pattern Closure

newtype Local = Local (Maybe String)

instance Eq Local where
  Local Nothing == _ = False
  _ == Local Nothing = False
  Local (Just a) == Local (Just b) = a == b

data Binding = Exactly String
             | Any Local

data Pattern = Binding Binding
             | Destructuring Binding Local

type Closure = Environment Value

data Definition = Definition Binding Value

define :: [Definition] -> Closure ()

newtype SyntaxError = SyntaxError String

toList :: SExpression -> Either SyntaxError [SExpression]
toList Empty = return []
toList pair@(Pair car cdr) =
  case toList cdr of
    Right tail -> car:tail
    Left _ -> Left $ SyntaxError "Not a proper list: " ++ show pair
toList sexp = Left $ SyntaxError "Expected list, got: " ++ show sexp

toPattern :: SExpression -> Either SyntaxError Pattern
toPattern (Symbol "_") = return $ Binding $ Any $ Local $ Nothing
toPattern (Symbol s  ) = return $ Binding $ Exactly $ s
toPattern (Pair (Symbol "quote") (Pair (Symbol s) ))

desugar :: SExpression -> SExpression
desugar (Boolean True) = Symbol "true"
desugar (Boolean False) = Symbol "false"
desugar (Character c) = Pair (Symbol "character") $ desugar $ Integer $ toEnum c

fromSExpression :: SExpression -> Either SyntaxError Value
fromSExpression (Symbol s) = return $ Atom s
fromSExpression (Pair (Symbol "function") (Pair patt body)) = do
  forms <- toList body
fromSExpression invalid =
  Left $ SyntaxError "Invalid syntax: " ++ show invalid

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
