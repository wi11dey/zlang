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


desugar :: SExpression -> SExpression
desugar (Boolean True) = Symbol "true"
desugar (Boolean False) = Symbol "false"
desugar (Character c) = Pair (Symbol "character") $ desugar $ Integer $ toEnum c
desugar (Integer i) = Symbol $ show i
desugar (Pair (Symbol "quote" (Pair (Symbol "_") Empty))) = Symbol "_"


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

toBinding :: SExpression -> Either SyntaxError Binding
toBinding (Symbol "_") =
  return
  $ Any
  $ Local
  $ Nothing
toBinding (Pair (Symbol "quote") (Pair (Symbol local) Empty)) =
  return
  $ Any
  $ Local
  $ Just local
toBinding (Pair (Symbol "quote") invalid) = Left $ SyntaxError "Expected symbol, got: " ++ show invalid
toBinding (Symbol exactly) = return $ Exactly exactly
toBinding invalid = Left $ SyntaxError "Invalid binding: " ++ show invalid

toPattern :: SExpression -> Either SyntaxError Pattern
toPattern (Pair car var@(Pair (Symbol "quote") _)) = do
  binding <- toBinding car
  Any (Local name) <- toBinding var
  return $ Destructuring binding name
toPattern patt =
  case binding of
    Left _ -> Left $ SyntaxError "Invalid pattern: " ++ show patt
    _ -> binding
  where binding = toBinding patt >>= return . Binding

toDefinition :: SExpression -> Either SyntaxError Definition
toDefinition (Pair (Symbol "define") (Pair key (Pair definition Empty))) = do
  binding <- toBinding key
  value <- toValue definition
  return $ Definition binding definition

toValue :: SExpression -> Either SyntaxError Value
toValue (Symbol s) = return $ Atom s
toValue f@(Pair (Symbol "function") (Pair patt body)) = do
  forms <- toList body
  case reverse forms of
    [] -> Left $ SyntaxError "Empty function: " ++ show f
    last:init -> do
      define $ map toDefinition forms
      return last
toValue invalid =
  Left $ SyntaxError "Invalid syntax: " ++ show invalid
