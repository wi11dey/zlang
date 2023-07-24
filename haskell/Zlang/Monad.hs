module Zlang.Monad where
import Data.Void
import Data.ByteString
import Data.Map (Map)
import qualified Data.Map as Map

data SExpression = Symbol String
                | Pair SExpression SExpression
                | Boolean Bool
                | Character Char
                | Vector [SExpression]
                | ByteVector ByteString
                | Integer Integer
                | Rational Integer Integer
                | Real Double
                | Complex (Complex Double)
                | String String
                | Empty
                deriving Eq

-- R7RS syntax
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
  show (ByteVector b) = "#u8"
  show (Integer i) = show i
  show (Complex c) = show c
  show (Real    r) = show r
  show (Rational numerator denominator) = show numerator ++ "/" ++ show denominator
  show (String str) = "\"" ++ str ++ "\""
  show Empty = "()"

instance Read SExpression where
  readsPrec _ str = maybeToList $ parse str where
    parse :: String -> Maybe (SExpression, String)
    parse '\'':rest = quotation "quote" rest
    parse '`':rest = quotation "quasiquote" rest
    parse ',':rest = quotation "unquote" rest
    parse ',':'@':rest = quotation "unquote-splicing" rest
    parse '(':list = do
      (car, cdr) <- parse list
      return (Pair car , cdr)
    parse '(':')':rest = return (Empty, rest)

    -- Booleans:
    parse '#':rest = msum $ map (`replaceWord` rest)
      [ ("true",  Boolean True)
      , ("t",     Boolean True)
      , ("false", Boolean False)
      , ("f",     Boolean False)
      ]

    parse '#':'\\':rest = character rest <|> msum $ map (`replaceWord` rest)
      [ ("alarm",     Character '\BEL')
      , ("backspace", Character '\BS')
      , ("delete",    Character '\DEL')
      , ("escape",    Character '\ESC')
      , ("newline",   Character '\LF')
      , ("null",      Character '\NUL')
      , ("return",    Character '\CR')
      , ("tab",       Character '\HT')
      ] where
      character :: String -> Maybe (SExpression, String)
      character c:rest = do
        wordBoundary rest
        return (Character c, rest)

    parse '#':'\\':'x':hex = do
      (ord, rest) <- listToMaybe $ readHex hex
      wordBoundary rest
      return (Character $ fromEnum ord, rest)
    parse '#':'\\':'X':hex = parse "#\\x" ++ hex

    parse space:rest
      | isSpace space = parse rest
      | otherwise = Nothing
    parse [] = Nothing

    quotation :: String -> String -> Maybe (SExpression, String)
    quotation symbol rest = do
      (quoted, remainder) <- parse rest
      return (Pair (Symbol symbol) (Pair quoted Empty), remainder)

    replaceWord :: (String, a) -> String -> Maybe (a, String)
    replaceWord (prefix, replacement) str = do
      -- Case insensitive:
      stripped <- stripPrefix
        (map toUpper prefix)
        (map toUpper str)
      wordBoundary stripped
      return (replacement, stripped)

    wordBoundary :: String -> Maybe ()
    wordBoundary space:rest
      | isSpace space = return ()
      | otherwise = Nothing


-- Syntactic sugar for literals is desugared here:
-- - Character literals: #\A is syntactic sugar for (character 65)
-- - String literals: "AA" is syntactic sugar for (string (character 65) (character 65))
-- - Rational literals: 1/2 is syntactic sugar for (rational 1 2)
-- - Floating-point literals: 0.5 is syntactic sugar for (rational 5 10)
-- - Complex literals 1/2+0.5i is syntactic sugar for (complex ((real part) (rational 1 2)) ((imaginary part) (rational 5 10)))

desugar :: SExpression -> SExpression
desugar (Boolean True) = Symbol "true"
desugar (Boolean False) = Symbol "false"
desugar (Character c) = Pair (Symbol "character") $ desugar $ Integer $ toEnum c
desugar (Integer i) = Symbol $ show i
desugar (Pair (Symbol "quote" (Pair (Symbol "_") Empty))) = Symbol "_"


data Environment a = Environment [a] [Map String [Value]]

data Definition = Definition Binding Value

define :: [Definition] -> Environment Void
define definitions = Environment []
  $ fromListWith (++)
  $ map (\(Definition binding value) -> (binding, [value]))
  $ definitions

instance Monad Environment where
  return value = Environment [value] []
  Environment _ outer >> Environment value inner = Environment value inner ++ outer

type Closure = Environment Value

apply :: Value -> Value -> Closure


data Value = Atom String
           | Application Value Value
           | Function Pattern Closure

newtype Local = Local (Maybe String)

data Binding = Exactly String
             | Any Local

data Pattern = Binding Binding
             | Destructuring Binding Local

newtype SyntaxError = SyntaxError String

syntaxError = Left . SyntaxError

instance MonadFail (Either SyntaxError) where
  fail = syntaxError

toList :: SExpression -> Either SyntaxError [SExpression]
toList Empty = return []
toList pair@(Pair car cdr) =
  case toList cdr of
    Right tail -> car:tail
    Left _ -> syntaxError "Not a proper list: " ++ show pair
toList sexp = syntaxError "Expected list, got: " ++ show sexp

toBinding :: SExpression -> Either SyntaxError Binding
toBinding (Symbol exactly) = return $ Exactly exactly
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
toBinding (Pair (Symbol "quote") invalid) =
  syntaxError "Expected symbol, got: " ++ show invalid
toBinding invalid =
  syntaxError "Invalid binding: " ++ show invalid

toPattern :: SExpression -> Either SyntaxError Pattern
toPattern (Pair car var@(Pair (Symbol "quote") _)) = do
  binding <- toBinding car
  Any (Local name) <- toBinding var
  return $ Destructuring binding name
toPattern patt =
  toBinding patt >>= return . Binding

toDefinition :: SExpression -> Either SyntaxError Definition
toDefinition (Pair (Symbol "define") (Pair key (Pair definition Empty))) = do
  binding <- toBinding key
  value <- toValue definition
  return $ Definition binding definition
toDefinition sexp@(Pair (Symbol "define") _) =
  syntaxError "Invalid definition: " ++ show sexp
toDefinition invalid =
  syntaxError "Expected definition, got: " ++ show sexp

toValue :: SExpression -> Either SyntaxError Value
toValue (Symbol s) = return $ Atom s
toValue f@(Pair (Symbol "function") (Pair patt body@(Pair _ _))) = do
  matcher <- toPattern patt
  forms <- toList body
  definitions <- mapM toDefinition $ init forms
  return $ Function matcher $ do
    define definitions
    return $ last forms
toValue sexp@(Pair (Symbol "function") _) =
  syntaxError "Invalid function: " ++ show sexp
toValue invalid =
  syntaxError "Invalid syntax: " ++ show invalid
