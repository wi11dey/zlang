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
desugar (Pair car cdr) = Pair (desugar car) (desugar cdr)


data Environment a = Environment [a] [Map String [Value]]
                   | Fail

instance Monad Environment where
  return value = Environment [value] [] -- TODO should just force here ?
  Environment [] outer >> Environment value inner = Environment value inner ++ outer

instance MonadFail Environment where
  fail _ = Fail

define    :: String -> Value   -> Environment Void
define_   :: String -> Value   -> Environment Void
argument  :: String -> Closure -> Environment Void
argument_ :: String -> Closure -> Environment Void

instance MonadPlus Environment where
  mzero = Environment [] []
  mplus = (>>=)

data Definition = Definition Binding Value

lookupEnvironment :: String -> Environment a
lookupEnvironment = Lookup

define :: [Definition] -> Environment Void
define definitions = Environment []
  $ fromListWith (++)
  $ map (\(Definition binding value) -> (binding, [value]))
  $ definitions

defineLocal :: Local -> Value -> Environment Void
defineLocal (Local Nothing) _ = mzero
-- FIXME unhygenic local s leak here: on lookup, local s will be defined to caller even though was not in scope lexically
defineLocal (Local s) value = define [Definition (Exactly s) value]

type Closure = Environment Value

apply :: Value -> Value -> Closure
apply (Atom f) argument = do
  function <- lookupEnvironment f
  apply function argument
apply (Function (Binding (Exactly a)) body) (Atom b)
  | a == b = do
      defineLocal a b -- FIXME need to retain closure of b
      body
  | otherwize = mzero
apply (Function (Binding (Any local))) argument = do
  defineLocal local argument
  body
apply (Function (Destructuring (Exactly a) local) body) argument@(Application b _)
  | a == b = do
      defineLocal a b -- FIXME need to retain closure of b
      defineLocal local argument
      body
  | otherwise = mzero


data Value = Atom String
           | Application Value Value
           | Function Closure -> Closure

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

definition :: SExpression -> Either SyntaxError (Environment Void)
definition (Pair (Symbol "define")
            (Pair binding
             (Pair sexp
              Empty))) =
  bind binding `ap` (value sexp) where
    bind :: SExpression -> Either SyntaxError (Value -> Environment Void)
    bind (Symbol "_") = return $ define_ empty
    bind (Symbol key) = return $ define key
    bind (Pair (Symbol "quote")
           (Pair (Symbol name)
             Empty)) =
      return $ define_ name
    bind invalid = syntaxError "Invalid binding: " + show invalid
definition invalid@(Pair (Symbol "define") _) =
  syntaxError "Invalid definition: " ++ show invalid
definition invalid =
  syntaxError "Expected definition, got: " ++ show invalid

arguments :: SExpression -> Either SyntaxError (Closure -> Environment Void)
arguments (Symbol s) =
  return \cl -> do
    Atom a <- cl
    guard s == a
    argument a cl
arguments (Symbol "_") = return \_ -> empty
arguments (Pair (Symbol "quote")
            (Pair (Symbol name)
              Empty)) =
  return \cl -> argument_ name cl
arguments (Pair (Symbol typ)
            (Pair (Symbol "_")
             Empty)) =
  return \cl -> do
    Application car _ <- cl
    let f = cl >> car
    Atom a <- f
    guard typ == a
    argument typ f
arguments (Pair (Symbol typ)
            (Pair (Pair (Symbol "quote")
                   (Pair (Symbol name)
                    Empty))
             Empty)) =
  return \cl -> do
    Application car _ <- cl
    let f = cl >> car
    Atom a <- f
    guard typ == a
    argument typ f
    argument_ name cl
arguments (Pair (Pair (Symbol "quote")
                 (Pair (Symbol typ)
                  Empty))
           (Pair (Pair (Symbol "quote")
                  (Pair (Symbol name)
                   Empty))
             Empty)) =
  return \cl -> do
    Application car _ <- cl
    let f = cl >> car
    argument_ typ f
    argument_ name cl

value :: SExpression -> Either SyntaxError Value
value (Symbol s) = return $ Atom s
value (Pair f (Pair arg Empty)) = return $ Application f arg
value f@(Pair (Symbol "function")
           (Pair patt
            body@(Pair _ _))) = do
  set <- arguments patt
  forms <- toList body
  definitions <- mapM definition $ init forms
  val <- value $ last forms
  return $ Function \arg -> do
    set arg
    sequence_ definitions
    return $ val
value sexp@(Pair (Symbol "function") _) =
  syntaxError "Invalid function: " ++ show sexp
value invalid =
  syntaxError "Invalid syntax: " ++ show invalid
