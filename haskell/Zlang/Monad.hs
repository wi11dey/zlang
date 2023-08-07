module Zlang.Monad where
import Prelude hiding (lookup)
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
                | Rational Rational
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
  show (Integer  i) = show i
  show (Complex  c) = show c
  show (Real     r) = show r
  show (Rational r) = do
    show (numerator r)
    "/"
    show (denominator r)
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


data Value v = Open v
             | Closed (Store v v)

data Scope v = Scope { definitions :: Map String [Value v]
                     , fallbacks   :: Map String [Value v]
                     }

data Store v a = Store [a] [Scope v]
               | Lookup String
               | Fail

-- force one file down at lookup

instance Monad (Store v) where
  return value = Store [value] []
  Store [] [] >> s = s
  s >> Store [] [] = s
  Store [] hd1:tl1 >>= Store [] hd2:tl2 = Store [] (
    Scope { definitions = Map.merge (definitions hd1) (definitions hd2)
          , fallbacks   = Map.merge (fallbacks   hd1) (fallbacks   hd2)
          }
    ):(tl2 ++ tl1)
  Store [] outer >>= Store value inner = Store value inner ++ outer

instance MonadFail (Store v) where
  fail _ = Fail

instance MonadPlus (Store v) where
  mzero = Store [] []
  mplus = (>>)

define   :: String ->       v -> Store v Void
define'  :: String ->       v -> Store v Void -- wildcard
argument :: String -> Store v -> Store v Void

define key value = Store [] [
  Scope { definitions = Map.singleton key (Open value)
        , fallbacks   = Map.empty
        }
  ]
define' key value = Store [] [
  Scope { definitions = Map.empty
        , fallbacks   = Map.singleton key (Open value)
        }
  ]
argument key cl = Store [] [
  Scope { definitions = Map.singleton key (Closed cl)
        , fallbacks   = Map.empty
        }
  ]

lookup :: String -> Store v v
lookup = Lookup


-- Syntactic sugar for literals is desugared here:
-- - Character literals: #\A is syntactic sugar for (character 65)
-- - String literals: "AA" is syntactic sugar for (string (character 65) (character 65))
-- - Rational literals: 1/2 is syntactic sugar for (rational 1 2)
-- - Floating-point literals: 0.5 is syntactic sugar for (rational 5 10)
-- - Complex literals 1/2+0.5i is syntactic sugar for (complex ((real part) (rational 1 2)) ((imaginary part) (rational 5 10)))

-- Idempotent

desugar :: SExpression -> SExpression
desugar (Boolean True) = Symbol "true"
desugar (Boolean False) = Symbol "false"
desugar (Character c) =
  Pair (Symbol "character")
  $ flip Pair
  $ Empty
  $ desugar
  $ Integer
  $ toEnum c
desugar (Real r) = desugar $ Rational $ toRational r
desugar (Rational r) =
  Pair (Symbol "ratio")
  $ Pair (desugar $ Integer $ numerator   r)
  $ Pair (desugar $ Integer $ denominator r)
  Empty
desugar (String s) =
  Pair (Symbol "string")
  $ foldl'
  $ flip (Pair . desugar . Character)
  $ Empty
  $ reverse s
desugar (Integer i) = Symbol $ show i
desugar (Pair (Symbol "quote" (Pair (Symbol "_") Empty))) = Symbol "_"
desugar (Pair (Symbol "define")
         (Pair quotation@(Pair (Symbol "quote")
                          _)
          body)) =
  (Pair (Symbol "define")
   (Pair quotation (desugar body)))
desugar (Pair (Symbol "define") (Pair quotation))
desugar (Pair car cdr) = Pair (desugar car) (desugar cdr)


type Store a = Store Object a
type Closure = Environment Object

data Object = Atom String
            | Application Object Object
            | Function (Closure -> Closure)

newtype SyntaxError = SyntaxError String

syntaxError = Left . SyntaxError

instance MonadFail (Either SyntaxError) where
  fail = syntaxError

toList :: SExpression -> Either SyntaxError [SExpression]
toList Empty = return []
toList (Pair car cdr) =
  return (car:) `ap` toList cdr
toList invalid = syntaxError "Not a proper list: " ++ show invalid

definition :: SExpression -> Either SyntaxError (Environment Void)
definition sexp@(Pair (Symbol "define") definition) =
  case definition of
    (Pair binding
      (Pair sexp
        Empty)) ->
      bind binding `ap` (object sexp)
    _ -> syntaxError "Invalid definition: " ++ show sexp
  where
    bind :: SExpression -> Either SyntaxError (Object -> Environment Void)
    bind (Symbol "_") = return $ define' mzero
    bind (Symbol key) = return $ define key
    bind (Pair (Symbol "quote")
           (Pair (Symbol name)
             Empty)) =
      return $ define' name
    bind invalid = syntaxError "Invalid binding: " + show invalid
definition invalid =
  syntaxError "Expected definition, got: " ++ show invalid

arguments :: SExpression -> Either SyntaxError (Closure -> Environment Void)
arguments patt =
  case patt of
    (Symbol "_") ->
      return $ const mzero
    (Symbol s) ->
      return $ exact s
    (Pair (Symbol "quote")
      (Pair (Symbol name)
        Empty)) ->
      return $ argument name
    (Pair (Symbol typ)
      (Pair (Symbol "_")
        Empty)) ->
      return \cl -> do
        f <- car cl
        exact typ f
    (Pair (Symbol typ)
      (Pair (Pair (Symbol "quote")
              (Pair (Symbol name)
                Empty))
        Empty)) ->
      return \cl -> do
        f <- car cl
        exact typ f
        argument name cl
    (Pair (Pair (Symbol "quote")
            (Pair (Symbol typ)
              Empty))
      (Pair (Pair (Symbol "quote")
              (Pair (Symbol name)
                Empty))
        Empty)) ->
      return \cl -> do
        f <- car cl
        argument typ f
        argument name cl
    invalid -> syntaxError "Invalid pattern: " ++ show invalid
  where
    car :: Closure -> Environment Closure
    car cl = do
      Application f _ <- cl
      return cl >> f

    exact :: String -> Closure -> Environment Void
    exact s cl = do
      Atom a <- cl
      guard s == a
      argument a cl

object :: SExpression -> Either SyntaxError Object
object (Symbol s) = return $ Atom s
object (Pair f (Pair arg Empty)) = return $ Application f arg
object sexp@(Pair (Symbol "function") f) =
  case desugar f of
    (Pair patt
     body@(Pair _ _)) -> do
      set <- arguments patt
      forms <- toList body
      definitions <- mapM definition $ init forms
      val <- object $ last forms
      return $ Function \arg -> do
        set arg
        sequence_ definitions
        return val
    _ -> syntaxError "Invalid function: " ++ show sexp
object invalid =
  syntaxError "Invalid syntax: " ++ show invalid
