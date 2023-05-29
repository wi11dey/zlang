module Zlang.Enviroment where
import Prelude hiding (lookup)

data Environment = Environment
  { locals    :: [(String, Closure)]
  , globals   :: [(String, Closure)]
  , wildcards :: [Closure]
  , parent    :: Maybe Environment
  }

void = Environment
  { locals    = []
  , globals   = []
  , wildcards = []
  , parent    = Nothing
  }

data Closure = Closure
  { environment :: Environment
  , expression  :: Expression
  }

lookupAll :: Environment -> String -> [Closure]

data Closure s a = Closure
  { locals    :: [(String, Closure)]
  , globals   :: [(String, Closure)]
  , wildcards :: [Closure]
  , parent    :: Maybe Environment
  , value     :: a
  }

instance Monad Closure s where
  return :: a -> Closure s a
  return value = Closure
    { locals    = []
    , globals   = []
    , wildcards = []
    , parent    = Nothing
    , value     = value
    }
  (>>=) :: Closure s a -> (a -> Closure s b) -> Closure s b

local :: String -> Closure Void

define :: String -> Value -> Closure Void

data Environment a = Environment { }
                   | Failure

instance Monad Environment where
  fail _ = Failure

local    :: String -> Value -> Environment ()
define   :: String -> Value -> Environment ()
wildcard :: String -> Value -> Environment ()

type Closure = Environment Value

data NonstandardNatural = Standard Natural
                        | Infinity
                        deriving (Eq, Ord)

instance Show NonstandardNatural where
  show (Standard n) = show n
  show Infinity = "∞"

data Literal = Natural NonstandardNatural
             | Empty
             deriving Eq

instance Show Literal where
  show (Natural n) = show n
  show Empty = "()"

data Identifier = Local  String
                | Global String
                deriving Eq

instance Show Identifier where
  show (Local  name) = "'" ++ name
  show (Global name) = name

data Pattern = Literal Literal
             | Identifier Identifier
             | Typed Identifier (Maybe String)
             deriving Eq

instance Show Pattern where
  show (Literal l) = show l
  show (Identifier i) = show i
  show (Typed typ Nothing)    = show "(" ++ show i ++ " " ++ "_" ++ ")"
  show (Typed typ Just value) = show "(" ++ show i ++ " " ++ "''" ++ value ++ ")"

data Function = Function (Maybe Pattern) Closure

data Value = Constant Literal
           | Variable String
           | Function Function
           | Call Closure Closure

instance Show Value where
  show (Constant c) = show c
  show (Variable name) = name
  show (Function Nothing) = "(function _ ...)"
  show (Function Just patt) = "(function " ++ show patt ++ " ...)"

apply :: Function -> Value -> Closure
apply (Function _ body) argument = do
  local _
  body
apply (Function Nothing body) _ = body
apply _ _ = Failure

lookup :: Environment () -> String -> [Closure]

force :: Closure -> EnvironmentT [Value]

type Closure = EnvironmentT Identity Value
