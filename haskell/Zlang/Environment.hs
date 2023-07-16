module Zlang.Closure where
import Data.Functor.Identity

force :: Closure Value -> ClosureT [Value]

force :: Closure Value -> [Closure Value]
force (Closure environment (Variable name)) = lookup environment name
force (Closure environment (Call f argument)) =
  [Call (force f) argument, Call f (force argument)]
force Call f argument = do
  f' <- force f
  argument' <- force argument
  return $ Call f' argument
  return $ Call f  argument'

type Function = Value -> Closure Value

local :: String -> Value -> Closure ()

function :: List -> Value -> Closure Value
function (Quoted name):body argument = do
  local name argument
  sequence $ map define $ init body
  return $ value $ last body

force :: Value -> ClosureT [Value]
force (Variable name) = lookup name
force (Call f argument) =
  do
    argument' <- force argument
    return $ f argument'
  <|> do
    (Function f') <- force f
    return $ f' argument
force _ = fail ""

data ClosureT m a = Computation
                    { evaluate :: m (Closure a)
                    }
                  | Lookup
                    { definitions :: [(String, a)]
                    , name :: String
                    }

type Closure = ClosureT Identity

instance (Monad m) => Monad (ClosureT m) where
  return = Computation . return . return
  Lookup definitions name >>= f = Computation $
    lookupAll definitions name

apply :: Function -> Value -> Closure Value

lookup :: String -> ClosureT [a]
lookup = Lookup []

force :: Value -> ClosureT [Value]
force (Variable name) = lookup name
force (Call (Call f1 f2) argument) = do
  apply (forceCallee f)
force value = lift $ return value

data Literal = Natural Natural
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

data Function = Function (Maybe Pattern) (Closure Value)

data Value = Constant Literal
           | Variable String
           | Function Function
           | Call Value Value

instance Show Value where
  show (Constant c) = show c
  show (Variable name) = name
  show (Function Nothing) = "(function _ ...)"
  show (Function Just patt) = "(function " ++ show patt ++ " ...)"

data Closure a = Closure a
  {
    parent :: Maybe Closure ()
  }

data ClosureT

define :: Scope -> String -> Value -> Environment ()
define scope name value = Environment (name, scope, value) ()

instance Monad Environment where
  return = Environment []
  (Environment definitions value) >>= f =
    case f value of
      
      Failure -> Failure
  Failure >>= _ = Failure
  fail _ = Failure

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

data Literal = Natural Natural
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
