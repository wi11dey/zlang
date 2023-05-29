module Zlang.Evaluation where
import Zlang.Environment

apply :: Closure -> Closure -> Maybe Closure
apply (Closure { expression = Constant c }) _ =
  error ("Cannot call " ++ show c ++ ".")
apply (Closure { expression = Function pattern inner body })
      (Closure { expression = argument }) =
  case (pattern, argument) of
    (Identifier Local  name, _) -> _
    (Identifier Global name, Variable v) | v == name -> _
    (Literal e, Constant c) | c == e -> _
    (Typed (Local  name) value, Call f' argument') -> _
    (Typed (Global name) value, Call f' argument') -> _
    _ -> Nothing
apply _ = Nothing

force :: Closure -> [Closure]
force (Closure { environment = env, expression = Variable name }) =
  lookupAll env name
force cl@(Closure { expression = Call f argument }) = concatMap
  (flip mapMaybe
   $ forceAll [cl { expression = f }]) . flip apply
  $ forceAll [cl { expression = argument }]
force _ = []

-- |forceAll takes an initial element queue, and returns the breadth-first traversal of forced closures from the initial elements.
forceAll :: [Closure] -> [Closure]
forceAll [] = []
forceAll first:tail = first:(forceAll $ tail ++ force first)
