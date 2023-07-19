module Zlang.Monad where
import Zlang.Expression

-- Syntactic sugar for literals is desugared here:
-- - Character literals: #\A is syntactic sugar for (character 65)
-- - String literals: "AA" is syntactic sugar for (string (character 65) (character 65))
-- - Rational literals: 1/2 is syntactic sugar for (rational 1 2)
-- - Floating-point literals: 0.5 is syntactic sugar for (rational 5 10)
-- - Complex literals 1/2+0.5i is syntactic sugar for (complex ((real part) (rational 1 2)) ((imaginary part) (rational 5 10)))

type Closure = Environment Value

data Value = Symbol String
           | Application Value Value
           | Closure Closure
           | NoMatch

apply :: Value -> Value -> Environment Value
apply (Symbol sym) (Closure cl) = do
  return
apply (Symbol sym) (Closure cl) = do
  return
