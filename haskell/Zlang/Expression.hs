module Zlang.Expression where
import Text.Parsec

-- Parser of zlang's Scheme-like syntax. N.B. `quote` and the read syntax ' are distinct to this parser.
-- Syntactic sugar for literals is desugared here:
-- - Character literals: #\A is syntactic sugar for (character 65)
-- - String literals: "AA" is syntactic sugar for (string (character 65) (character 65))
-- - Rational literals: 1/2 is syntactic sugar for (rational 1 2)
-- - Floating-point literals: 0.5 is syntactic sugar for (rational 5 10)
-- - Complex literals 1/2+0.5i is syntactic sugar for (complex ((real part) (rational 1 2)) ((imaginary part) (rational 5 10)))

type Parser = Parsec Void String

data Expression = Symbol String
                 | Quoted String
                 | Integer Int
                 | List [Expression]
                 deriving Eq, Show

identifier :: Parser String
identifier = letters

symbol :: Parser Expression
symbol = identifier >>= Symbol

quoted :: Parser Expression
quoted = char '\'' >> identifier >>= Quoted

string :: Parser Expression
string = do char '"'
                 characters <- many (noneOf "\"")
                 char '"'
                 return foldl' Pair (Symbol "string")
                   $ map (Pair (Symbol "character")) . (Integer . ord)
                   $ characters
