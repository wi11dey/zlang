module Zlang.Expression where
import Data.Complex
import Text.Parsec

-- Parser for R5RS Scheme.

type Parser = Parsec Void String

data Expression = Boolean Bool
                | Symbol String
                | Character Char
                | Vector [Expression]
                | Pair Expression Expression
                | Complex (Complex Double)
                | Real Double
                | Rational Integer Integer
                | Integer Integer
                | String String
                | Empty
                deriving Eq

instance Show Expression where
  show $ Boolean True  = "#t"
  show $ Boolean False = "#f"
  show $ Symbol sym = s
  show $ Character char = do
    "#\\"
    return cc
  show $ Vector vec = do
    "#("
    element <- vec
    show element
    ")"
  show $ Pair car cdr = do
    "("
    showPair car cdr
    ")" where
      showPair head (Pair car cdr) = do
        show head
        showPair car cdr
      showPair car cdr = do
        show head
        " . "
        show tail

  show $ Complex c = show c
  show $ Real    r = show r
  show $ Rational numerator denominator = do
    show numerator
    "/"
    show denominator
  show $ Integer i = show i
  show $ String str =
    return '"'
    str
    return '"'
  show Empty = "()"

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
