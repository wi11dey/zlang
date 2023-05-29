module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data Literal = Boolean Bool
             | Integer Int
             | Empty

data Name = Atom String
          | Wildcard String

data Argument = Name Name
              | Exact Literal
              | Typed Name (Maybe String)

type Definition = (Name, Expression)

data Expression = Literal Literal
                | Symbol String
                | Function (Maybe Argument) [Definition] Expression
                | Call Expression Expression

parseCharacter :: Parser Expression
parseCharacter = do string "#\\"
                    return parseHex <|> parse

parseString :: Parser Expression
parseString = do char '"'
                 characters <- many (noneOf "\"")
                 char '"'
                 return foldl' Call (Symbol "string")
                   $ map (Call (Symbol "character")) . (Literal . Integer . ord)
                   $ characters

parseAtom :: Parser String

parseWildcard :: Parser String
parseWildcard = do char '\''
                   parseAtom

parseName :: Parser Name
parseName =  (parseWildcard >>= Wildcard)
         <|> (parseSymbol   >>= Symbol)

force :: Expression -> [Expression]
force Literal _ = []
force Function _ _ _ = []
force Call f arg = [] -- TODO
force Variable name = [] -- TODO

main :: IO ()
main = do expr <- getLine
          putStrLn (readExpr expr)

-- {-# LANGUAGE OverloadedStrings #-}

-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import Data.Text (Text)
-- import Data.Void

-- data Literal =
--   | Boolean Bool
--   | Integer Int
--   | Empty

-- data Expression =
--   | Constant Literal
--   | Variable String
--   | Definition
--   | Function Argument
--   | Application Expression Expression

-- data Argument =
--   | Expression
--   | Wildcard Text
--   | Blank

-- type Parser = Parsec Void Text

-- force :: Expression -> Expression
-- force Constant c = c
-- force Function f = f
