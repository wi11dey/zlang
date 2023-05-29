module Zlang.Syntax where
import Zlang.SExpression

-- Curried functions are desugared here.

data Literal = Boolean Bool
             | Integer Int
             | Empty

data Identifier = Global String
                | Local String

data Pattern = Identifier Identifier
             | Literal Literal
             | Typed Identifier (Maybe String)

type Definition = (Identifier, Expression)

data Expression = Constant Literal
                | Variable String
                | Function (Maybe Pattern) [Definition] Expression
                | Call Expression Expression
                deriving Eq, Show

definition :: SExpression -> Definition
definition List [Symbol "define", Symbol atom, definition]
definition List [Symbol "define", Quoted wildcard, definition]
definition List (Symbol "define"):(List f:args):body =
  definition List [Symbol "define", f, List (Symbol "function"):args] -- TODO unroll args

fromSExpression :: SExpression -> Expression
fromSExpression (List []) = Empty
fromSExpression (List [_]) = error "Call must have an argument."
fromSExpression (List [f, arg]) = Call (fromSExpression f) (fromSExpression arg)
fromSExpression (List first:second:rest) =
  fromSExpression (List (fromSEXpression $ List [first, second]):rest) -- Curry calls.
fromSExpression (List (Symbol "define"):_) = error "Definition cannot be an expression."
fromSExpression (List (Symbol "function"):arg:body) =
  Function
  (case arg of
      Symbol "_" -> Nothing
      List [] -> Just Exact Empty
      Integer i -> Just Exact Integer i
      Boolean b -> Just Exact Boolean b)
  (map definition  $ init body)
  (fromSExpression $ last body)
