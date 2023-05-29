module Zlang.REPL where
import Zlang.SExpression

repl :: IO ()
repl = do
  putStr "z> "
  hFlush stdout
  line <- getLine
  fromSExpression List [Symbol "function", Symbol "_"]:(parse line)
  repl
