import Compile
import Control.Monad.Error

test = List [Atom "+",Number 1, Number 2]

extractValue' :: (Either LispError LispVal) -> String
extractValue' x = do
  case x of
    (Right x) -> show x
    (Left x) -> show x

main = do
  env <- primitiveBindings
  let res = eval env test
  extr <- runErrorT res
  putStrLn $ extractValue' extr
