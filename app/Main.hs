{-# LANGUAGE RankNTypes,LambdaCase, FlexibleContexts, UnicodeSyntax  #-}
module Main where

import Parser
import Eval
import Datatypes
import Repl
import System.Environment
import Control.Monad
import Control.Monad
import System.IO
import Eval
import Datatypes
import Parser
import Ops.Util (liftThrows)

evalString env expr   = runsIOThrows $ liftM show $ (liftThrows $ readExpr expr)>>= eval env

evalAndPrint  ∷ Env → String → IO ()
evalAndPrint env expr 
  | command == ":t"  =  putStrLn . extractValue' showType   $ readExpr processCommand
  | command == ":l"  =  putStrLn . extractValue' showParse  $ readExpr processCommand
  | otherwise =  evalString env expr >>= putStrLn
  where command = take 2 expr
        processCommand = drop 3 expr
        
extractValue' :: Show a => (t -> String) -> Either a t -> String
extractValue' showFunc x = do
  case x of
    (Right x) → showFunc x
    (Left x) → show x

runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runsIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== ":q") (readPrompt "λ> ") .  evalAndPrint

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


readPrompt prompt = flushStr prompt >> getLine



until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action


main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then runRepl
    else runOne args
