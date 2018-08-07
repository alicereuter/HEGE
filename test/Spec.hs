{-# LANGUAGE RankNTypes,LambdaCase, FlexibleContexts, UnicodeSyntax  #-}

import Test.Hspec
import Datatypes
import Parser
import Control.Monad
import Eval
import Control.Monad.Except


main = hspec $ do
  describe "NumParsing" $ do
    it "parses floats" $ do
      shouldReturn (z "3.13")  "3.13"
    it "parses ints" $ do
      shouldReturn (z  "3") "3"
      shouldReturn (z  "312") "312"
    it "parses octal" $ do
      shouldReturn (z  "#o2")   "2" 
      shouldReturn (z  "#o10")  "8"
      shouldReturn (z  "#hff")  "255"
  describe "String Parsing" $ do
    it "parses strings" $ do
      shouldReturn (z "\"arst \"")  "\"arst \""
      shouldReturn (z  "\" this \\\" is \"") "\" this \" is \""
  describe "Evaluation " $ do
    it "math " $ do
      shouldReturn ( z "(+ 2 2)")  "4"
      shouldReturn ( z "(+ 2 (- 4 1))")  "5"
      shouldReturn ( z "(- (+ 4 6 3) 3 5 2)" )  "3"
      shouldReturn ( z "(+ 3.3 3.3)" )  "6.6"
    it "does type" $ do
      shouldReturn (z "(int? 4)")  "#t"
    it "list" $ do
      shouldReturn (z "(head '(1 2))")  "1"
      shouldReturn (z "(tail '(3 4))") "(4)"
      shouldReturn (z "(length \"srr\")")  "3"
    it "tests" $ do
      shouldReturn (z "(cond (= 3 3) 4)") "4"
      shouldReturn (z "(cond (= 2 3) 3 (= 3 3))")  "expected 2 args pfound values #t"
      shouldReturn (z "(cond (= 2 3) 3)")  "expected 2 args pfound values \"no values\""
    it "case" $ do
      shouldReturn (z "(case 9 ((0 9) 'dog))") "(quote dog)"
      shouldReturn (z "(case (* 3 3) ((9 0) 'dsr))")  "(quote dsr)"
      shouldReturn (z "(case (* 3 3) ((0 0) 'dsr))")   "Doesn't match pattern: \"\""
      shouldReturn (z "(case (* 3 3) ((0 0) 'dsr) ((9 0) 9)))")  "9"
    it "string" $ do
      shouldReturn (z "(substr \"abcdef\" 0 1)") "\"ab\""
      shouldReturn (z "(substr \"abcdef\" 0 0)") "\"a\""
      shouldReturn (z "(concat \"a\" \"b\")") "\"ab\""
      shouldReturn (z "(string->list \"dog\")")  "('d' 'o' 'g')" 
      shouldReturn (z "(list->string '('a' 'b' 'c'))") "\"abc\""
    it "bool" $ do
      shouldReturn (z "(|| #t #t)") "#t"
      where z x = evalStr x

evalStr x = do
  env <- primitiveBindings 
  evalString env x
  
evalString env expr   = runsIOThrows $ liftM show $ (liftThrows $ readExpr expr)>>= eval env

extractValue' ∷ (Either LispError LispVal) → String
extractValue' x = do
  case x of
    (Right x) → show x
    (Left x) → show x

