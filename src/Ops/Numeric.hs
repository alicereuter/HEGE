{-# LANGUAGE FlexibleContexts,RankNTypes,LambdaCase, UnicodeSyntax  #-}
module Ops.Numeric (mathOps,unpackNum) where

import Ops.Types (num)
import Control.Monad.Except (throwError)
import Datatypes (LispError (NumArgs,TypeMismatch),
                 ThrowError,
                 LispVal (Number,Float))
import Ops.Util (boolBinop)

-- | Math Operations  
mathOps :: [(String, [LispVal] -> Either LispError LispVal)]
mathOps = [("+", add),
           ("-", sub ),
           ("*", mult),
           {- Integer ops -}
           ("div", division),
           ("mod", modulus),
           ("quotient", qoutient),
           ("remainder", remainder),
           {- float ops -}
           ("/", floatdiv ),
           ("=", numBoolBinop (==)),
           ("<", numBoolBinop (<)),
           (">", numBoolBinop (>)),
           ("/=",numBoolBinop (/=)),
           (">=",numBoolBinop (>=)),
           ("<=",numBoolBinop (<=))]
          
-- | addition function
add :: [LispVal] -> Either LispError LispVal
add = numericBinop (+)

-- | subtraction function
sub :: [LispVal] -> Either LispError LispVal
sub = numericBinop (-)

-- | multiplication function
mult :: [LispVal] -> Either LispError LispVal
mult = numericBinop (*)

-- | integer division
division :: [LispVal] -> Either LispError LispVal
division = intBinop (div)

modulus :: [LispVal] -> Either LispError LispVal
modulus = intBinop (mod)

qoutient :: [LispVal] -> Either LispError LispVal
qoutient = intBinop quot

remainder :: [LispVal] -> Either LispError LispVal
remainder = intBinop rem

floatdiv :: [LispVal] -> Either LispError LispVal
floatdiv = floatBinop (/)


-- | Generalized constructor for either Num or Float function
numericBinop :: (forall a. Num a => (a -> a -> a))  -> [LispVal] -> Either LispError LispVal
numericBinop _           []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop  op x = if num x
  then mapM unpackNum x >>= return . Number . foldl1 op
  else mapM unpackFloat x >>= return . Float . foldl1 op

-- | Unpack LispVal to integer
unpackNum :: LispVal -> ThrowError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number: " notNum


-- | Unpack LispVal to float
unpackFloat :: LispVal -> ThrowError Double
unpackFloat (Float n) = return n
unpackFloat notNum    = throwError $ TypeMismatch "float: " notNum

-- | Wrapper for integer functions
intBinop :: (Integer-> Integer-> Integer) -> [LispVal] -> Either LispError LispVal
intBinop _           []  = throwError $ NumArgs 2 []
intBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
intBinop  op x = mapM unpackNum x >>= return . Number . (foldl1 op) 

-- | Constructor for operation that operates on floating poing numbers
floatBinop :: (Double -> Double -> Double) -> [LispVal] -> Either LispError LispVal
floatBinop op x = mapM unpackFloat x >>= return . Float . foldl1 op

-- | Does Integer operation
numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowError LispVal
numBoolBinop  = boolBinop unpackNum
