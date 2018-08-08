{-# LANGUAGE UnicodeSyntax #-}
module Ops.Bool (boolOps) where

import Datatypes (LispVal (Bool)
                 ,LispError (TypeMismatch)
                 ,ThrowError)
import Ops.Util (boolBinop)

boolOps ∷ [(String, [LispVal] → ThrowError LispVal)]
boolOps = [("&&",boolBoolBinop (&&)),
          ("||",boolBoolBinop (||))]

-- | Does boolean operations
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowError LispVal
boolBoolBinop = boolBinop unpackBool

-- | Unpack LispVal to Bool
unpackBool :: LispVal -> ThrowError Bool
unpackBool (Bool b)  = return b
unpackBool  badArg = Left $ TypeMismatch "Expected boolean" badArg
