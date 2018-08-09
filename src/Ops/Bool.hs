{-# LANGUAGE UnicodeSyntax #-}
module Ops.Bool (boolOps,eqv) where

import Control.Monad.Except (throwError)
import Datatypes (LispVal (Bool,Number,Float,String,Atom,DottedList,List)
                 ,LispError (TypeMismatch,NumArgs)
                 ,ThrowError)
import Ops.Util (boolBinop)

boolOps ∷ [(String, [LispVal] → ThrowError LispVal)]
boolOps = [("=",eqv)
          ,("&&",lispAnd)
          ,("||",lispOr)]

lispAnd ∷ [LispVal] → ThrowError LispVal
lispAnd = boolBoolBinop (&&)

lispOr ∷ [LispVal] → ThrowError LispVal
lispOr  = boolBoolBinop (||)

-- | Does boolean operations
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowError LispVal
boolBoolBinop = boolBinop unpackBool

-- | Unpack LispVal to Bool
unpackBool :: LispVal -> ThrowError Bool
unpackBool (Bool b)  = return b
unpackBool  badArg = Left $ TypeMismatch "Expected boolean" badArg

-- | Eqvuilance checking function
eqv :: [LispVal] -> ThrowError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Float arg1),  (Float arg2)]          = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left _ -> False
                                Right (Bool val) -> val
                                Right _ -> False
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList
