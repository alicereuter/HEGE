{-# LANGUAGE FlexibleContexts,RankNTypes,LambdaCase, UnicodeSyntax  #-}
module Ops.Types (typeOps,num) where

import Datatypes (ThrowError,
                 LispVal (Number,Bool,Atom,String,Char),
                 LispError (TypeMismatch))
import Control.Monad.Except (throwError)
import Ops.Util (str)

typeOps :: [(String, [LispVal] -> ThrowError LispVal)]
typeOps = [("bool?", typeCheck bool),
           ("int?",  typeCheck num),
           ("string?", typeCheck str),
           ("sym?",    typeCheck atm ),
           ("symbol->str", symbolStr ),
           ("str->symbol", strSymbol)]


-- | wrapper that takes a list of lispval and applies function to lispval
typeCheck :: ([LispVal] -> Bool) -> [LispVal] -> ThrowError LispVal
typeCheck function c = if function c
                   then return $ Bool True
                   else return $ Bool False




-- | Checks if a list of lispVals are atoms
atm :: [LispVal] -> Bool
atm (Atom _:rest) = True && atm rest
atm (_:_)      = False
atm []            = True

-- | checks if a list of  lispVals are booleans
bool :: [LispVal] -> Bool
bool (Bool _:rest) = True && bool rest
bool (_:_)      = False 
bool []            = True


-- | checks if a list of lispvals are all numbers
num :: [LispVal] -> Bool
num = all (\case Number _ -> True; _ -> False)


-- | convert from symbol to String
symbolStr :: [LispVal] -> ThrowError LispVal
symbolStr [Atom x ] = (return .  String . tail) x
symbolStr y      = throwError $ TypeMismatch "not an atom" $ head  y


-- | convert string to symbol
strSymbol :: [LispVal] -> ThrowError LispVal
strSymbol [String x ] = (return . Atom) ('\'':x)
strSymbol y      = throwError $ TypeMismatch "not a string" $ head  y
