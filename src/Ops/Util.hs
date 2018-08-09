module Ops.Util (str,chr,boolBinop,liftThrows) where

import Datatypes ( LispVal (Bool,String,Char),
                   LispError (NumArgs),
                   IOThrowsError,
                   ThrowError)
import Control.Monad.Except (throwError)
       
-- | function wrapper that takes two args and returns a boolean
boolBinop :: (LispVal -> ThrowError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right 
                                     
-- | Checks if a list lispvals are String
str :: [LispVal] -> Bool
str (String _:rest) = True && str rest
str (_:_)           = False
str []              = True

-- | Checks if a list of lispvals are chars
chr :: [LispVal] -> Bool
chr (Char _:rest) = True && chr rest
chr (_:_)         = False
chr []            = True

-- | Turns an Either LispError LispVal to ErrorT LispError IO LispVal
liftThrows :: ThrowError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
