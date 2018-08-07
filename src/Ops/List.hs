module Ops.List (listOps) where 

import Datatypes ( LispVal (List,DottedList,String,Char,Number),
                   LispError (NumArgs,TypeMismatch,Default),
                 ThrowError)
import Ops.Util (boolBinop,str,chr)
import Control.Monad.Except( throwError)

listOps :: [(String, [LispVal] -> ThrowError LispVal)]
listOps = [("length", strLength),
             ("ind",strRef),
             ("substr",substring),
             ("concat",concatl),
             ("string->list",strList),
             ("list->string",listStr),
             ("string=?",strBoolBinop (==)),
             {- list -}
             ("head",headl),
             ("tail",taill),
             ("++", cons)]

-- | takes the length of the string
strLength :: [LispVal] -> ThrowError LispVal
strLength [(String x)] = return $ Number $ toInteger $ length x
strLength badArg =  Left $ TypeMismatch "expected a String" $ head badArg


strRef :: [LispVal] -> ThrowError LispVal
strRef [(String string),(Number x)] = do
  if (fromInteger x) >= length string || x < 0 then
    throwError $ Default "invalid index"
    else return $ Char $ string !! (fromInteger x)
strRef _ = throwError $ Default "Not a String and Number "


-- | Takes substring of list
substring :: [LispVal] -> ThrowError LispVal
substring [(String string),(Number x),(Number y)] = return $ String $ take ((fromInteger  y )- (fromInteger x-1)) dr
  where dr = drop (fromInteger x) string
substring badArg =  Left $ TypeMismatch "expected String" $ head badArg


-- | concats a list of strings
concatl :: [LispVal] -> ThrowError LispVal
concatl x = if str x
  then return $ String $ concat $map  unwrap x
  else throwError $ Default "not all strings"
  where unwrap = (\wrappedString -> case wrappedString of
            (String string) -> string
            _ -> "")

-- | Converts from a string to  a list of chars
strList :: [LispVal] -> ThrowError LispVal
strList [String s] = return $ List $ map (\x -> Char x) s
strList badArg = Left $ TypeMismatch "expected String" $ head badArg


-- | converts from list of chars to string
listStr :: [LispVal] -> ThrowError LispVal
listStr [List s] = if chr s
  then return $ String $ map (\x -> case x of
                                 (Char c) -> c
                                 _ -> ' ') s
       else throwError $ Default "not chrs"
listStr badArg = Left $ TypeMismatch "expected list of Chars" $ head badArg

-- | head of a list of LispVal
headl :: [LispVal] -> ThrowError LispVal
headl [List (x:_)]         = return x
headl [DottedList (x :_) _]  = return x
headl [badarg]              = throwError $ TypeMismatch "pair" badarg
headl badArg                   = throwError $ NumArgs 1 badArg

-- | takes tail of a list of LispVals
taill :: [LispVal] -> ThrowError LispVal
taill [List (_:xs)]         = return $ List xs
taill [DottedList [_] x]    = return x
taill [DottedList (_:xs) x] = return $ DottedList xs x
taill [badarg]              = throwError $ TypeMismatch "pair" badarg
taill badArg                   = throwError $ NumArgs 1 badArg

-- | evaluates cons of dotted list
cons :: [LispVal] -> ThrowError LispVal
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList ( x : xs) xlast
cons [x1,x2] = return $ DottedList [x1] x2
cons badArg     = throwError $ NumArgs 2 badArg

-- | Does  string operations
strBoolBinop :: (String -> String -> Bool ) -> [LispVal] -> ThrowError LispVal
strBoolBinop  = boolBinop unpackStr

-- | Unpack LispVal to String
unpackStr :: LispVal -> ThrowError String
unpackStr (String n) = pure n
unpackStr badArg = Left $ TypeMismatch "Expected String" badArg
