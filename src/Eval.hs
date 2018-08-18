{-# LANGUAGE FlexibleContexts,RankNTypes,LambdaCase, UnicodeSyntax  #-}
module Eval where

import Datatypes (ThrowError
                 ,LispVal (Bool,Number,List,Atom,Func,String,DottedList,Float,IOFunc,Char,PrimitiveFunc)
                 ,LispError (TypeMismatch,UnboundVar,NumArgs,Default,NotFunction,BadSpecialForm)
                 ,Env
                 ,trapError
                 ,showVal
                 ,IOThrowsError)
import Control.Monad.Except (throwError,ExceptT,liftIO,liftM,runExceptT)
import Data.IORef (newIORef,readIORef,writeIORef)
import Ops.Util (str,liftThrows)
import Ops.Types (typeOps)
import Ops.List(listOps)
import Ops.Numeric (mathOps)
import Ops.Bool (boolOps,eqv)
import Ops.IO (ioPrimitives,load)

-- | Returns an empty IORef environment
nullEnv :: IO Env
nullEnv = newIORef []

-- | Returns an environment with bindings with all IOFunctions as well as primitive functions
-- | rewrite this TODO rewrite this
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunction IOFunc) ioFuncs  ++ map (makeFunction PrimitiveFunc) primitives)
  where makeFunction constructor (var,func) = (var, constructor func)
        ioFuncs = ("apply",applyFunc):ioPrimitives


-- | Runs an IOThrowe into string
runsIOThrows :: IOThrowsError String -> IO String
runsIOThrows action = runExceptT (trapError action) >>= return . extractString


-- | Extract String from ThrowError
extractString :: Either LispError String -> String
extractString (Right val) = val
extractString (Left err)  = show err

-- | Extracts lispVal from ThrowError
extractValue :: Either LispError LispVal -> LispVal
extractValue (Right val ) = val
extractValue (Left val)   = String $ show val

-- | Evaluates a Lisp Val in an environment
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float _ ) = return val
eval _ val@(Bool _)   = return val
eval _ (List [Atom "quote", val]) =return val
eval env (List [Atom "if", predicat, conseq, alt]) = ifFunc env predicat conseq alt
eval env (List (Atom "cond":form)) = cond env $ List form
eval env (List (Atom "case": form))  = casex env $ List form
eval env (List [Atom "set!", Atom var, form])  = eval env form >>= setVar env var
eval env (List [Atom "define",Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "isIO",function]) = eval env function >>= isIO 
eval env (List (Atom "define" : List (Atom var : parameter) : bodyFunc)) = makeNormalFunc env parameter bodyFunc >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : parameters) varargs : bodyFunc)) = makeVarArgs varargs env parameters bodyFunc >>= defineVar env var
eval env (List (Atom "lambda" : List parameter : bodyFunc)) = makeNormalFunc env parameter bodyFunc
eval env (List (Atom "lambda" : DottedList parameter varargs : bodyFunc)) = makeVarArgs varargs env parameter bodyFunc
eval env (List (Atom "lambda" : varargs@(Atom _) : bodyFunc)) = makeVarArgs varargs env [] bodyFunc
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval _ char@(Char _) = return char
eval env (Atom varName)      = getVar env varName
eval env (List ( function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "unregonized form " badForm

ifFunc :: Env  -> LispVal -> LispVal -> LispVal -> ExceptT LispError IO LispVal
ifFunc env predicat conseq alt = do
  result <- eval env predicat
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    notpred -> throwError $ TypeMismatch "not bool" notpred

-- | function used to create function that takes a list as an argument
defineList :: Env -> String -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
defineList env var parameter bodyFunc = makeNormalFunc env parameter bodyFunc >>= defineVar env var

-- | function used to create function that takes a dotted list as an argument
defineDottedList :: Env
                          -> String
                          -> [LispVal]
                          -> LispVal
                          -> [LispVal]
                          -> ExceptT LispError IO LispVal
defineDottedList env var parameters varargs bodyFunc= makeVarArgs varargs env parameters bodyFunc >>= defineVar env var

-- | function used to set a variable to a value
setFunction :: Env
                     -> String -> LispVal -> ExceptT LispError IO LispVal
setFunction env var form= eval env form >>= setVar env var

-- | Constructor to make Function
makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env parameters bodyFunc = return $ Func (map showVal parameters) varargs bodyFunc env

-- | constructor to make function with static number of args
makeNormalFunc ::  Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

-- | Constructur to make function with
makeVarArgs :: LispVal ->  Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs    = makeFunc . Just . showVal

-- | Evaluates cond statement 
cond :: Env -> LispVal -> IOThrowsError LispVal
cond env (List x) = condl env x
cond _ badArg     = throwError $ TypeMismatch "not a list" $ badArg

-- | cond function core evaluation
condl :: Env -> [LispVal] -> IOThrowsError LispVal
condl _ [y]= throwError $ NumArgs 2  [y]
condl _ [] = throwError $ NumArgs 2  [String "no values"]
condl env  x = do
  result <- eval env (x !! 0)
  evaled ← mapM (eval env) $ drop 1 x
  case result of
    Bool True -> return (evaled !! 0)
    Bool False-> cond env $ List $ tail evaled
    badArg        -> throwError $ TypeMismatch "not a Boolean" $ badArg


-- | wrapper for case unpacks List val
casex :: Env -> LispVal -> IOThrowsError LispVal
casex env (List x) = casel env x
casex _ badArg     = throwError $ TypeMismatch "not a list" $ badArg

-- | evaluates case prgoram flow
casel :: Env ->  [LispVal] -> IOThrowsError LispVal
casel env (x:xs) = do
  match <- eval env x
  let trues =  map (\cases -> help match cases) xs
  let t =   filter (\pair -> fst pair) $ zip trues xs
  if null t then throwError $ NotFunction "Doesn't match pattern"  ""
    else do
    let result = (\list -> case list of
                (List ((List _): args)) -> args !! 0
                _                          -> String "" ) $ snd $ head t
    return $ result   
casel _ [] = throwError $ NumArgs 1 [String "not enough"]


-- | applies a function to it's argument 
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func parameters varargs bodyFunction environment) args =
  if number parameters /= number args && varargs == Nothing
     then throwError $ NumArgs (number parameters) args
     else (liftIO $ bindVars environment $ zip parameters args) >>= bindVarArgs varargs>>= evalBody
  where remainingArgs = drop (length parameters) args
        number = toInteger . length
        evalBody env = liftM last $ mapM (eval env) bodyFunction
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env          
apply (IOFunc func) args = func args
apply val y= throwError $ Default $ " tried to apply " ++ (show val) ++ " to  " ++ (show y)


-- | table of primitive function
primitives :: [(String,[LispVal] -> ThrowError LispVal)]
primitives = concat $ [mathOps
                      ,typeOps
                      ,listOps
                      ,boolOps]




{- Type checking ops -}
-- | Checks a list of lispvals are floats
checkifFloat :: [LispVal] -> Bool
checkifFloat (Float _:rest) = True && str rest
checkifFloat (_:_)          = False
checkifFloat []             = True

-- | wrapper that takes a list of lispval and applies function to lispval
typeCheck :: ([LispVal] -> Bool) -> [LispVal] -> ThrowError LispVal
typeCheck function lispVal = if function lispVal
                   then return $ Bool True
                   else return $ Bool False

-- | Checks if LispVal is in List of LispVal
help :: LispVal -> LispVal -> Bool
help y (List ((List first):_)) = any (\x -> unpackBoolRaw $ extractValue $ eqv [x,y]  ) first
help _ _ = False

-- | Unpacks a lispVal to boolean
unpackBoolRaw :: LispVal -> Bool
unpackBoolRaw (Bool b) = b
unpackBoolRaw _ = False

-- | Checks if a variable is bound
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- | Gets Variable from environment and returns it value
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <-liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Variable not defined" var) (liftIO . readIORef) (lookup var env)

-- | Sets variable in the environment
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "unbound variable" var) (liftIO . (flip writeIORef value)) (lookup var env)
  return value

-- | defines new variable in the environment
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var,valueRef): env)
    return value
    
-- | binds a list of variables to an environment
bindVars :: Env -> [(String,LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv funcBindings env = liftM (++ env) (mapM addBinding funcBindings)
        addBinding (var,value) = do ref <- newIORef value
                                    return (var, ref)

isIO :: LispVal -> ExceptT LispError IO LispVal
isIO (Func _ _ funcBody _) = return $ Bool $ any id $  map typeLisp funcBody
isIO badArg = throwError $ TypeMismatch "expected a function" $ badArg


listIO :: [String]
listIO = "define":"set!":"load":map fst ioPrimitives

typeLisp :: LispVal -> Bool
typeLisp (Atom a) = elem a listIO
typeLisp (List list) = any id $ map typeLisp list
typeLisp _  = False

-- | apply function to vars 
applyFunc :: [LispVal] -> IOThrowsError LispVal
applyFunc [func, List args] = apply func args
applyFunc (func : args)     = apply func args
applyFunc x = throwError $ TypeMismatch "expected a function and Args" $ List x
