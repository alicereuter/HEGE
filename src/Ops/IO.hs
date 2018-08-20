module Ops.IO (ioPrimitives, load) where

import Control.Monad.Except (throwError,liftM,liftIO)
import System.IO (IOMode (ReadMode,WriteMode)
                 ,openFile
                 ,hPrint
                 ,hGetLine
                 ,hClose
                 ,stdout
                 ,stdin)
import Datatypes (LispVal (List,String,Port,Bool),
                 LispError (TypeMismatch),
                 IOThrowsError)
import Parser (readExprLisp,readExpr)
import Ops.Util (liftThrows)


-- | list of io functions
ioPrimitives :: [(String,[LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file",  openInputFile),
                ("open-output-file", openOutputFile),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read",readProc),
                ("write",writeProc),
                ("read-contents",readContents),
                ("read-all",readAll)]

openInputFile :: [LispVal]  -> IOThrowsError LispVal
openInputFile  = makePort ReadMode

openOutputFile :: [LispVal] -> IOThrowsError LispVal
openOutputFile = makePort WriteMode

-- | Make a port to readfromfile
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ x = throwError $ TypeMismatch "expected a String" $ List x

-- | Closes file
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort badArg           = throwError $ TypeMismatch "expected a port" $ List badArg

-- | read from file 
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []   = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc x = throwError $ TypeMismatch "expected a  List of Strings" $ List x

-- | write to file
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc x = throwError $ TypeMismatch "expected a String and a port" $ List x

-- | read contents from file
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents x = throwError $ TypeMismatch "expected a  List of Strings" $ List x



-- | reads from a list of files
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll x = throwError $ TypeMismatch "expected a  List of Strings" $ List x

-- | Load a hege file
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprLisp
