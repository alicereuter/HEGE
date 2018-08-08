{-# LANGUAGE StandaloneDeriving #-}

module Datatypes where
import Text.ParserCombinators.Parsec (ParseError)               
import Control.Monad.Except (ExceptT,MonadError,catchError)
import Data.IORef (IORef)
import Data.List (intercalate)
import GHC.IO.Handle (Handle)

-- | Core Datatypes of LispVal
data LispVal = Atom String 
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Char Char
             | Float Double
             | PrimitiveFunc ([LispVal] -> ThrowError LispVal)
             | Func { params  :: [String],
                      vararg  :: (Maybe String),
                      body    :: [LispVal],
                      closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle


instance Show LispVal where show = showVal

type Env = IORef [(String, IORef LispVal)]

data LispError  = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String


type IOThrowsError = ExceptT LispError IO
type ThrowError = Either LispError


instance Show LispError where show = showError

showParse :: LispVal -> String
showParse  (Number x) = "Number " ++ show x
showParse (String x)  = "String " ++ show x
showParse (Atom x)    = "Atom " ++ show x
showParse  (List x)  = "List [" ++ intercalate "," (map show x) ++ "]"
showParse    _  = "NOT Finished"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ":  " ++ varname
showError (BadSpecialForm message form) = message ++ ":  " ++ show form
showError (NotFunction message func)   = message ++ ": " ++ show func
showError (NumArgs expected found)     = "expected " ++ show expected ++ " args pfound values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid " ++ expected ++ " found" ++ show found
showError (Parser parseErr)  = "Parse error at:  " ++ show parseErr
showError (Default err) = "Unknown Error ~(.^.)~:" ++ err




unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unwordsList' :: [LispVal] -> String
unwordsList' = (intercalate ", ") . map showType

showVal :: LispVal ->  String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name 
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList  contents ++ ")"
showVal (DottedList front back) = "(" ++ unwordsList front++ " . " ++ showVal back ++ ")"
showVal (Char c ) = "'"++[c]++"'"
showVal (PrimitiveFunc _) = "<primitive"
showVal (Func {params = args,
               vararg = varargs,
               body   = funcBody,
               closure  = _}) = "(function ( params" ++ unwords ( map show args) ++
                                   (case varargs of
                                      Nothing -> ") ...)"
                                      Just arg -> " . " ++ arg ++ ") ...)") ++ (concat (map show funcBody))
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


showType :: LispVal ->  String
showType (String contents) = "\"" ++ contents ++ "\" :: String" 
showType (Atom name) = name ++ " ∷ Atom"
showType (Number contents) = show contents ++ " :: Number"
showType (Float contents) = show contents ++ " :: Float"
showType (Bool True) = "#t"
showType (Bool False) = "#f"
showType (List contents) = "(" ++ unwordsList'  contents ++ ") ∷ List"
showType (DottedList front back) = "(" ++ unwordsList' front++ " . " ++ showType back ++ ")"
showType (Char c ) = "'"++[c]++"'"
showType (PrimitiveFunc _) = "<primitive"
showType (Func {params = args,
               vararg = varargs,
               body   = funcBody,
               closure  = _}) = "(lambda (" ++ unwords ( map show args) ++
                                   (case varargs of
                                      Nothing -> ""
                                      Just arg -> " . " ++ arg ++ ") ...)") ++ (concat (map show funcBody))
showType (Port _)   = "<IO port>"
showType (IOFunc _) = "<IO primitive>"

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (pure . show)
