{-# LANGUAGE RankNTypes,LambdaCase, FlexibleContexts, UnicodeSyntax  #-}
module Parser (readExprLisp,readExpr) where

import Text.ParserCombinators.Parsec (Parser
                                     ,parse
                                     ,(<|>)
                                     ,try
                                     ,choice
                                     ,many,
                                      many1
                                     ,endBy
                                     ,sepBy
                                     ,oneOf
                                     ,noneOf
                                     ,digit
                                     ,string
                                     ,letter,
                                      char)
import Control.Monad.Except (throwError)
import Numeric (readHex,readOct,readFloat)
import Datatypes (LispVal(Number
                         ,Float
                         ,List
                         ,DottedList
                         ,Atom
                         ,Bool
                         ,String
                         ,Char)
                 ,ThrowError
                 ,LispError (Parser))


symbol :: Parser Char
symbol = oneOf "!$%|*+-/:<=?>@^_~#&"

readOrThrow :: Parser a -> String -> ThrowError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
  
readExpr ∷ String → Either LispError LispVal
readExpr = readOrThrow parseExpr

-- | Parses a file with a list of expressions seperated by white space
--   or comments
readExprLisp :: String -> Either LispError [LispVal]
readExprLisp = readOrThrow (many parseExpr')

-- | Parses whitespace or comment
comment ::  Parser String
comment = do
  _ <- char '-'
  _ <- char '-'
  _ <- many $ noneOf "\n"
  _ <- string "\n"
  return "dog"

parseExpr' :: Parser LispVal
parseExpr' = do
  _ <- parseWs
  expr <- parseExpr
  _ <- parseWs
  return expr
parseWs :: Parser [String]
parseWs = many $ choice [comment,
                         spaces]
spaces :: Parser String
spaces = many1 $ oneOf " \n\r\t" 


escapeStuff :: Parser Char
escapeStuff =  (char '\\' >> char '\"' >> return  '\"' )
  <|>   (char '\\' >> char '\n' >> return  '\n' )
  <|> try (char '\\' >> char '\r' >> return  '\r' )
  <|> try (char '\\' >> char '\t' >> return  '\t' )
  <|> try (char '\\' >> char '\\' >> return  '\\' )
  <|> noneOf "\""

-- | Parses string which are characters surrounded by "
parseString :: Parser LispVal
parseString = do _ <- char '"'
                 str <-  many escapeStuff
                 _ <- char '"'
                 return $ String  str

-- | Parses hege char data type which is defined a character surrounded by '
parseChar :: Parser LispVal
parseChar = do _ <- char '\''
               t <- letter <|> symbol
               _ <- char '\''
               return $ Char t


parseAtom :: Parser LispVal                 
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _ -> Atom atom

-- | Parses list of expressions separated by spaces
parseList :: Parser LispVal
parseList = do
  _ <- char '('
  list <- sepBy parseExpr spaces
  _ <- char ')'
  return $ List list

parseInt :: Parser LispVal
parseInt = try parseOct
   <|> try parseHex
   <|> parseNum

parseNumber :: Parser LispVal
parseNumber = try parseFloat
  <|> parseInt

-- | Parses hex number
parseHex :: Parser LispVal
parseHex = do
  _<-string "#h"
  hexNum <-  many1 (digit <|> oneOf "abcdef")
  return $ Number $ fst (readHex hexNum !!0)

-- | Parses octal number
parseOct :: Parser LispVal
parseOct = do
  _<-string "#o"
  octNum <- many1 (oneOf "01234567")
  return $ Number $ fst (readOct octNum !! 0)

parseFloat :: Parser LispVal
parseFloat = do
          float <- many1 digit
          _<-char '.'
          start <- many1 digit
          return $ Float $ (fst ((readFloat $ float ++ "." ++ start) !! 0))

-- | Parses regular number
parseNum :: Parser LispVal         
parseNum  = do
  digits <- many1 digit
  return $ Number $ read digits
  
parseDottedList :: Parser LispVal
parseDottedList = do
  _ <- char '('
  front <- endBy parseExpr spaces
  back <- char '.' >> spaces >> parseExpr
  _ <- char ')'
  return $ DottedList front back
  

parseQuoted :: Parser LispVal
parseQuoted = do
  _<-char '\''
  x <- parseExpr
  return $ List [ Atom "quote",x]

parseExpr :: Parser LispVal
parseExpr = parseNumber
  <|> parseString
  <|> parseAtom
  <|> try parseChar
  <|> parseQuoted
  <|>  (try parseList) <|> parseDottedList
