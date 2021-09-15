module Jq.JParser where
import Debug.Trace

import Jq.Json
import Parsing.Parsing

-- JNull parser
parseJNull :: Parser JSON
parseJNull = do
  _ <- symbol "null"
  return JNull

-- True pareser
parseJTrue :: Parser JSON
parseJTrue = do
  _ <- symbol "true"
  return (JBool True)

-- False parser
parseJFalse :: Parser JSON
parseJFalse = do
  _ <- symbol "false"
  return (JBool False)

-- String parser
parseJString :: Parser JSON
parseJString = do
    _ <- char '\"'
    n<-ident 
    _<-char '\"'
    return (JString n)

-- Doubles
parseJNumber :: Parser JSON 
parseJNumber = do
  a <-  double
  return (JNumber a) 
    

parseJArray :: Parser JSON
parseJArray = do 
    _ <- symbol "["
    n <- parseJSON
    ns <- many (do 
        _<-symbol "," 
        parseJSON)
    _ <- symbol "]"
    return (JArray (n:ns)) 

parseEmptyJArray :: Parser JSON 
parseEmptyJArray = do
  _ <- symbol "[]"
  return (JArray []) 

parseEmptyJObject :: Parser JSON 
parseEmptyJObject = do
  _ <- symbol "{}"
  return (JObject [])

parseJObject :: Parser JSON
parseJObject = do 
    _ <- symbol "{\""
    
    key <- stringIdentrifier   
    _ <- symbol "\""
    _ <- token (symbol ":")
    value <- parseJSON
    ns <- many (do
        _ <-symbol ","
        (do 
            _<-symbol "\""
            k <- stringIdentrifier 
            _<-symbol "\"" 
            _ <- token (symbol ":")
            v <- parseJSON
            return (k,v)
            )
        )
    _ <- symbol "}"
    return (JObject ((key, value):ns)) 



parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseJFalse <|> parseJTrue <|> parseJNumber <|> parseJString <|> parseJArray <|> parseJObject <|> parseEmptyJArray <|> parseEmptyJObject
