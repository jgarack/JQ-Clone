module Jq.CParser where



import Parsing.Parsing
import Jq.Filters
    ( Config (ConfigC),
      Filter(Not, Identity, RecursiveDescent, ArrayInObject, NullFilter,
             ObjectIndexing, OptionalObjectIndexing, ArrayIndex,
             OptionalArrayIndex, ArraySlicing, OptionalArraySlicing, Iterator,
             OptionalIterator, Pipe, Equals, Greater, GreaterOrEqual, Less,
             LessOrEqual, NotEquals, And, Or, Coma, JNumberConstructor,
             JStringConstructor, JBooleanConstructor, JNullConstructor,
             JArrayConstructor, JObjectConstructor, SquareBrackets,
             CurlyBrackets, EmptyCurlyBrackets, Implication) )
import Jq.JParser
import Jq.Json ( JSON(JDot, JArray, JObject, JEmpty, JString) )

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseRecursiveDescent :: Parser Filter
parseRecursiveDescent = do
  _ <- token . char $ '.'
  _ <- token . char $ '.'
  return RecursiveDescent 

parseObjectIndexing :: Parser Filter
parseObjectIndexing = parseOptionalObjectIndexingNormal <|> parseOptionalObjectIndexingQuote <|> parseOptionalObjectIndexingGeneric <|> parseObjectIndexingQuote <|> parseObjectIndexingNormal <|> parseObjectIndexingGeneric <|> parseArrayInObject

parseArrayInObject :: Parser Filter 
parseArrayInObject = do
  _ <- symbol "[]"
  ns <- parseObjectIndexing <|> parseNothing
  return (ArrayInObject ns) 

parseObjectIndexingGeneric :: Parser Filter
parseObjectIndexingGeneric = do
  _ <- char '.'
  _ <- symbol "[\""
  a <- stringIdentrifier  
  _ <- symbol "\"]"
  ns <- parseObjectIndexing <|> parseNothing
  return (ObjectIndexing a ns)

parseNothing :: Parser Filter 
parseNothing = do
  return (NullFilter)

parseObjectIndexingQuote :: Parser Filter
parseObjectIndexingQuote = do
  _ <- char '.'
  _ <- char '\"'
  a <- stringIdentrifier 
  _ <- char '\"' 
  ns <- parseObjectIndexing <|> parseNothing
  return (ObjectIndexing a ns)

parseObjectIndexingNormal :: Parser Filter
parseObjectIndexingNormal = do
  _ <- char '.'
  
  a <- identifier 
  ns <- parseObjectIndexing <|> parseNothing
  return (ObjectIndexing a ns)


parseOptionalObjectIndexingNormal :: Parser Filter
parseOptionalObjectIndexingNormal = do 
  _ <- char '.'
  
  a <- identifier 

  _ <- char '?'
  ns <- parseObjectIndexing <|> parseNothing
  return (OptionalObjectIndexing a ns)


parseOptionalObjectIndexingQuote :: Parser Filter
parseOptionalObjectIndexingQuote = do 
  _ <- char '.'
  _ <- char '\"'
  a <- stringIdentrifier 
  _ <- char '\"' 
  _ <- char '?'
  ns <- parseObjectIndexing <|> parseNothing
  return (OptionalObjectIndexing a ns)

parseOptionalObjectIndexingGeneric :: Parser Filter
parseOptionalObjectIndexingGeneric = do 
  _ <- char '.'
  _ <- char '[' 
  _ <- char '\"' 
  a <- stringIdentrifier 
  _ <- char '\"' 
  _ <- char ']'  
  _ <- char '?'
  ns <- parseObjectIndexing <|> parseNothing
  return (OptionalObjectIndexing a ns)

parseBasicFilters :: Parser Filter 
parseBasicFilters = parseOptionalIterator <|> parseIterator <|> parseSquareBrackets <|> parseCurlyBrackets  <|> parseEmptyCurlyBrackets <|> parseConstructors <|> parseParenthesis <|> parseNot <|> parseOptionalArrayIndexing <|> parseOptionalArraySlicing <|> parseArraySlicing <|>parseObjectIndexing <|> parseArrayIndexing <|>parseObjectIndexing <|> parseRecursiveDescent <|> parseIdentity 

parseArrayIndexing :: Parser Filter
parseArrayIndexing = do
  _ <- symbol ".["
  a <- integer
  _ <- symbol "]"
  return (ArrayIndex a)

parseOptionalArrayIndexing :: Parser Filter
parseOptionalArrayIndexing = do
  _ <- symbol ".["
  a <- integer
  _ <- symbol "]"
  _ <- symbol "?"

  return (OptionalArrayIndex a)

parseArraySlicing :: Parser Filter 
parseArraySlicing = do
  _ <- symbol ".["
  a1 <- natural
  _ <- symbol ":"
  a2 <- natural 
  _ <- symbol "]"
  return (ArraySlicing a1 a2)

parseOptionalArraySlicing :: Parser Filter 
parseOptionalArraySlicing = do
  _ <- symbol ".["
  a1 <- natural
  _ <- symbol ":"
  a2 <- natural 
  _ <- symbol "]?"
  return (OptionalArraySlicing a1 a2)

parseIterator :: Parser Filter
parseIterator = do
  _ <- symbol ".["
  ns <- many (do
    _ <- many (do char ',')
    natural)
  _<-symbol "]"
  return (Iterator ns)

parseOptionalIterator :: Parser Filter
parseOptionalIterator = do
  _ <- symbol ".["
  ns <- many (do
    _ <- many (do char ',')
    natural)
  _<-symbol "]?"
  return (OptionalIterator ns)


parsePipes :: Parser Filter
parsePipes = do 
  a <- parseFilterForPipes
  _ <- token (char '|')
  b <- parseFilter
  return (Pipe a b)

parseEquals :: Parser Filter
parseEquals = do 
  a <- parseBasicFilters
  _ <- symbol "=="
  b <- parseBasicFilters
  return (Equals a b)

parseGreater :: Parser Filter
parseGreater = do
  a <- parseBasicFilters
  _ <- symbol ">"
  b <- parseBasicFilters
  return (Greater a b)

parseGreaterOrEqual :: Parser Filter
parseGreaterOrEqual = do
  a <- parseBasicFilters
  _ <- symbol ">="
  b <- parseBasicFilters
  return (GreaterOrEqual a b)

parseLess :: Parser Filter
parseLess = do
  a <- parseBasicFilters
  _ <- symbol "<"
  b <- parseBasicFilters
  return (Less a b)

parseLessOrEqual :: Parser Filter
parseLessOrEqual = do
  a <- parseBasicFilters
  _ <- symbol "<="
  b <- parseBasicFilters
  return (LessOrEqual a b)

parseNotEquals :: Parser Filter
parseNotEquals = do 
  a <- parseBasicFilters
  _ <- symbol "!="
  b <- parseBasicFilters
  return (NotEquals a b)

parseAnd :: Parser Filter
parseAnd = do
  a <- parseBasicFilters 
  _ <- symbol "and"
  b <- parseBasicFilters
  return (And a b)

parseOr :: Parser Filter
parseOr = do
  a <- parseBasicFilters 
  _ <- symbol "or"
  b <- parseBasicFilters
  return (Or a b)

parseComas :: Parser Filter 
parseComas = do
  a <- parseEquals <|> parseBasicFilters
  _ <- token (symbol ",")
  b <- parseComas <|> parseEquals <|> parseBasicFilters 
  return (Coma a b)

parseParenthesis :: Parser Filter
parseParenthesis = do
  _<-symbol "("
  a <- parseFilter
  _<-symbol ")"
  return a

parseConstructors :: Parser Filter
parseConstructors = parseJNumberConstructor <|> parseJStringConstructor <|> parseJTrueConstructor <|> parseJFalseConstructor <|> parseJNullConstructor <|> parseJArrayConstructor <|> parseJObjectConstructor

parseJNumberConstructor :: Parser Filter
parseJNumberConstructor = do
  a <- parseJNumber
  return (JNumberConstructor a)

parseJStringConstructor :: Parser Filter
parseJStringConstructor = do
  a <- parseJString 
  return (JStringConstructor a)


parseJTrueConstructor :: Parser Filter
parseJTrueConstructor = do
  a <- parseJTrue
  return (JBooleanConstructor a)

parseJFalseConstructor :: Parser Filter
parseJFalseConstructor = do
  a <- parseJFalse 
  return (JBooleanConstructor a)

parseJNullConstructor :: Parser Filter
parseJNullConstructor = do
  a <- parseJNull
  return (JNullConstructor a)

parseJArrayConstructor :: Parser Filter
parseJArrayConstructor = do
  a <- parseJArray2
  return (JArrayConstructor a)

parseJObjectConstructor :: Parser Filter
parseJObjectConstructor = do
  a <- parseJObject2
  return (JObjectConstructor a)

parseSquareBrackets :: Parser Filter 
parseSquareBrackets = do
  _ <- char '['
  a <- parseFilterForSB
  ns <- many (do 
    _ <- token (char ',')
    parseFilterForSB)
  _ <- char ']'
  return (SquareBrackets (a:ns))


parseCurlyBrackets :: Parser Filter
parseCurlyBrackets = do 
  _ <- symbol "{"
  ls <- many (do
    key <- parseFilter <|> (
          do
            _ <- many (char '\"')
            a <- identifier  
            _ <- many (char '\"')
            return (JStringConstructor (JString a))

            )
    value <- many (do 
      _ <- char ':'
      parsePipes <|> parseParenthesis <|>  parseBasicFilters)

    ns <- many (do
        _ <-symbol ","
        (do 
            k <- parseFilter <|> (
              do
              _ <- many (char '\"')
              a <- identifier  
              _ <- many (char '\"')
              return (JStringConstructor (JString a))
              ) 
            _ <- many (char '\"')
            v <- many (do 
              _ <- char ':'
              parsePipes <|> parseParenthesis <|>  parseBasicFilters)
            return (k, headNoExceptions v)
            )
        )
    return ((key, headNoExceptions value):ns)
    )
  _ <- symbol "}"
  return (CurlyBrackets (concat ls))

-- parseEmptyCurlyBrackets :: Parser Filter
-- parseEmptyCurlyBrackets = do
--   _ <- many (char '\"')
--   k1 <- identifier
--   _ <- many (char '\"')
--   v1 <- many (do 
--     _ <- char ':'
--     parseJSONFilter)
--   xs <- many (do
--     _ <- char '\"'
--     _ <- many (char '\"')
--     kn <- identifier
--     _ <- many (char '\"')
--     vn <- many (do 
--       _ <- char ':'
--       parseJSONFilter)
--     return (kn, headNoExceptions vn))
    
--   return (EmptyCurlyBrackets ((k1,headNoExceptions v1):xs))

parseEmptyCurlyBrackets :: Parser Filter 
parseEmptyCurlyBrackets = do
  _ <- char '{'
  _ <- many (char '\"')
  s <- identifier
  _ <- many (char '\"')
  _ <- char '}'
  
  return (EmptyCurlyBrackets s)

  


headNoExceptions :: [Filter] -> Filter  
headNoExceptions (x:_) = x
headNoExceptions [] = NullFilter  

parseImplication :: Parser Filter 
parseImplication = do
  _ <- symbol "if"
  a <- parseFilter
  _ <- symbol "then"
  b <- parseFilter
  _ <- symbol "else"
  c <- parseFilter
  _ <- symbol "end"
  return (Implication a b c)

parseNot :: Parser Filter 
parseNot = do
  _ <- symbol "not"
  return Not

parseComparison :: Parser Filter
parseComparison = parseGreater <|> parseGreaterOrEqual <|> parseLess <|> parseLessOrEqual <|> parseEquals <|> parseNotEquals <|> parseAnd <|> parseOr

parseFilter :: Parser Filter
parseFilter =  parseImplication <|> parseComparison<|> parsePipes  <|> parseSquareBrackets <|> parseParenthesis <|> parseComas  <|> parseBasicFilters

parseFilterForSB :: Parser Filter
parseFilterForSB =  parseImplication <|> parsePipes  <|> parseSquareBrackets  <|> parseParenthesis  <|> parseComparison <|> parseBasicFilters


parseFilterForPipes :: Parser Filter
parseFilterForPipes =  parseImplication <|>parseComas  <|> parseComparison <|> parseParenthesis <|> parseBasicFilters



parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        -- [] -> Left $ "parsed: " ++ show v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e

-------------JSON in filters
parseJArray2 :: Parser JSON
parseJArray2 = do 
    _ <- symbol "["
    
    ns <- many (
        do 
        n <- parseJSONFilter
        xs <- many (
            do
            _<-symbol "," 
            parseJSONFilter
            )
        return (n:xs)
        )
    _ <- symbol "]"
    return (JArray (concat ns))

parseJObject2 :: Parser JSON
parseJObject2 = do 
    _ <- symbol "{"
    ls <- many (do
        key <- identifier <|> (
          do
            _ <- many (char '\"')
            a <- stringIdentrifier 
            _ <- many (char '\"')
            return a

            )  

        _ <- symbol ":"

        value <- parseJSONFilter
        ns <- many (do
            _ <-symbol ","
            (do 
                k <- identifier <|> (
                  do
                    _ <- many (char '\"')
                    a <- stringIdentrifier 
                    _ <- many (char '\"')
                    return a
                  ) 
                _ <- symbol ":"
                v <- parseJSONFilter
                return (k,v)
                )
            )
        return (((key, value):ns))
        )
    _ <- symbol "}"
    return (JObject (concat ls))

parseJDot :: Parser JSON
parseJDot = do
    _ <- char '.'
    return JDot

parseJSONFilter :: Parser JSON
parseJSONFilter = token $ parseJNull <|> parseJFalse <|> parseJTrue <|> parseJNumber <|> parseJString <|> parseJArray2 <|> parseJObject2 <|> parseJDot


