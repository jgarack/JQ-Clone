module Jq.Json where

-- JSON.hs contains a datatype JSON to represent JSON data

import Numeric
data JSON = JNull
          | JEmpty 
          | JNumber Double
          | JString String
          | JBool Bool
          | JArray [JSON]
          | JObject [(String, JSON)]
          | JDot

instance Eq JSON where
  (==) JNull JNull = True 
  (==) JEmpty JEmpty = True 
  (==) JDot JDot = True
  (==) (JBool b1) (JBool b2) = b1 == b2
  (==) (JNumber n1) (JNumber n2) = n1 == n2
  (==) (JString s1) (JString s2) = s1 == s2
  (==) (JObject xs) (JObject ys) = xs ==ys
  (==) (JArray xs) (JArray ys) = xs == ys
  (==) _ _ = False 

instance Ord JSON where
  compare (JBool b1) (JBool b2) = compare b1 b2
  compare (JNumber n1) (JNumber n2) = compare n1 n2
  compare (JString s1) (JString s2) = compare s1 s2
  compare (JObject xs) (JObject ys) = compare xs ys
  compare (JArray xs) (JArray ys) = compare xs ys
  compare _ _ = compare True True

instance Show JSON where
  show x = printJSON 0 x


printJSON :: Int -> JSON -> String 
printJSON identation JEmpty       = ""
printJSON identation JDot         = ".123"
printJSON identation JNull        = "null"
printJSON idenration (JNumber d)  = printRemainder d
printJSON identation (JBool b)    = if b then "true" else "false"
printJSON identation (JString s)  = (showString [] ("\""++s++"\""))
printJSON identation (JArray [])  = "[]"
printJSON identation (JObject []) = "{}"
printJSON identation (JArray xs)  = "[\n" ++ printIdentation (identation+1) ++ printJArray identation xs
printJSON identation (JObject xs) = "{\n" ++ printIdentation (identation+1) ++ printJObject (identation) xs


printIdentation :: Int -> String
printIdentation id = if id <= 0 then "" else "  " ++ printIdentation (id-1)

printRemainder :: Double -> String 
printRemainder d = if d -  fromInteger (floor d) == 0.0 then showFFloat (Just 0) d "" else show d

printJArray :: Int -> [JSON] -> String
printJArray id []      = "]";
printJArray id (v:[])  = printJSON (id+1) v ++ "\n"++printIdentation (id) ++"]"
printJArray id (v:xs)  = printJSON (id+1) v ++ ",\n" ++ printIdentation (id+1) ++ printJArray (id) xs


printJObject :: Int -> [(String, JSON)] -> String
printJObject id []         = "}"
printJObject id ((s,v):[]) = "\""++(showString [] s)++"\"" ++ ": " ++ printJSON (id+1) v ++"\n"++ printIdentation (id) ++"}"
printJObject id ((s,v):xs) = "\""++(showString [] s)++"\"" ++ ": " ++ printJSON (id+1) v ++ ",\n" ++ printIdentation (id+1) ++ printJObject id xs
