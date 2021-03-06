{-# OPTIONS_GHC -w #-}
-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing.Parsing (module Parsing.Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)


string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = (
      do 
         y <- char '\\'
         _ <- char 'u'
         a <- some alphanum 
         return  [fst (head (readLitChar ('\\' : show ((read :: String -> Int) ("0x" ++ a)))))]
         )<|>(do 
   x  <- many (
      do
      _ <- char '\\'
      char '\"')
   y <- char '\\'
      <|>  alphanum <|> char '!' <|> char ',' <|> 
      char '#' <|> char '$' <|> char '%' <|> char '&' <|> 
      char '(' <|> char ')' <|> char '*' <|> char '+' <|> 
      char '`' <|> char ',' <|> char '/'  <|> char  ':' <|> 
      char ';' <|> char '<'  <|> char '=' <|> char '>' <|>
      char '?' <|> char '@' <|> char ']' <|> char '^'  <|> char '[' <|> char  ']' <|> char '_' <|> 
      char '{' <|> char '}'  <|> char '|' <|> char '~'  <|> 
      char ' ' <|> char '.'
   z  <- many (
      do
      _ <- char '\\'
      a <- char '\"'
      return a) 
   xs <- many ident
   return (x++[y]++z++(concat xs)))



identbasic :: Parser String
identbasic = do 
   x  <- lower
   xs <- many (alphanum <|> char '_')
   return (x:xs)



nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat


natDr :: Parser Double 
natDr = (do 
   xs <- some digit
   _ <- char '.'  
   ys <- some digit
   return (read (xs++"."++ys)))

natD :: Parser Double 
natD = (do 
   xs <- some digit
   return (read (xs)))

double :: Parser Double 
double =(do
      a <- natDr
      _ <- char 'E'
      sign <- char '+' <|> char '-'
      x <- nat
      return (case sign of
         '+' -> a * 10^x
         '-' -> a / 10^(x)
         _ -> error "not good number syntax"
         )
      ) <|> (do 
   _ <- char '-'
   n <- natDr <|> natD 
   return (-n)) <|>natDr <|> natD 
   



-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

stringIdentrifier :: Parser String
stringIdentrifier = token ident

identifier :: Parser String
identifier = token identbasic

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)
