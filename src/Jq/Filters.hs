module Jq.Filters where
import Jq.Json
  
--Filters.hs contains a datatype Filter to represent jq filters
data Filter = Identity
            | RecursiveDescent
            | NullFilter
            | ObjectTransfer String
            | ObjectIndexing String Filter
            | OptionalObjectIndexing String Filter
            | ArrayIndex Int            
            | ArraySlicing Int Int
            | OptionalArrayIndex Int            
            | OptionalArraySlicing Int Int
            | Pipe Filter Filter
            | Coma Filter Filter
            | Iterator [Int]
            | OptionalIterator [Int]
            | ObjectIterator [Int]
            | Parenthesis [Filter]
            | JArrayConstructor JSON
            | JObjectConstructor JSON
            | JNumberConstructor JSON
            | JBooleanConstructor JSON
            | JStringConstructor JSON
            | JNullConstructor JSON
            | SquareBrackets [Filter]
            | CurlyBrackets [(Filter,  Filter)]
            | Equals Filter Filter
            | NotEquals Filter Filter
            | Implication Filter Filter Filter
            | Not
            | Greater Filter Filter
            | GreaterOrEqual Filter Filter
            | Less Filter Filter
            | LessOrEqual Filter Filter
            | And Filter Filter
            | Or Filter Filter
            | EmptyCurlyBrackets String
            | ArrayInObject Filter

instance Eq Filter where
  (==) NullFilter NullFilter = True 
  (==) _ _ = False  


instance Show Filter where
  show (Greater a b) = show a ++ ">" ++ show b
  show (GreaterOrEqual a b) = show a ++ ">=" ++ show b
  show (Less a b) = show a ++ "<" ++ show b
  show (LessOrEqual a b) = show a ++ "<=" ++ show b
  show Not = "not"
  show (And a b) = show a ++ " and " ++ show b
  show (Or a b) = show a ++ " or " ++ show b
  show (Implication a b c) = "if " ++ show a ++ " then " ++ show b ++ " else " ++ show c ++ " end"
  show (NotEquals a b) = show a ++ "!=" ++ show b
  show (Equals a b) = show a ++ "==" ++ show b
  show Identity = ".xd" 
  show RecursiveDescent = ".."
  show NullFilter = "null"
  show (ObjectTransfer x) = x
  show (ObjectIndexing a f) =  ".xd" ++ a ++ show f
  show (OptionalObjectIndexing a f) = ".xd" ++  a ++ "?" ++ show f
  show (ArrayInObject f) = "[]" ++ show f
  show (ArrayIndex i) = show i
  show (ArraySlicing i1 i2) = ".["++show i1 ++ ":" ++ show i2 ++ "]...."
  show (OptionalArrayIndex i1) = show i1 
  show (OptionalArraySlicing i1 i2) = ".["++show i1 ++ show i2 ++ "]?"
  show (Pipe f1 f2) = "("++show f1 ++ ") | (" ++ show f2++")"
  show (Coma f1 f2) = "(" ++ show f1 ++ "), (" ++ show f2 ++ ")"
  show (Parenthesis f1) =  "("++show f1++")" 
  show (JArrayConstructor x) = show x
  show (JObjectConstructor x) = show x
  show (JNumberConstructor x) = show x 
  show (JBooleanConstructor x) = show x
  show (JStringConstructor  x) = show x ++ "hahaha"
  show (JNullConstructor x) = show x
  show (Iterator x) = show x ++ "it"
  show (OptionalIterator x) = "[" ++ show x
  show (SquareBrackets x) =  show x 
  show (CurlyBrackets xs) = "{" ++ show xs ++ "}4"
  show x = show x


data Config = ConfigC {filters :: Filter}
