{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Compiler where
--Compiler.hs contains the function compile that transforms a Filter 
--into a function of type JSON -> Either String [JSON], that can be 
--executed on JSON values to produce either an error String or a list 
--of results
import Jq.Filters ( Filter(..) )
import           Jq.Json
import           Data.Either


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]

compile (ObjectIndexing _ _) (JObject []) = return [JNull]
compile (ObjectIndexing _ _) (JNull) = return [JNull]

compile (ObjectIndexing key (ArrayInObject next)) (JObject ((k,v):xs)) = case (key, next) of
    (k1, NullFilter) -> if k1 == k then compile (Iterator []) v else compile (ObjectIndexing key (ArrayInObject next)) (JObject xs)
    (k1, _) -> if k1 == k then  Right (concatMap (\l -> fromRight [] (compile next l)) (fromRight [] (compile (Iterator []) v)))  else compile (ObjectIndexing key (ArrayInObject next)) (JObject xs)

compile (ObjectIndexing key next) (JObject ((k,v):xs)) = case (key, next) of
    (k1, NullFilter) -> if k1 == k then return [v] else compile (ObjectIndexing key next) (JObject xs)
    (k1, _) -> if k1 == k then compile next v else compile (ObjectIndexing key next) (JObject xs)
compile (ObjectIndexing _ _) _ = Left "cannot index not-JObject"


compile (OptionalObjectIndexing _ _) (JObject []) = return [JNull]
compile (OptionalObjectIndexing key next) (JObject ((k,v):xs)) = case (key, next) of
    (k1, NullFilter) -> if k1 == k then return [v] else compile (OptionalObjectIndexing key next) (JObject xs)
    (k1, _) -> if k1 == k then compile next v else compile (OptionalObjectIndexing key next) (JObject xs)
compile (OptionalObjectIndexing _ _) _ = Right []



compile Not x = case x of
    JNull -> Right [(JBool True)]
    JBool True -> Right [(JBool False)]
    JBool False -> Right [(JBool True)]
    _ -> Right [(JBool False)]


compile (Implication a b c) x = case (fromRight [JNull] (compile a x)) of
    [JBool True] -> compile b x
    [JBool False] -> compile c x
    _ -> Left "condition evaluation needs to result in bool"

compile (ArrayIndex _) (JArray []) = return [JNull] 

compile (ArrayIndex a) (JArray (x:xs)) 
    | a < 0 = return [xs!!((length xs+a))]
    | otherwise = return [xs!!a]
compile (ArrayIndex _) (JNull) = return [JNull]

compile (ArrayIndex _) _ = Left "cant get index from not an array"

compile (Greater a b) x = Right (universalIterate (>) (fromRight [] (compile a x)) (fromRight [] (compile b x)))
compile (GreaterOrEqual a b) x = Right (universalIterate (>=) (fromRight [] (compile a x)) (fromRight [] (compile b x)))
compile (Less a b) x = Right (universalIterate (<) (fromRight [] (compile a x)) (fromRight [] (compile b x)))
compile (LessOrEqual a b) x = Right (universalIterate (<=) (fromRight [] (compile a x)) (fromRight [] (compile b x)))




compile (Parenthesis fs) (a) = Right (concatMap (\f -> fromRight [JNull] (compile f a)) fs)

compile (ArraySlicing a1 a2) (JArray []) = return [JArray []]
compile (ArraySlicing a1 a2) (JArray xs)
    | length xs <= a2 && a1 > 0 = return [JArray (drop a1 xs)]
    | length xs <= a2 || a1 < 0 = return [JNull]
    | otherwise = return [JArray (take (a2 - a1) (drop a1 xs))]
compile (ArraySlicing a1 a2) (JString xs) = return [JString (take (a2 - a1+1) (drop (a1) xs))]
compile (ArraySlicing _ _) JNull = return [JNull ]
compile (ArraySlicing _ _) _ = Left "cant slice a non-array"

compile (OptionalArraySlicing a1 a2) (JArray []) = return [JArray []]
compile (OptionalArraySlicing a1 a2) (JArray xs)
    | length xs <= a2 && a1 > 0 = return [JArray (drop a1 xs)]
    | length xs <= a2 || a1 < 0 = Right []
    | otherwise = return [JArray (take (a2 - a1) (drop a1 xs))]
compile (OptionalArraySlicing a1 a2) (JString xs) = return [JString (take (a2 - a1) (drop (a1) xs))]
compile (OptionalArraySlicing _ _) (JNull) = return [JNull]

compile (OptionalArraySlicing _ _) _ = Right []

compile (OptionalArrayIndex _) (JNull) = return [JNull]
compile (OptionalArrayIndex _) (JArray []) = return [JNull] 
compile (OptionalArrayIndex a) (JArray (xs)) 
    | a < 0 = return [xs!!((length xs -a))]
    | otherwise = return [xs!!a]

compile (OptionalArrayIndex _) _ = Right []


compile (Pipe f1 f2) a = Right (concat [fromRight [JNull] (compile f2 bx) | bx <- fromRight [JNull] (compile f1 a)])

compile (Coma f1 f2) a = Right (fromRight [JNull] (compile f1 a) ++ fromRight [JNull] (compile f2 a))

compile (OptionalIterator xs) (JArray js) = compile (Iterator xs) (JArray js)
compile (OptionalIterator xs) (JObject js) = compile (Iterator xs) (JObject js)

compile (OptionalIterator _) _ = Right []

compile (Iterator []) (JArray js) = Right js
compile (Iterator [x]) (JArray js) = Right [js!!x]
compile (Iterator (x:xs)) (JArray js) = Right ((js!!x) : fromRight [JNull] (compile (Iterator xs) (JArray js)))
compile (Iterator []) (JObject ls) = return (iterateObject ls)
compile (Iterator [x]) (JObject js) = Right [(getValue (js!!x))]
compile (Iterator (x:xs)) (JObject js) = 
    Right (getValue (js!!x) : fromRight [JNull] (compile (Iterator xs) (JObject js)))
compile (Iterator _) _ = Left "cannoot iterate over a non-bject ot non array"

compile (JNumberConstructor j) a = return [j]
compile (JBooleanConstructor j) a = return [j]
compile (JStringConstructor j) a = return [j]
compile (JNullConstructor j) a = return [j]

compile (JArrayConstructor (JArray (xs))) a = return [(JArray (containsDotJA xs a))]
    
compile (JObjectConstructor (JObject [])) a = return [JObject []]
compile (JObjectConstructor (JObject (xs))) a = return [JObject (containsDotJO xs a)]




compile (RecursiveDescent) (JArray (x:[])) = Right ([JArray [x]] ++ (fromRight [JNull] (compile RecursiveDescent x)))
compile (RecursiveDescent) (JArray (x:xs)) = Right ([JArray (x:xs)] ++ (fromRight [JNull] (compile RecursiveDescent x) ++ (recursiveDescArrays xs)))
compile (RecursiveDescent) (JObject ((k,v):[])) = Right ([(JObject [(k,v)])])
compile (RecursiveDescent) (JObject ((k,v):kvs)) = Right (([(JObject ((k,v):kvs))]) ++ (fromRight [JNull] (compile (Iterator []) (JObject ((k,v):kvs)))))
compile (RecursiveDescent) x = compile Identity x

compile (And a b) x = return (concat [andOperation x y| x<-(fromRight [JNull] (compile a x)), y<- (fromRight [JNull] (compile b x))])

compile (Or a b) x = return (concat [orOperation x y| x<-(fromRight [JNull] (compile a x)), y<- (fromRight [JNull] (compile b x))])

compile (EmptyCurlyBrackets s) (JObject []) = Right [JNull ]
compile (EmptyCurlyBrackets s) (JObject ((k,v):xs))
    | k == s = compile Identity (JObject [(k,v)])
    | otherwise = compile (EmptyCurlyBrackets s) (JObject xs)
compile (EmptyCurlyBrackets _) _ = Left "cant index this type"


compile (SquareBrackets []) a = Right [JArray []]
compile (SquareBrackets (f:fs)) (xs) = Right ([JArray (compileList (f:fs) xs)])


compile (CurlyBrackets []) xs = Right [JObject []] 
compile (CurlyBrackets kfs) xs = Right [JObject x | x <-compileObject [ 
    (case fromRight [] (compile y xs) of
        [(JString s)] -> s
        _ -> error "bad input"
    
        ,f)| (y,f)<-kfs] xs]

compile (Equals a b) x = Right (equalsIterate (fromRight [] (compile a x)) (fromRight [] (compile b x)))
compile (NotEquals a b) x = Right (notEqualsIterate (fromRight [] (compile a x)) (fromRight [] (compile b x)))

universalIterate :: (JSON->JSON->Bool )->[JSON ] -> [JSON ] -> [JSON ]
universalIterate _ [] _ = []
universalIterate _ _ [] = []
universalIterate f (x:xs) (y:ys) 
    | f x y = [JBool True] ++ universalIterate f (x:xs) ys ++ universalIterate f xs (y:ys) ++ universalIterate f xs ys
    | otherwise = [JBool False] ++ universalIterate f (x:xs) ys ++ universalIterate f xs (y:ys) ++ universalIterate f xs ys

notEqualsIterate :: [JSON ] -> [JSON ] -> [JSON ]
notEqualsIterate [] _ = []
notEqualsIterate _ [] = []
notEqualsIterate (x:xs) (y:ys) 
    | x==y = [JBool True] ++ notEqualsIterate (x:xs) ys ++ notEqualsIterate xs (y:ys) ++ notEqualsIterate xs ys
    | otherwise = [JBool False] ++ notEqualsIterate (x:xs) ys ++ notEqualsIterate xs (y:ys) ++ notEqualsIterate xs ys


equalsIterate :: [JSON ] -> [JSON ] -> [JSON ]
equalsIterate [] _ = []
equalsIterate _ [] = []
equalsIterate (x:xs) (y:ys) 
    | x/=y = [JBool True] ++ equalsIterate (x:xs) ys ++ equalsIterate xs (y:ys) ++ equalsIterate xs ys
    | otherwise = [JBool False] ++ equalsIterate (x:xs) ys ++ equalsIterate xs (y:ys) ++ equalsIterate xs ys

compileObject :: [(String, Filter)] -> JSON -> [[(String, JSON)]]
compileObject [] xs = [[]]
compileObject ((k,v):kfs) xs = case (v, xs) of
    (NullFilter, JObject ob) -> [(findByKey ob k)++y | y <- compileObject kfs xs]
    (NullFilter, JNull) -> [[(k,JNull )]++y | y <- compileObject kfs xs]
    (_, _) -> [[(k,x)]++y| y<-compileObject kfs xs, x<-(fromRight [JNull] (compile v xs))]


compileMultipleOutput :: [JSON] -> String -> [[(String,JSON)]]
compileMultipleOutput (x:[]) key = [[(key, x)]]
compileMultipleOutput (x:xs) key = [(key,x)] :compileMultipleOutput xs key

findByKey :: [(String, JSON)] -> String -> [(String, JSON)]
findByKey [] key = [(key,JNull)]
findByKey  ((k,v):xs)  key
    | k==key = [(k,v)]
    | otherwise = findByKey  xs key

compileList :: [Filter] -> JSON -> [JSON]
compileList [] x = []
compileList (f:fs) x = fromRight [] (compile f x) ++ compileList fs x


orHelper :: [JSON] -> [JSON] -> Either String [JSON] 
orHelper [] _ = Right []
orHelper _ [] = Right []
orHelper (x:xs) (y:ys) = Right ((orOperation x y) ++ (fromRight [] (orHelper xs (y:ys)) ++ (fromRight [] (orHelper (x:xs) ys))))

orOperation :: JSON -> JSON -> [JSON]
orOperation x y = case (x,y) of
    (JNull, JNull) -> [JBool False]
    (JEmpty, JEmpty) -> [JBool False]
    (JDot, JDot) -> [JBool True]
    (JBool b1, JBool b2) -> [JBool (b1||b2)]
    (_,_) -> [JBool True]

andOperation :: JSON -> JSON -> [JSON]
andOperation x y = case (x,y) of
    (JNull, _) -> [JBool False]
    (_, JNull) -> [JBool False]
    (JEmpty, JEmpty) -> [JBool False]
    (JDot, JDot) -> [JBool True]
    (JBool b1, JBool b2) -> [JBool (b1&&b2)]
    (JBool False, _) -> [JBool False]
    (_, JBool True) -> [JBool False] 
    (_,_) -> [JBool True]


recursiveDescArrays :: [JSON] -> [JSON]
recursiveDescArrays [] = []
recursiveDescArrays (x:xs) = fromRight [JNull] (compile RecursiveDescent x) ++ recursiveDescArrays xs

iterateForObject :: [(String, Filter)] -> JSON -> [(String, JSON)]
iterateForObject ((k,f):[]) a = [(k, (fromRight [JNull] (compile f a)!!0))]
iterateForObject ((k,f):kfs) a = (k, (fromRight [JNull] (compile f a)!!0)) : iterateForObject kfs a

-- create method that returns the same as compile, but add a flag to specify which time printed if first time print item otherwise only print insid of the item.
containsDotJO :: [(String,JSON)] -> JSON -> [(String, JSON)]
containsDotJO [] a = []
containsDotJO ((k,x):xs) a = case x of 
    JDot -> return (k,a) ++ containsDotJO xs a
    JArray ls -> return (k,(JArray (containsDotJA ls a))) ++ containsDotJO xs a
    JObject ls -> return (k, JObject (containsDotJO ls a)) ++ containsDotJO xs a
    _ -> return (k,x) ++ containsDotJO xs a


containsDotJA :: [JSON] -> JSON -> [JSON]
containsDotJA [] a = []
containsDotJA (x:xs) a = case x of 
    JDot -> return a ++ containsDotJA xs a
    JArray ls -> return (JArray (containsDotJA ls a)) ++ containsDotJA xs a
    JObject ls -> return (JObject (containsDotJO ls a)) ++ containsDotJA xs a
    _ -> return x ++ containsDotJA xs a
    
iterateObject :: [(String,JSON )] -> [JSON]
iterateObject [] = []
iterateObject ((k,v):rs) = [v] ++ iterateObject rs



getValue :: (String, JSON) -> JSON
getValue (k,v) = v 

getValues :: [(String, JSON)] -> [JSON]
getValues ((k,v):[]) = [v]
getValues ((k,v):js) = [v] ++ getValues js

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
