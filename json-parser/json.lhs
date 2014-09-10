> {-# LANGUAGE FlexibleInstances #-}

> module Json where 
> import Data.Char
> import Text.ParserCombinators.Parsec

The JavaScript Object Notation (JSON) is a lightweight data-interchange 
format. A JSON value is either a string, or a number, or an object, or an
array, or true or false or null, see http://www.json.org. In Haskell, 
JSON values can be represented using the following datatype. 
A JSON object is an unordered set of name / value pairs, represented by 
a Haskell list of string / value pairs; an array is an ordered collection of 
values, again represented by a Haskell list.

> data JValue
>   = JString String             -- a string
>   | JNumber Double             -- a number
>   | JObject [(String,JValue)]  -- an unordered set of name / value pairs
>   | JArray [JValue]            -- an ordered collection of values
>   | JTrue                      -- true
>   | JFalse                     -- false
>   | JNull                      -- null
>   deriving(Show)

> {- ########################################## -}

      2.) JValue to define Java objects

> {- ########################################## -}

> theHobbit :: JValue
> theHobbit = JObject [name,isRented,chapters]
>   where name     = ("name",JString "TheHobbit")
>         isRented = ("isRented",JFalse)
>         chapters = ("chapters",JArray [ch1,ch2])
>         ch1      = JObject [ch1Num,ch1Len,ch1Sta]
>         ch1Num   = ("number",JNumber 1)
>         ch1Len   = ("length",JNumber 5.30) 
>         ch1Sta   = ("start",JNumber 0)
>         ch2      = JObject [ch2Num,ch2Len,ch2Sta]
>         ch2Num   = ("number",JNumber 2)
>         ch2Len   = ("length",JNumber 12)
>         ch2Sta   = ("start",JNumber 5.30)
>

> {- ########################################## -}

      3.) JSON value to textual representation

> {- ########################################## -}

> jprint :: JValue -> String
> jprint (JString s)     = show $ jprintString s
> jprint (JNumber n)     = show n
> jprint obj@(JObject o) = "{" ++ jprintObject obj
> jprint arr@(JArray a)  = "[" ++ jprintArray arr
> jprint JTrue           = "true"
> jprint JFalse          = "false"
> jprint JNull           = "null"

> jprintString :: String -> String
> jprintString []          = []
> jprintString ('\\':c:cs) 
>   | c == 'u'  = hexEscape (take 4 cs) : jprintString (drop 4 cs) 
>   | otherwise = escape c : jprintString cs
> jprintString (c:cs)      = c : jprintString cs  
>  
> escape :: Char -> Char
> escape x = case x of
>                '"' -> '"'
>                '/' -> '/'
>                'b' -> '\b'
>                'f' -> '\f' 
>                'n' -> '\n' 
>                'r' -> '\r' 
>                't' -> '\t' 
>
> hexEscape :: String -> Char
> hexEscape h = chr $ foldl (\x y -> (16 * x) + y) 0 (map digitToInt h)
>
> jprintObject :: JValue -> String
> jprintObject (JObject [])     = "}"
> jprintObject (JObject [o])    = (jprintSingleO o) ++ "}" 
> jprintObject (JObject (o:os)) = (jprintSingleO o) ++ "," ++ recCall                 
>   where recCall = jprintObject (JObject os)
>
> jprintSingleO :: (String,JValue) -> String
> jprintSingleO obj = show (fst obj) ++ ":" ++ jprint (snd obj)
>
> jprintArray :: JValue -> String
> jprintArray (JArray [])     = "]"
> jprintArray (JArray [a])    = jprint a ++ "]" 
> jprintArray (JArray (a:as)) = jprint a ++ "," ++ jprintArray (JArray as)


> {- ########################################## -}

       4.) JPretty pretty printer function

> {- ########################################## -}

> personMario  = JObject [fstName,lstName,address] 
> fstName = ("firstname",JString "Mario")
> lstName = ("lastname",JString "Pirker")
> address = ("address", JObject [town,zipCode])
> town    = ("town",JString "Innsbruck")
> zipCode = ("zip",JString "6020")
>
> arrayPretty = JArray [personMario,JNull]
> arrayPretty' = JArray [arrayPretty,personMario]

jpretty' is actually a wrapper function that replaces \n with newline and prints it.

> jpretty' :: JValue -> IO ()
> jpretty' jval = putStrLn $ jpretty jval

jpretty is called by jpretty' with the initial JSON value. The initial indentation is 
0 (first parameter of jprettyPrint function call).

> jpretty :: JValue -> String
> jpretty jval = jprettyPrint 0 jval

jprettyPrint does the work and performs a case distinction based on the input value.
The first parameter of the function is the amount of indentation. 

> jprettyPrint :: Int -> JValue -> String
> jprettyPrint i str@(JString s) = jprint str
> jprettyPrint i num@(JNumber n) = jprint num
> jprettyPrint i obj@(JObject o) = "{ " ++ jprettyObject i obj
> jprettyPrint i arr@(JArray a)  = "[ " ++ jprettyArray i arr
> jprettyPrint i JTrue           = jprint JTrue
> jprettyPrint i JFalse          = jprint JFalse
> jprettyPrint i JNull           = jprint JNull

jprettyObject pretty prints a JSON object.

> jprettyObject :: Int -> JValue -> String
> jprettyObject i (JObject [])     = "}"
> jprettyObject i (JObject [o])    = jprettySingleO i o ++ " }"
> jprettyObject i (JObject (o:os)) = jprettySingleO i o ++ "\n" ++ recCall
>   where recCall = (replicate i ' ') ++ ", " ++  jprettyObject i (JObject os)

jprettySingle prints out one object and calls jprettyPrint recursive with a 
new indentation counter. Why do we addd 6 to the length of the string?
2 characters are needed for the opening bracket and one space
2 characters are needed for the colon and one space
2 characters are needed for the comma and one space

> jprettySingleO :: Int -> (String,JValue) -> String
> jprettySingleO i obj = show (fst obj) ++ ": " ++ jprettyPrint (i+len) (snd obj)
>   where len = length (fst obj) + 6

> jprettyArray :: Int -> JValue -> String
> jprettyArray i (JArray [])     = "]"
> jprettyArray i (JArray [a])    = jprettyPrint (i+2) a ++ " ]"
> jprettyArray i (JArray (a:as)) = jprettyPrint (i+2) a ++ recCall
>   where recCall = "\n" ++ (replicate i ' ') ++ ", " ++ jprettyArray i (JArray as)

> {- ########################################## -}

       5.) jlookup function

> {- ########################################## -}

> personTest  = JObject [fstName1,lstName1,address1]
> fstName1    = ("firstname",JString "Max")
> lstName1    = ("lastname",JString "Mustermann")
> address1    = ("address", JObject [town1,zipCode1])
> town1       = ("town",JString "Innsbruck")
> zipCode1    = ("zip",JString "6020")
> personTest2 = JObject [fstName2,lstName2,address2]
> fstName2    = ("firstname",JString "Angela")
> lstName2    = ("lastname",JString "Merkel")
> address2    = ("address", JObject [town2,zipCode2])
> town2       = ("town",JString "Berlin")
> zipCode2    = ("zip2",JString "10115")
> 
> arrayTest = JArray [personTest,personTest2]
> 
> jlookup :: JValue -> String -> Maybe JValue
> jlookup (JObject [o]) s 
>   | isJObject (snd o) = if (fst o == s) 
>                         then Just (snd o) 
>                         else jlookup (snd o) s
>   | otherwise         = if (fst o == s) 
>                         then Just (snd o) 
>                         else Nothing
> jlookup (JObject (o:os)) s = if (fst o == s) 
>                              then Just (snd o) 
>                              else jlookup (JObject os) s
> jlookup (JArray [a]) s = if (isJObject a) 
>                          then jlookup a s
>                          else Nothing    
> jlookup (JArray (a:as)) s   
>   | isJObject a = case (isNothing (jlookup a s)) of 
>                     True   -> jlookup (JArray as) s
>                     False  -> jlookup a s
>   | otherwise   = jlookup (JArray as) s
> jlookup _ s               = Nothing 
> 
> isJObject :: JValue -> Bool
> isJObject (JObject o) = True
> isJObject _           = False
> 
> isNothing :: Maybe JValue -> Bool
> isNothing Nothing = True
> isNothing _       = False 
 

> {- ########################################## -}

      6.) Haskell -> JSON values

> {- ########################################## -}

> data Person = P {firstname::String,lastname::String,age::Int}
>
> jperson :: Person -> JValue
> jperson (P fn ln a) = JObject [firstname,lastname,age]
>  where firstname = ("firstname",JString fn)
>        lastname  = ("lastname",JString ln)
>        age       = ("age",JNumber (fromIntegral a))
> 
> jstring :: String -> JValue 
> jstring str = JString str
> 
> jstrings :: [String] -> JValue
> jstrings []     = JArray []
> jstrings [s]    = addToJArray (JArray []) (jstring s)
> jstrings (x:xs) = addToJArray (jstrings xs) (jstring x)
> 
> jnumber :: Double -> JValue
> jnumber num = JNumber num
> 
> jnumbers :: [Double] -> JValue
> jnumbers []     = JArray []
> jnumbers [n]    = addToJArray (JArray []) (jnumber n)
> jnumbers (n:ns) = addToJArray (jnumbers ns) (jnumber n)
> 
> jnumberss :: [[Double]] -> JValue
> jnumberss [[]]        = JArray [JArray []]
> jnumberss [ls@(n:ns)] = JArray [jnumbers ls]
> jnumberss (n:ns)      = addToJArray (jnumberss ns) (jnumbers n)
> 
> addToJArray :: JValue -> JValue -> JValue 
> addToJArray (JArray arr) val = JArray (val:arr)

> {- ########################################## -}

      7.) Type Class Haskell -> JSON values

> {- ########################################## -}

> class HaskellToJSON a where
>  toJSON :: a -> JValue
> 
> instance HaskellToJSON Person where 
>  toJSON p = jperson p
> 
> instance HaskellToJSON String where
>  toJSON s = jstring s
> 
> instance HaskellToJSON Double where 
>  toJSON d = jnumber d
> 
> instance HaskellToJSON [String] where 
>  toJSON xs = jstrings xs
> 
> instance HaskellToJSON [Double] where 
>  toJSON ds = jnumbers ds 
> 
> instance HaskellToJSON [[Double]] where 
>  toJSON dss = jnumberss dss 
   

> {- ########################################## -}

      8.) Instance Declaration Eq JValue

> {- ########################################## -}

> instance Eq JValue where 
>  (JString a) == (JString b) = (a == b)
>  (JNumber a) == (JNumber b) = (a == b)
>  JTrue == JTrue             = True
>  JTrue == _                 = False
>  JFalse == JFalse           = True
>  JFalse == _                = False
>  JNull == JNull             = True
>  JNull == _                 = False
>  JArray a == JArray b       = a == b
>  (JObject a) == (JObject b) = objEqu (JObject a) (JObject b)
>   where objEqu (JObject []) (JObject [])         = True
>         objEqu (JObject (x:xs)) oys@(JObject ys) = 
>              case (jlookup oys (fst x)) of 
>               Nothing   -> False
>               Just val  -> objEqu' (JObject xs) oys
>         objEqu _ _                               = False
>         objEqu' (JObject []) _                    = True
>         objEqu' oxs@(JObject xs) oys@(JObject ys) = objEqu oxs oys
