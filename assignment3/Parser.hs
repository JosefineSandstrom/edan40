module Parser(module CoreParser, T, digit, digitVal, char', chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

-- Error
err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

-- Same as char but fails when finding "\n"
char' :: Parser Char
char' []     = Nothing
char' ('\n':cs)   = Nothing
char' (c:cs) = Just (c, cs)

-- Iterate a parser as long as it succeeds, empty list is returned if recursion fails
iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

-- Applies two parsers in sequence as # but throws away the result from the first one
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- Applies two parsers in sequence as # but throws away the result from the second one
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- Decides if a character is a space
spaces :: Parser String
spaces =  iter (char ? isSpace)

-- Ignores any space before and after applying a parser for a token 
-- (token == elements in string)
token :: Parser a -> Parser a
token m = m #- spaces

-- Decides if a character is a letter
letter :: Parser Char
letter =  char ? isAlpha

-- Decides if first elements in string is a word
word :: Parser String
word = token (letter # iter letter >-> cons)

-- Accepts n characters of string
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons ! fail

-- Decides if w is the first element in string
accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- Accepts the same string input as 'accept w' but reports the missing string using err in case of failure.
require :: String -> Parser String
require w  = (token (chars (length w))) ? (==w) ! err "Missing string"

-- Decides if the given character is the the same as the one accepted by char.
lit :: Char -> Parser Char
lit c = token char ? (==c)

-- Decides if a character is a digit
digit :: Parser Char 
digit = char ? isDigit 

-- Transforms digit to type Int and then to type Num
digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

-- Returns n if no Integers at beginning of string, otherwise append Integers to n
number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n

-- Accepts the first digit with digitVal and sends it to number’, then removes any spaces.
number :: Parser Integer
number = token (digitVal #> number')

