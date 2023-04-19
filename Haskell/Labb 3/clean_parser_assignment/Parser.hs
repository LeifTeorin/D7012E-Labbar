module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
import Distribution.Compat.CharParsing (CharParsing(string))
import Distribution.Fields.LexerMonad (LexState(warnings))
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

--Exercise 1
semicolon :: Parser Char
semicolon (';':skip) = Just(';', skip)
semicolon skip = Nothing

--Exercise 2
becomes :: Parser String
becomes ('=' : skip) = Just("=", skip)
becomes _ = Nothing

--Exercise 3
char' :: Parser Char
char' (c:rest) = Just(c,rest)
char' []= Nothing

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
(m -# n) cs = 
    case n cs of
    Nothing -> Nothing
    Just(b, cs) -> Just(b, cs) 

(#-) :: Parser a -> Parser b -> Parser a
(m #- n) cs = 
    case m cs of
    Nothing -> Nothing
    Just(a, cs) -> Just(a, cs)

spaces :: Parser String
spaces = iter $ (?) char isSpace

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char
letter = (?) char isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars n = iterate' char n
    where
        iterate' m 0 = return []
        iterate' m n = m # iterate' m (n-1) >-> cons 

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require w  = accept w ! err warning
    where
        warning = "wrong" ++ w

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

