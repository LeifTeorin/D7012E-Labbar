module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import Language.Haskell.TH (Lit(IntegerL))

data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\ _ -> Mul) !
        lit '/' >-> (\ _ -> Div)

addOp = lit '+' >-> (\ _ -> Add) !
        lit '-' >-> (\ _ -> Sub)

bldOp e (oper,e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"
             
term' e = mulOp # factor >-> bldOp e #> term' ! return e
term = factor #> term'
       
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) env = n
value (Var v) env = case Dictionary.lookup v env of Just y -> y ; _ -> error (v ++ " undefined")
value (Add left right) env = value left env + value right env
value (Sub left right) env = value left env - value right env
value (Mul left right) env = value left env * value right env
value (Div left right) env = case value right env of
        0 -> error "oh no division by zero"
        _ -> div (value left env) (value right env) 

instance Parse Expr where
    parse = expr
    toString (Num num) = show num
    toString (Var str) = str
    toString (Add left right) = toString left ++ "+" ++ toString right
    toString (Sub left right) = toString left ++ "-" ++ toString right
    toString (Mul left right) = toString left ++ "*" ++ toString right
    toString (Div left right) = toString left ++ "/" ++ toString right
