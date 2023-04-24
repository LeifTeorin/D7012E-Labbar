module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import qualified Statement
import qualified Statement as String
type T = Statement

data Statement = 
    Assignment String Expr.T -- assign a value to a variable
    | If Expr.T Statement Statement -- if a condition is true we run one or more statements
    | While Expr.T Statement -- while a condition is true, repeat one or more statements
    | Seq [Statement] -- takes a list of statements and represents a sequence of statements to be executed in order.
    | Skip -- represents an empty statement, typically used as a placeholder or a no-op.
    | Print Expr.T -- takes an expression to be printed as output.
    | Read String -- takes a string representing a variable name to be read as input.
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e

if_ :: Parser Statement
if_ = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse  >-> buildIf_

buildIf_ :: ((Expr.T, Statement), Statement) -> Statement
buildIf_ ((c, t), e) = If c t e

while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile

buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (c, d) = While c d

seq :: Parser Statement
seq = require "begin" -# iter parse #- require "end" >-> buildSeq

buildSeq :: [Statement] -> Statement
buildSeq = Seq 

skip :: Parser Statement
skip = accept "skip" # require ";" >-> buildSkip

buildSkip :: a -> Statement
buildSkip _ = Skip

print :: Parser Statement
print = accept "print" -# Expr.parse #- require ";" >-> buildPrint

buildPrint :: Expr.T -> Statement
buildPrint = Print

read :: Parser Statement
read = accept "read" -# word #- require ";" >-> buildRead

buildRead :: String -> Statement
buildRead s = Read s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
