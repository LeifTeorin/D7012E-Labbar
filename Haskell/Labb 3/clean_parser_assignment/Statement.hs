module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement

data Statement = 
    Assignment String Expr.T -- assign a value to a variable
    | If Expr.T Statement Statement -- if a condition is true we run one or more statements
    | While Expr.T Statement -- while a condition is true, repeat one or more statements
    | Seq [Statement] -- takes a list of statements and represents a sequence of statements to be executed in order.
    | Skip -- represents an empty statement, typically used as a placeholder or a no-op.
    | Write Expr.T -- takes an expression to be printed as output.
    | Read String -- takes a string representing a variable name to be read as input.
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e

if_ :: Parser Statement
if_ = accept "if" -# Expr.parse #- require "then" # parse #- accept "else" # parse  >-> buildIf_

buildIf_ :: ((Expr.T, Statement), Statement) -> Statement
buildIf_ ((c, t), e) = If c t e

while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile

buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (c, d) = While c d

seq :: Parser Statement
seq = accept "begin" -# iter parse #- require "end" >-> buildSeq

buildSeq :: [Statement] -> Statement
buildSeq = Seq 

skip :: Parser Statement
skip = accept "skip" # require ";" >-> buildSkip

buildSkip :: a -> Statement
buildSkip _ = Skip

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite

buildWrite :: Expr.T -> Statement
buildWrite = Write

read :: Parser Statement
read = accept "read" -# word #- require ";" >-> buildRead

buildRead :: String -> Statement
buildRead s = Read s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment var val: stms) dict input = exec stms (Dictionary.insert (var, Expr.value val dict) dict) input
exec (While cond do_: stms) dict input = if (Expr.value cond dict)>0 then exec (do_: While cond do_: stms) dict input else exec stms dict input
exec (Seq stmts: stms) dict input = exec stmts dict input ++ exec stms dict input
exec (Skip: stms) dict input = exec stms dict input
exec (Write e: stms) dict input = Expr.value e dict : exec stms dict input
exec (Read s: stms) dict input = exec stms (Dictionary.insert (s, head input) dict) (tail input)


instance Parse Statement where
    parse = assignment ! skip ! Statement.read ! Statement.write ! skip ! while ! Statement.seq ! if_
    toString (Assignment var val) = var ++ ":=" ++ Expr.toString val ++ ";\n" 
    toString (While cond do_) = "while" ++ Expr.toString cond ++ "do\n" ++ toString do_
    toString (If cond thenStmts elseStmts) = "if" ++ Expr.toString cond ++ "then\n" ++ toString thenStmts  ++ "else\n" ++ toString elseStmts
    toString (Seq stmts) = "seq\n" ++ concatMap toString stmts ++ "end\n"
    toString (Skip) = "skip;\n"
    toString (Write e) = "write" ++ Expr.toString e ++ ";\n"
    toString (Read s) = "read" ++ s ++ ";\n"