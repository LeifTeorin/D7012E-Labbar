module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show -- to be defined


instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program s) = "Begin Program\n" ++ concatMap Statement.toString s
             
exec (Program s) = Statement.exec s Dictionary.empty -- we start with an empty environment
