module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =  Assignment String Expr.T
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Read String
    | Write Expr.T
    | Begin [Statement]
    | Skip
    | Comment
    deriving Show
    
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e
    
if' = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2
    
while' = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
buildWhile (e, s) = While e s
    
read' = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v
    
write' = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e
    
skip' = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip
    
comment' = accept "--" # require "\n" >-> buildComment
buildComment _ = Comment
    
begin' = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin ss = Begin ss
    
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
    
exec (Assignment name expr: stmts) dict input = exec (stmts) (Dictionary.insert (name, Expr.value expr dict) dict) input
    
exec (While cond doStmt : stmts) dict input
    | (Expr.value cond dict) > 0  = exec (doStmt : While cond doStmt : stmts) dict input  -- kan vara fel här (stmt vs stmt : stmts)
    | otherwise                   = exec stmts dict input
    
exec (Read v : stmts) dict (input:inputs) = exec stmts (Dictionary.insert (v, input) dict) inputs
    
exec (Write expr : stmts) dict input = exec stmts dict (input ++ [Expr.value expr dict])
    
exec (Begin stmt : stmts) dict input = exec (stmt ++ stmts) dict input
    
exec (Skip : stmts) dict input = exec stmts dict input
    
exec (Comment : stmts) dict input = exec stmts dict input
    
instance Parse Statement where
    parse = assignment ! if' ! while' ! read' ! write' ! skip' ! begin' ! comment' ! error "Parse error"
    toString = error "Statement.toString not implemented"