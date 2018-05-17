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
    | Comment String
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
    
comment' = accept "--" -# (iter char') #- require "\n" >-> buildComment
buildComment s = Comment s
    
begin' = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin ss = Begin ss
    
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] dict input = input

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
    
exec (Comment s : stmts) dict input = exec stmts dict input

indent n = replicate n '\t'

shw :: Int -> Statement -> String
shw n (Assignment name expr)        = indent n ++ name ++ " := " ++ Expr.toString expr ++ ";\n"
shw n (If cond thenStmts elseStmts) = indent n ++ "if " ++ Expr.toString cond ++ " then\n" ++ shw (n+1) thenStmts ++ indent n ++ "else\n" ++ shw (n+1) elseStmts  
shw n (While cond doStmt)           = indent n ++ "while " ++ Expr.toString cond ++ " do\n" ++ shw (n+1) doStmt
shw n (Read v)                      = indent n ++ "read " ++ v ++ ";\n"
shw n (Write expr)                  = indent n ++ "write " ++ Expr.toString expr ++ ";\n"
shw n (Begin stmt)                  = indent n ++ "begin\n" ++ concat (map (shw (n+1)) stmt) ++ indent n ++ "end\n" 
shw n (Skip)                        = indent n ++ "skip;\n"
shw n (Comment s)                   = indent n ++ "--" ++ s ++ "\n"

instance Parse Statement where
    parse = assignment ! if' ! while' ! read' ! write' ! skip' ! begin' ! comment'
    toString = shw 0