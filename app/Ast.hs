module Ast (Expr (..)) where

data Expr
    = Pow Expr Expr
    | Constant Double
    | Mult Expr Expr
    | X
    | Add Expr Expr
    | Sub Expr Expr
    deriving (Eq)

instance Show Expr where 
    show (Constant x) = show x
    show X = "x"
    show (Mult lhs rhs) = "(" ++ show lhs ++ ")(" ++ show rhs ++ ")"
    show (Pow base expr) = "(" ++ show base ++ "^" ++ show expr ++")"
    show (Add lhs rhs) = "(" ++ show lhs ++ " + " ++ show rhs ++ ")"
    show (Sub lhs rhs) = "(" ++ show lhs ++ " - " ++ show rhs ++ ")"