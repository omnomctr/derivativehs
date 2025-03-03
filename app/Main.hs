module Main where

import Ast (Expr (..))

derive :: Expr -> Expr
derive (Constant _) = Constant 0
derive X = Constant 1
derive (Mult lhs rhs) =
    let lhs' = derive lhs 
        rhs' = derive rhs 
    in (lhs' `Mult` rhs) `Add` (lhs `Mult` rhs')
derive (Add lhs rhs) = derive lhs `Add` derive rhs
derive (Sub lhs rhs) = derive lhs `Sub` derive rhs
derive (Pow base c@(Constant x)) = 
    Mult c (Mult 
        (Pow base (Constant $ x - 1))
        (derive base))
derive x = error $ show x ++ " is not implemented"

factor :: Expr -> Expr 
factor (Pow base exp) = 
    case (factor base, factor exp) of 
        (_, Constant 0) -> Constant 1
        -- we evaluate constant expressions
        (Constant base, Constant exp) -> Constant $ base ** exp
        (base, Constant 1) -> base 
        (base, exp) -> base `Pow` exp
factor (Mult lhs rhs) = 
    case (factor lhs, factor rhs) of 
        (Constant x, Constant y) -> Constant $ x * y
        (lhs, Constant 1) -> lhs
        (Constant 1, rhs) -> rhs 
        (lhs, rhs) -> lhs `Mult` rhs
factor (Add lhs rhs) =
    case (factor lhs, factor rhs) of 
        (Constant x, Constant y) -> Constant $ x + y
        (Constant 0, rhs) -> rhs
        (lhs, Constant 0) -> lhs
        (lhs, rhs) -> lhs `Add` rhs
factor (Sub lhs rhs) 
    | lhs == rhs = Constant 0
    | otherwise = case (factor lhs, factor rhs) of
        (Constant x, Constant y) -> Constant $ x - y
        (lhs, Constant 0) -> lhs 
        (lhs, rhs) -> lhs `Sub` rhs
factor x = x

main :: IO ()
main =
    let expr = (X `Add` (X `Pow` Constant 3)) `Pow` Constant 2 in do
        putStrLn ("original expression: " ++ show expr)
        putStrLn ("derivative: " ++ (show . derive) expr)
        putStrLn ("factored derivative: " ++ (show . factor . derive) expr)
