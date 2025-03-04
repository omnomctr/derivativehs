module Main where

import Expr (Expr (..), z, q, rq, Number (Z), powIntegral)

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
derive (Div lhs rhs) = 
    let lhs' = derive lhs 
        rhs' = derive rhs 
    in ((lhs' `Mult` rhs) `Sub` (lhs `Mult` rhs')) `Div` (rhs `Pow` Constant 2)
derive x = error $ show x ++ " is not implemented"

reduce :: Expr -> Expr 
reduce (Pow base exp) = 
    case (reduce base, reduce exp) of 
        (_, Constant 0) -> Constant 1
        -- we only evaluate it if its a rational
        (Constant base, Constant (Z exp)) -> Constant $ base ^ exp
        (base, Constant 1) -> base 
        (base, exp) -> base `Pow` exp
reduce (Mult lhs rhs) = 
    case (reduce lhs, reduce rhs) of 
        -- we evaluate constant expressions
        (Constant x, Constant y) -> Constant $ x * y
        (lhs, Constant 1) -> lhs
        (Constant 1, rhs) -> rhs 
        (lhs, rhs) -> lhs `Mult` rhs
reduce (Add lhs rhs) =
    case (reduce lhs, reduce rhs) of 
        (Constant x, Constant y) -> Constant $ x + y
        (Constant 0, rhs) -> rhs
        (lhs, Constant 0) -> lhs
        (lhs, rhs) -> lhs `Add` rhs
reduce (Sub lhs rhs) 
    | lhs == rhs = Constant 0
    | otherwise = case (reduce lhs, reduce rhs) of
        (Constant x, Constant y) -> Constant $ x - y
        (lhs, Constant 0) -> lhs 
        (lhs, rhs) -> lhs `Sub` rhs
reduce (Div lhs rhs)
    | lhs == rhs = Constant 1
    | otherwise = case (reduce lhs, reduce rhs) of 
        (Constant num, Constant denom) -> Constant $ num / denom
        (num, Constant 1) -> num 
        (num, denom) -> num `Div` denom
reduce x = x

main :: IO ()
main = do
    print $ q 1 2 + 2
    print $ q 4 2
    print $ q 4 2 + z 4
    let expr = (X `Add` (X `Pow` Constant 3)) `Pow` Constant 2 in do
        putStrLn ("original expression: " ++ show expr)
        putStrLn ("derivative: " ++ (show . derive) expr)
        putStrLn ("reduced derivative: " ++ (show . reduce . derive) expr)
