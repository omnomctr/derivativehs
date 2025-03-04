{-# Language InstanceSigs #-}
module Expr (Expr (..), Number (..), z, q, rq, powIntegral) where
import qualified Data.Ratio (denominator, numerator)

data Expr
    = Pow Expr Expr
    | Constant Number
    | Mult Expr Expr
    | X
    | Add Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    deriving (Eq)

instance Show Expr where 
    show (Constant x) = show x
    show X = "x"
    show (Mult lhs rhs) = "(" ++ show lhs ++ ")(" ++ show rhs ++ ")"
    show (Pow base expr) = "(" ++ show base ++ "^" ++ show expr ++")"
    show (Add lhs rhs) = "(" ++ show lhs ++ " + " ++ show rhs ++ ")"
    show (Sub lhs rhs) = "(" ++ show lhs ++ " - " ++ show rhs ++ ")"
    show (Div lhs rhs) = "(" ++ show lhs ++ " / " ++ show rhs ++ ")"

-- number type w/ support for real, rational and irational numbers
data Number
    = Z Int -- real number
    | Q Int Int -- Rational (fraction)
    | RQ Double -- Irational number (R - Q)
    deriving (Eq)

z :: Int -> Number 
z = Z

q :: Int -> Int -> Number 
q numerator 1 = z numerator
q numerator denominator = 
    if denominator' == 1 then z numerator' else Q numerator' denominator'
    where commonFactors = gcd numerator denominator
          numerator' = numerator `quot` commonFactors
          denominator' = denominator `quot` commonFactors

rq :: Double -> Number
rq = RQ

toIrrational :: Number -> Double
toIrrational (Z n) = fromIntegral n
toIrrational (Q x y) = fromIntegral x / fromIntegral y
toIrrational (RQ n) = n 

instance Num Number where 
    (*) :: Number -> Number -> Number
    (Z lhs) * (Z rhs) = Z $ lhs * rhs
    (Z lhs) * (Q numerator denominator) = Q (numerator * lhs) denominator
    (Q numerator denominator) * (Z rhs) = Q (numerator * rhs) denominator
    x * (RQ n) = RQ $ toIrrational x * n
    (RQ n) * x = RQ $ n * toIrrational x
    (Q lhsn lhsd) * (Q rhsn rhsd) = Q (lhsn * rhsn) (lhsd * rhsd)

    (+) :: Number -> Number -> Number
    (Z lhs) + (Z rhs) = Z $ lhs + rhs
    (RQ n) + rhs = RQ $ n + toIrrational rhs
    lhs + (RQ n) = RQ $ toIrrational lhs + n
    -- L/l + r = (L + Rl) / l
    (Q numerator denominator) + (Z rhs) = q (numerator + rhs * denominator) denominator
    (Z lhs) + (Q numerator denominator) = q (lhs * denominator + numerator) denominator
    (Q lhsn lhsd) + (Q rhsn rhsd) =
        -- L/l + R/r = (Lr + Rl) / rl
        q ((lhsn * rhsd) + (rhsn * lhsd)) (lhsd * rhsd)
    
    (-) :: Number -> Number -> Number
    (Z lhs) - (Z rhs) = Z $ lhs - rhs
    (RQ lhs) - rhs = RQ $ lhs - toIrrational rhs
    lhs - (RQ rhs) = RQ $ toIrrational lhs - rhs
    (Q lhsn lhsd) - (Z rhs) = q (lhsn - rhs * lhsd) lhsd
    (Z lhs) - (Q rhsn rhsd) = q (lhs * rhsd - rhsn) rhsd
    (Q lhsn lhsd) - (Q rhsn rhsd) =
        q ((lhsn * rhsd) + (rhsn * lhsd)) (lhsd * rhsd)

    abs :: Number -> Number
    abs (RQ n) = RQ $ abs n
    abs (Z n) = Z $ abs n 
    abs (Q num denom) = Q (abs num) (abs denom)
    
    signum :: Number -> Number
    signum (Z n) = Z $ signum n
    signum (RQ n) = RQ $ signum n
    signum (Q num denom) = q (signum num) (signum denom)
    
    fromInteger :: Integer -> Number
    fromInteger = Z . fromInteger

    negate :: Number -> Number
    negate = (*) (Z (-1))

instance Fractional Number where 
    (/) :: Number -> Number -> Number
    (Z lhs) / (Z rhs) = q lhs rhs
    (RQ lhs) / rhs = RQ $ lhs / toIrrational rhs
    lhs / (RQ rhs) = RQ $ toIrrational lhs / rhs
    (Q lhsn lhsd) / (Q rhsn rhsd) = 
        -- (L/l)/(R/r) = Lr / Rl
        q (lhsn * rhsd) (rhsn * lhsd)
    (Z lhs) / (Q rhsn rhsd) = 
        -- (L / (R/r)) = Lr/R
        q (rhsd * lhs) rhsn
    (Q lhsn lhsd) / (Z rhs) = 
        -- (L/l) / R = L / Rl
        q lhsn (lhsd * rhs)

    recip :: Number -> Number
    recip = (1/)

    fromRational :: Rational -> Number
    -- didnt know Rational exists when I made the Number type oops
    fromRational rat = q (fromIntegral $ Data.Ratio.numerator rat) (fromIntegral $ Data.Ratio.denominator rat)

instance Show Number where 
    show (Z n) = show n 
    show (Q num denom) = show num ++ "/" ++ show denom 
    show (RQ n) = show n