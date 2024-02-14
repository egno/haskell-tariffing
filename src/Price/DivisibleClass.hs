module Price.DivisibleClass where

newtype Fraction = Fraction Float
    deriving (Show, Eq, Ord)

class Divisible a where
    fraction :: a -> a -> Maybe Fraction
    multiply :: a -> Fraction -> a
    add :: a -> a -> Maybe a
