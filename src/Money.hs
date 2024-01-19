module Money where

import DivisibleClass (Divisible (..))

data Money = Money String Float
    deriving (Show, Eq, Ord)

instance Divisible Money where

    fraction (Money c1 k1) (Money c2 k2)
        | c1 /= c2 = Nothing
        | otherwise = Just (k1 / k2)

    multiply (Money c k) p = Money c (k * p)

    add (Money c1 k1) (Money c2 k2)
        | c1 /= c2 = Nothing
        | otherwise = Just $ Money c1 (k1 + k2)