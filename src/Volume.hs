module Volume where

import           DivisibleClass (Divisible (..))
import           Unit           (Unit (..))

data Volume = Volume Unit Float
    deriving (Show, Eq, Ord)

instance Divisible Volume where
    fraction :: Volume -> Volume -> Maybe Float
    fraction (Volume u1 k1) (Volume u2 k2)
        | u1 /= u2 = Nothing
        | otherwise = Just (k1 / k2)

    multiply :: Volume -> Float -> Volume
    multiply (Volume u k) p = Volume u (k * p)

    add :: Volume -> Volume -> Maybe Volume
    add (Volume u1 k1) (Volume u2 k2)
        | u1 /= u2 = Nothing
        | otherwise = Just $ Volume u1 (k1 + k2)
