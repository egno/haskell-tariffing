module Price.Volume where

import           Price.DivisibleClass (Divisible (..), Fraction (..))
import           Price.Unit           (Unit (..))

data Volume = Volume Unit Float
    deriving (Show, Eq, Ord)

instance Divisible Volume where
    fraction :: Volume -> Volume -> Maybe Fraction
    fraction (Volume u1 k1) (Volume u2 k2)
        | u1 /= u2 = Nothing
        | otherwise = Just  $ Fraction (k1 / k2)

    multiply :: Volume -> Fraction -> Volume
    multiply (Volume u k) (Fraction p) = Volume u (k * p)

    add :: Volume -> Volume -> Maybe Volume
    add (Volume u1 k1) (Volume u2 k2)
        | u1 /= u2 = Nothing
        | otherwise = Just $ Volume u1 (k1 + k2)
