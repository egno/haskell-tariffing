module Price.Consumption where

import           Price.DivisibleClass (Divisible (..), Fraction (..))
import           Price.Metric         (Metric (..))
import           Price.Volume         (Volume (..))

data Consumption = Consumption {metric :: Metric, volume :: Volume}
    deriving (Show, Eq, Ord)

instance Divisible Consumption where
    fraction :: Consumption -> Consumption -> Maybe Fraction
    fraction (Consumption m1 v1) (Consumption m2 v2)
        | m1 /= m2 = Nothing
        | otherwise = fraction v1 v2

    multiply :: Consumption -> Fraction -> Consumption
    multiply (Consumption c k) p = Consumption c (multiply k p)

    add :: Consumption -> Consumption -> Maybe Consumption
    add (Consumption m1 v1) (Consumption m2 v2)
        | m1 /= m2 = Nothing
        | otherwise = consumption (add v1 v2)
      where
        consumption Nothing  = Nothing
        consumption (Just v) = Just $ Consumption m1 v
