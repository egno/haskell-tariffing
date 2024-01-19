module Price where

import Data.Maybe (mapMaybe)
import DivisibleClass (Divisible (..))
import Metric (Metric (..))
import Money (Money (..))
import Unit (Unit (..))

data Volume = Volume Unit Float
    deriving (Show, Eq, Ord)

instance Divisible Volume where
    fraction (Volume u1 k1) (Volume u2 k2)
        | u1 /= u2 = Nothing
        | otherwise = Just (k1 / k2)

    multiply (Volume u k) p = Volume u (k * p)

    add (Volume u1 k1) (Volume u2 k2)
        | u1 /= u2 = Nothing
        | otherwise = Just $ Volume u1 (k1 + k2)

data Consumption = Consumption {metric :: Metric, volume :: Volume}
    deriving (Show, Eq, Ord)

instance Divisible Consumption where
    fraction (Consumption m1 v1) (Consumption m2 v2)
        | m1 /= m2 = Nothing
        | otherwise = fraction v1 v2

    multiply (Consumption c k) p = Consumption c (multiply k p)

    add (Consumption m1 v1) (Consumption m2 v2)
        | m1 /= m2 = Nothing
        | otherwise = consumption (add v1 v2)
      where
        consumption Nothing = Nothing
        consumption (Just v) = Just $ Consumption m1 v

data Price = Price {consumption :: Consumption, money :: Money}
    deriving (Show, Eq, Ord)

consumptionPrice :: Consumption -> Price -> Maybe Price
consumptionPrice c1 (Price c2 m) = fractionalPrice $ fraction c1 c2
  where
    fractionalPrice Nothing = Nothing
    fractionalPrice (Just part) =
        Just $
            Price
                { consumption = c1
                , money = multiply m part
                }

calculatePrices :: Consumption -> [Price] -> [Price]
calculatePrices c = mapMaybe (consumptionPrice c)

currencyPrices :: String -> [Price] -> [Price]
currencyPrices c = filter (\(Price _ (Money c1 _)) -> c1 == c)

currencyMoney :: String -> [Price] -> Maybe Money
currencyMoney c ps = foldMoney $ currencyPrices c ps
  where
    foldMoney [] = Nothing
    foldMoney ps1 = foldr addMoney Nothing ps1
    addMoney (Price _ (Money _ k)) (Just (Money c1 ks)) = Just $ Money c1 (ks + k)
    addMoney (Price _ m) Nothing = Just m

-- examples
minute :: Unit
minute = Unit "min"

fullPriceList :: [Price]
fullPriceList =
    [ Price{consumption = Consumption (Metric "vcc") (Volume minute 60.0), money = Money "USD" 0.383}
    , Price{consumption = Consumption (Metric "bm") (Volume minute 60.0), money = Money "USD" 0.718}
    ]

prices :: [Price]
prices = calculatePrices (Consumption (Metric "vcc") (Volume minute (60.0 * 60.0 * 24.0) )) fullPriceList

totalMoney :: Maybe Money
totalMoney = currencyMoney "USD" prices
