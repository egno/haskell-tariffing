module Price where

import           Data.Maybe     (mapMaybe)
import           DivisibleClass (Divisible (..))
import           Metric         (Metric (..))
import           Money          (Money (..))
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

data Consumption = Consumption {metric :: Metric, volume :: Volume}
    deriving (Show, Eq, Ord)

instance Divisible Consumption where
    fraction :: Consumption -> Consumption -> Maybe Float
    fraction (Consumption m1 v1) (Consumption m2 v2)
        | m1 /= m2 = Nothing
        | otherwise = fraction v1 v2

    multiply :: Consumption -> Float -> Consumption
    multiply (Consumption c k) p = Consumption c (multiply k p)

    add :: Consumption -> Consumption -> Maybe Consumption
    add (Consumption m1 v1) (Consumption m2 v2)
        | m1 /= m2 = Nothing
        | otherwise = consumption (add v1 v2)
      where
        consumption Nothing  = Nothing
        consumption (Just v) = Just $ Consumption m1 v

data Price = Price {consumption :: Consumption, money :: Money}
    deriving (Show, Eq, Ord)

newtype Cost = Cost Price
    deriving (Show, Eq, Ord)

cost :: Consumption -> Price -> Maybe Cost
cost c1 (Price c2 m) = fractionalPrice $ fraction c1 c2
  where
    fractionalPrice Nothing = Nothing
    fractionalPrice (Just part) =
        Just $
            Cost Price
                { consumption = c1
                , money = multiply m part
                }


calculateCosts :: Consumption -> [Price] -> [Cost]
calculateCosts c = mapMaybe (cost c)

currencyCosts :: String -> [Cost] -> [Cost]
currencyCosts c = filter (\(Cost (Price _ (Money c1 _))) -> c1 == c)

currencyMoney :: String -> [Cost] -> Maybe Money
currencyMoney c ps = foldMoney $ currencyCosts c ps
  where
    foldMoney []  = Nothing
    foldMoney ps1 = foldr addMoney Nothing ps1
    addMoney (Cost (Price _ (Money _ k))) (Just (Money c1 ks)) = Just $ Money c1 (ks + k)
    addMoney (Cost (Price _ m)) Nothing = Just m
