module Price where

import           Consumption    (Consumption)
import           Cost           (Cost(Cost))
import           Data.Maybe     (mapMaybe)
import           DivisibleClass (Divisible (..))
import           Money          (Money)

class Consumable a where
    cost :: a -> Consumption -> Maybe Cost


data LinearPrice = LinearPrice {consumption :: Consumption, money :: Money}
    deriving (Show, Eq, Ord)

instance Consumable LinearPrice where
    cost (LinearPrice c2 m) c1 = fractionalPrice $ fraction c1 c2
        where
            fractionalPrice Nothing = Nothing
            fractionalPrice (Just part) =
                Just $ Cost c1 (moneypart part)
                where
                    moneypart = multiply m



calculateCosts :: (Consumable a) => [a] -> Consumption -> [Cost]
calculateCosts priceList c = mapMaybe (`cost` c) priceList

-- currencyCosts :: String -> [Cost] -> [Cost]
-- currencyCosts c = filter (\(Cost (Price _ (Money c1 _))) -> c1 == c)

-- currencyMoney :: String -> [Cost] -> Maybe Money
-- currencyMoney c ps = foldMoney $ currencyCosts c ps
--   where
--     foldMoney []  = Nothing
--     foldMoney ps1 = foldr addMoney Nothing ps1
--     addMoney (Cost (Price _ (Money _ k))) (Just (Money c1 ks)) = Just $ Money c1 (ks + k)
--     addMoney (Cost (Price _ m)) Nothing = Just m
