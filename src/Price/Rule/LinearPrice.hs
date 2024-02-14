module Price.Rule.LinearPrice where

import           Price.ConsumableClass (Consumable (..))
import           Price.Consumption     (Consumption (..))
import           Price.Cost            (Cost (Cost))
import           Price.DivisibleClass  (Divisible (..))
import           Price.Money           (Money)

data LinearPrice = LinearPrice {consumption :: Consumption, money :: Money}
    deriving (Show, Eq, Ord)

instance Consumable LinearPrice where
    cost :: LinearPrice -> Consumption -> Maybe Cost
    cost (LinearPrice c2 m) c1 = fractionalPrice $ fraction c1 c2
        where
            moneypart = multiply m
            fractionalPrice Nothing = Nothing
            fractionalPrice (Just part) =
                Just $ Cost c1 (moneypart part)
