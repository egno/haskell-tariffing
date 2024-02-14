module Price.ConsumableClass where

import           Price.Consumption (Consumption)
import           Price.Cost        (Cost)

class Consumable a where
    cost :: a -> Consumption -> Maybe Cost
