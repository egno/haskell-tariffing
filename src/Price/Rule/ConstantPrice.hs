module Price.Rule.ConstantPrice where

import           Price.ConsumableClass (Consumable (..))
import           Price.Consumption     (Consumption (..))
import           Price.Cost            (Cost (Cost))
import           Price.Money           (Money)

newtype ConstantPrice = ConstantPrice Money
    deriving (Show, Eq, Ord)

instance Consumable ConstantPrice where
    cost :: ConstantPrice -> Consumption -> Maybe Cost
    cost (ConstantPrice m) c1 = Just $ Cost c1 m

