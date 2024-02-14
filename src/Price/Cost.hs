module Price.Cost where

import           Price.Consumption (Consumption (..))
import           Price.Money       (Money (..))

data Cost = Cost {consumption :: Consumption, money :: Money}
    deriving (Show, Eq, Ord)
