module Cost where

import           Consumption (Consumption(..))
import           Money       (Money)

data Cost = Cost {consumption :: Consumption, money :: Money}
    deriving (Show, Eq, Ord)
