module Example where

import           Data.Hashable (Hashable (hash))
import           Metric        (Metric (Metric))
import           Money         (Money (..))
import           Price         (Consumption (Consumption), Cost, Price (..),
                                Volume (Volume), calculateCosts, currencyMoney)
import           Unit          (Unit (..))

minute :: Unit
minute = Unit "min"

metrics :: [String]
metrics = [
    code ++ ":cpu-"++ cpuType++"*" ++ cpu ++ "&ram*" ++ ram ++ ":R" ++ [region] ++ "-" ++ [regionNum] |
    code <- ["vcc","bm"],
    cpuType <- ["g0", "g1", "g2"],
    cpu <- [show i | i::Int <- [1,2,4,6,8,10,12,16,24,23,64]],
    ram <- [show $ (2::Int) ^ i| i::Int <- [1..8]],
    region <- ['A'..'D'],
    regionNum <- ['1'..'2']
    ]

price :: String -> Float
price s = abs $ fromIntegral (hash s) / 1.0 ^ (10::Int)

fullPriceList :: [Price]
fullPriceList = [genPrice m c | m <- metrics, c <- ["USD", "EUR", "TLR"]]
    where genPrice s c = Price {consumption = Consumption (Metric s) (Volume minute 60.0), money = Money c (price $ c ++ s)}

test_metric :: Metric
test_metric = Metric "vcc:cpu-g2*8&ram*256:RB-2"

test_volume :: Volume
test_volume=Volume minute (60.0 * 60.0 * 24.0)

costs :: [Cost]
costs = calculateCosts (Consumption test_metric test_volume) fullPriceList

totalMoney :: Maybe Money
totalMoney = currencyMoney "USD" costs
