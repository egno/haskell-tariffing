module Example where

import Data.Hashable
import Price
import Unit 
import Money
import Metric

minute :: Unit
minute = Unit "min"

metrics :: [String]
metrics = [
    code ++ ":cpu-"++ cpuType++"*" ++ show cpu ++ "&ram*" ++ show (2^ram) ++ ":R" ++ [region] ++ "-" ++ show regionNum |
    code <- ["vcc","bm"],
    cpuType <- ["g0", "g1", "g2"],
    cpu <- [1,2,4,6,8,10,12,16,24,23,64],
    ram <- [1..8],
    region <- ['A'..'D'],
    regionNum <- [1..2]
    ]

price :: String -> Float 
price s = abs $ fromIntegral (hash s) / 1.0^10

fullPriceList :: [Price]
fullPriceList = [genPrice m c | m <- metrics, c <- ["USD", "EUR", "TLR"]]
    where genPrice s c = Price {consumption = Consumption (Metric s) (Volume minute 60.0), money = Money c (price $ c ++ s)}


costs :: [Cost]
costs = calculateCosts (Consumption (Metric "vcc:cpu-g2*8&ram*256:RB-2") (Volume minute (60.0 * 60.0 * 24.0) )) fullPriceList

totalMoney :: Maybe Money
totalMoney = currencyMoney "USD" costs