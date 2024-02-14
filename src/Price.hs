module Price where

import           Data.Maybe            (mapMaybe)
import           Price.ConsumableClass (Consumable (..))
import           Price.Consumption     (Consumption)
import           Price.Cost            (Cost (..), money)
import           Price.Money           (Money (..), amount, currency)



calculateCosts :: (Consumable a) => [a] -> Consumption -> [Cost]
calculateCosts priceList c = mapMaybe (`cost` c) priceList

currencyCosts :: String -> [Cost] -> [Cost]
currencyCosts cur = filter (\c -> currency (money c) == cur)

currencyMoney :: String -> [Cost] -> Maybe Money
currencyMoney cur ps = foldMoney $ currencyCosts cur ps
  where
    foldMoney []  = Nothing
    foldMoney ps1 = foldr addMoney Nothing ps1

    addMoney c (Just (Money c1 ks)) = Just $ Money c1 (ks + amount (money c))
    addMoney c Nothing              = Just $ money c
