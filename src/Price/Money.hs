module Price.Money where

import           Price.DivisibleClass (Divisible (..), Fraction (..))

data Money where
  Money :: String -> Float -> Money
  deriving (Show, Eq, Ord)

instance Divisible Money where

  fraction :: Money -> Money -> Maybe Fraction
  fraction (Money c1 k1) (Money c2 k2)
    | c1 /= c2  = Nothing
    | otherwise = Just $ Fraction (k1 / k2)

  multiply :: Money -> Fraction -> Money
  multiply (Money c k) (Fraction p) = Money c (k * p)

  add :: Money -> Money -> Maybe Money
  add (Money c1 k1) (Money c2 k2)
    | c1 /= c2  = Nothing
    | otherwise = Just $ Money c1 (k1 + k2)


currency :: Money -> String
currency (Money c _) = c

amount :: Money -> Float
amount (Money _ v) = v
