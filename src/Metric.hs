module Metric where

newtype Metric where
  Metric :: String -> Metric
  deriving (Show, Eq, Ord)