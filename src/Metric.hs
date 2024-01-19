module Metric where

newtype Metric = Metric String
    deriving (Show, Eq, Ord)