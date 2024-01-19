module DivisibleClass where

class Divisible a where
    fraction :: a -> a -> Maybe Float
    multiply :: a -> Float -> a
    add :: a -> a -> Maybe a
