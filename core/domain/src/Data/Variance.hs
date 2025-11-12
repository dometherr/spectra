{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Variance (
    Variance,
    Over,
    Interval (..),
    Scale,
    mkVariance,
    scale,
) where

import qualified Data.Vector as V
import           GHC.TypeLits (Nat)

-- | A variance represents the price fluctuation
data Variance o
    = Variance {open, high, low, close :: Double}
    deriving (Show)

-- | A type-level function that holds the amount at given interval
data Over (a :: Nat) (i :: Interval)
    deriving (Show)

-- | A interval represents the time interval
data Interval = Second | Minute | Hour
    deriving (Show)

-- | A variance builder
mkVariance :: Double -> Double -> Double -> Double -> Variance (Over a i)
mkVariance = Variance

-- | A scale converts a list of variances into a single variance
class Scale from to where
    scale :: [from] -> [to]

-- | A scale instance that knows how to convert 15 second variances into 1 minute variances
instance Scale (Variance (Over 15 'Second)) (Variance (Over 1 'Minute)) where
    scale = V.toList . aggregate 4 . V.fromList

-- | A scale instance that knows how to convert 1 minute variances into 5 minute variances
instance Scale (Variance (Over 1 'Minute)) (Variance (Over 5 'Minute)) where
    scale = V.toList . aggregate 5 . V.fromList

-- | A helper function that aggregates a vector of variances into a aggregated vector of variances
aggregate :: Int -> V.Vector (Variance o) -> V.Vector (Variance (Over a i))
aggregate m vs = V.generate chunks mkChunk
  where
    chunks = V.length vs `div` m
    mkChunk i =
        let slice = V.slice (i * m) m vs
            (o, c) = (open (V.head slice), close (V.last slice))
            (hi, lo) =
                V.foldl'
                    (\(!h, !l) v -> (max h $ high v, min l $ low v))
                    (high (V.head slice), low (V.head slice))
                    (V.tail slice)
         in mkVariance o hi lo c
