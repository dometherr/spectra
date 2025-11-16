{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Data.Variance (
    Variance,
    mkVariance,
    Fluctuation,
    mkFluctuation,
    Interval (..),
    Over,
    Scale,
    scale,
    Over15'',
    Over1',
    Over5',
    Over15',
) where

import qualified Data.Vector as V
import           GHC.TypeLits (Nat)
import           Internal.Data.Types (Timestamp)
import qualified Data.Text as T

-- | A variance represents the price variance over a given time interval
data Variance over where
    WVariance ::
        { openedAt :: Timestamp
        , fluctuation :: Fluctuation
        } ->
        Variance (Over (a :: Nat) (i :: Interval))

-- | A standalone Show instance for Variance
deriving instance Show (Variance over)
deriving instance Eq (Variance over)

-- | A variance builder
mkVariance :: Timestamp -> Fluctuation -> Variance (Over (a :: Nat) (i :: Interval))
mkVariance = WVariance

-- | A fluctuation represents the price fluctuation
data Fluctuation
    = Fluctuation {open, high, low, close :: Double}
    deriving (Show, Eq)

-- | A fluctuation builder
mkFluctuation :: Double -> Double -> Double -> Double -> Either T.Text Fluctuation
mkFluctuation o h l c
    | o <= h 
        && o >= l
        && h >= l 
        && h >= c
        && l <= c  = Right $ Fluctuation o h l c
    | otherwise    = Left "fluctuation is not valid: out of bounds"

-- | A interval represents the time interval
data Interval = Second | Minute | Hour
    deriving (Show, Eq)

-- | A type-level function that holds the amount at given interval
data Over (a :: Nat) (i :: Interval)
    deriving (Show, Eq)

-- | Type aliases for common over intervals
type Over15'' = Over 15 'Second
type Over1' = Over 1 'Minute
type Over5' = Over 5 'Minute
type Over15' = Over 15 'Minute

-- | A scale converts a list of variances into a single variance
class Foldable f => Scale f from to where
    -- | A contract that ensures conversion at the type level from @from@ to @to@
    scale :: f from -> f to

-- | A scale instance that knows how to convert 15 second variances into 1 minute variances
instance Scale V.Vector (Variance Over15'') (Variance Over1') where
    scale = aggregate 4

-- | A scale instance that knows how to convert 1 minute variances into 5 minute variances
instance Scale V.Vector (Variance Over1') (Variance Over5') where
    scale = aggregate 5

-- | A function that aggregates a vector of variances into a aggregated vector of variances
-- 
-- If the input length is not evenly divisible by @size@, trailing elements are dropped.
aggregate :: 
    ( from ~ Variance (Over (a  :: Nat) (i  :: Interval))  -- ^ The input variance type
    , to   ~ Variance (Over (a' :: Nat) (i' :: Interval)) -- ^ The output variance type
    ) => 
    Int -> V.Vector from -> V.Vector to
aggregate size froms
    | size <= 0 || V.null froms = V.empty
    | otherwise                 = V.generate chunks chunk
  where
    -- | A function that returns the highest high and lowest low values of a fluctuation
    comparing (!h, !l) v =
        (max h $ high $ fluctuation v, min l $ low $ fluctuation v)
    -- | The number of chunks to generate
    chunks = V.length froms `div` size
    -- | A function that generates a variance for a given position
    chunk pos =
        let
            slice      = V.slice (pos * size) size froms
            (sh, sl)   = (V.head slice, V.last slice)
            (sho, slc) = (open $ fluctuation sh, close $ fluctuation sl)
            (shi, slo) = 
                V.foldl' comparing 
                        (high $ fluctuation sh, low $ fluctuation sh)
                        (V.tail slice)
         in mkVariance (openedAt sh) $ Fluctuation sho shi slo slc
