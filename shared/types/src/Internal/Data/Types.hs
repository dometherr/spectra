module Internal.Data.Types (
    Timestamp,
    unTimestamp,
    mkTimestamp, 

    Duration,
    unDuration,
    mkDuration,
    addd
) where

-- | A timestamp represents a point in time
newtype Timestamp
    = Timestamp { unTimestamp :: Integer }
    deriving (Show, Eq)

-- | A timestamp builder
mkTimestamp :: Integer -> Timestamp
mkTimestamp = Timestamp

-- | A duration represents a period of time
newtype Duration
    = Duration { unDuration :: Integer }
    deriving (Show, Eq)

-- | A duration builder
mkDuration :: Integer -> Duration
mkDuration = Duration

-- | A function that adds a Integer to a Duration
addd :: Integer -> Duration -> Duration
addd n d = mkDuration $ (unDuration d + n)
