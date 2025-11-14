module Internal.Data.Types (
    Timestamp,
    unTimestamp,
    mkTimestamp
) where

-- | A timestamp represents a point in time
newtype Timestamp
    = Timestamp { unTimestamp :: Integer }
    deriving (Show, Eq)

mkTimestamp :: Integer -> Timestamp
mkTimestamp = Timestamp
