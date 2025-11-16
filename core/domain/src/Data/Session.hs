{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Session (
    Session,
    SessionMeta,
    SessionId,
    SessionParams,
    SessionState (..),
    mkSession,
    Transition,
    runTransition,
) where

import           Control.Lens (makeLenses, (%~), (&))
import           Data.Kind
import           Data.Symbol
import qualified Data.Text as T
import           Internal.Data.Types (Duration, Timestamp, unTimestamp, mkDuration)
import qualified Internal.Data.Types as Ty

-- | A session metadata
data SessionMeta
    = SessionMeta
    { sid :: SessionId
    , symbol :: Symbol
    , startedAt :: Timestamp
    , _pausedFor :: Duration
    , params :: SessionParams
    }
    deriving (Show, Eq)

-- | A session represents a trading session for a specific symbol
data Session s where
    SActive     :: SessionMeta              -> Session 'Active
    SPausedAt   :: SessionMeta -> Timestamp -> Session 'Paused
    SFinishedAt :: SessionMeta -> Timestamp -> Session 'Finished

deriving instance Show (Session s)
deriving instance Eq (Session s)

-- | A session's unique identifier
newtype SessionId
    = SessionId {unSessionId :: T.Text}
    deriving (Show, Eq, Ord)

-- | A session's parameters
data SessionParams
    = SessionParams
    { agentFlags :: [T.Text]
    }
    deriving (Show, Eq)

-- | A session state
data SessionState
    = Active
    | Paused
    | Finished
    deriving (Show, Eq)

makeLenses ''SessionMeta

-- | A session builder
mkSession :: Symbol -> SessionId -> Timestamp -> SessionParams -> Session 'Active
mkSession sym sessionId now ps =
    SActive $ SessionMeta sessionId sym now (mkDuration 0) ps

-- | A function that increments the duration of a paused session
pausedInc :: SessionMeta -> Timestamp -> Timestamp -> SessionMeta
pausedInc sm lastPause now =
    let lastedFor = unTimestamp now - unTimestamp lastPause
     in sm & pausedFor %~ (Ty.addd lastedFor)

-- -- | A transition on @s@ from @f@ to @t@
class Transition (s :: SessionState -> Type) f t where
    transition :: s f -> Timestamp -> s t

-- | A transition instance from 'Active' to 'Paused'
instance Transition Session 'Active 'Paused where
    transition (SActive sm) = 
        SPausedAt sm

-- | A transition instance from 'Active' to 'Finished'
instance Transition Session 'Active 'Finished where
    transition (SActive sm) = 
        SFinishedAt sm

-- | A transition instance from 'Paused' to 'Active'
instance Transition Session 'Paused 'Active where
    transition (SPausedAt sm at) = 
        SActive . pausedInc sm at
        
-- | A transition instance from 'Paused' to 'Finished'
instance Transition Session 'Paused 'Finished where
    transition (SPausedAt sm at) = \now -> 
        SFinishedAt (pausedInc sm at now) now

-- | A function that runs a transition from a session state to another
runTransition :: Transition s f t => s f -> Timestamp -> s t
runTransition = transition
