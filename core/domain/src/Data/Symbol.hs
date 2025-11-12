{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Symbol (Symbol, mkSymbol) where

import qualified Data.Text as T
import Data.Variance

-- | A symbol represents a financial asset
data Symbol
    = Symbol {symId :: SymbolId, symVars :: [Variance (Over 15 'Second)]}

-- | A symbol's unique identifier
newtype SymbolId
    = SymbolId {unSymbolId :: T.Text}

-- | A symbol builder
mkSymbol :: T.Text -> Symbol
mkSymbol = flip Symbol [] . SymbolId
