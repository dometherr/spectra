{-# LANGUAGE OverloadedStrings #-}

module Data.Symbol (Symbol, mkSymbol) where

import qualified Data.Text as T

-- | A symbol represents a financial asset
data Symbol
    = Symbol {symId :: SymbolId}

-- | A symbol's unique identifier
newtype SymbolId
    = SymbolId {unSymbolId :: T.Text}

-- | A symbol builder
mkSymbol :: T.Text -> Symbol
mkSymbol = Symbol . SymbolId
