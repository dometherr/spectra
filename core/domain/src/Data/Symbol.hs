{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Symbol (Symbol, mkSymbol) where

import qualified Data.Text as T
import           Data.Variance (Variance, Over15'')

-- | A symbol represents a financial asset
data Symbol
    = Symbol {symId :: SymbolId, symVars :: [Variance Over15'']}
    deriving (Show, Eq)

-- | A symbol's unique identifier
newtype SymbolId
    = SymbolId {unSymbolId :: T.Text}
    deriving (Show, Eq, Ord)

-- | A symbol builder
mkSymbol :: T.Text -> Symbol
mkSymbol = flip Symbol [] . SymbolId
