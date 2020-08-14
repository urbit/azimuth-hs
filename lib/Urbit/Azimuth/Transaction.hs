{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}

module Urbit.Azimuth.Transaction (
    Api.TxReceipt(..)

  , TxnParams(..)
  , defaultTxnParams
  ) where

import qualified Network.Ethereum.Api.Types as Api
import Numeric.Natural

-- | Optional transaction parameters for Azimuth actions.
data TxnParams = TxnParams {
    txnGasLimit :: Maybe Natural
  , txnGasPrice :: Maybe Natural
  , txnBlock    :: Maybe Api.DefaultBlock
  } deriving stock Show

-- | Default optional transaction parameters (i.e., a gas price of 80 Gwei).
defaultTxnParams :: TxnParams
defaultTxnParams = TxnParams {
    txnGasLimit = Nothing
  , txnGasPrice = Just 80_000_000_000
  , txnBlock    = Nothing
  }

