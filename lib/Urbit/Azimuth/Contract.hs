{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Urbit.Azimuth.Contract (
    Contracts(..)
  , getContracts
  , withContract

  , Azimuth(..)
  , runAzimuth
  , runAzimuth'
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT(..), asks)
import Data.Solidity.Prim.Address (Address)
import qualified Network.Ethereum.Account as Ethereum.Account
import qualified Network.Ethereum.Account.Internal as AI
import qualified Network.Ethereum.Ens as Ethereum.Ens
import Network.JsonRpc.TinyClient (JsonRpc)
import Urbit.Azimuth.Account
import Urbit.Azimuth.Transaction

-- | Supported Azimuth contracts.
data Contracts = Contracts {
    azimuth  :: Address
  , ecliptic :: Address
  } deriving stock Show

-- | The environment for any given Azimuth action.
data AzimuthEnv = AzimuthEnv {
    azimuthContracts :: Contracts
  , azimuthTxnParams :: Maybe TxnParams
  } deriving stock Show

-- | The Azimuth type represents an authenticated connection to a JSON RPC by
--   way of a local private key, and has access to a 'Contracts' object.
newtype Azimuth p m a =
    Azimuth (ReaderT AzimuthEnv (AI.AccountT p m) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Run an Azimuth action using default optional transaction parameters.
runAzimuth
  :: Unlockable p m
  => Contracts
  -> p
  -> Azimuth p m a
  -> m a
runAzimuth contracts account (Azimuth action) = withAccount account $
  runReaderT action (AzimuthEnv contracts Nothing)

-- | Run an Azimuth action, specifying optional transaction parameters
--   explicitly.
runAzimuth'
  :: Unlockable p m
  => Contracts
  -> TxnParams
  -> p
  -> Azimuth p m a
  -> m a
runAzimuth' contracts params account (Azimuth action) =
  withAccount account $
    runReaderT action (AzimuthEnv contracts (Just params))

-- | Fetch the Azimuth and Ecliptic contracts by way of their ENS records.
getContracts :: JsonRpc m => m Contracts
getContracts = withAccount () $ do
  azimuth  <- Ethereum.Ens.resolve "azimuth.eth"
  ecliptic <- Ethereum.Ens.resolve "ecliptic.eth"
  pure Contracts {..}

-- | Perform an Azimuth action, targeting the appropriate contract.
withContract
  :: Unlockable p m
  => (Contracts -> Address)
  -> AI.AccountT p m a
  -> Azimuth p m a
withContract selector action = Azimuth $ do
  contracts <- asks azimuthContracts
  txnParams <- asks azimuthTxnParams

  let contract = Just (selector contracts)

      TxnParams {..} = case txnParams of
        Nothing -> defaultTxnParams
        Just ps -> ps

      -- NB this can probably be improved with a better understanding of
      --    Network.Ethereum.Api.Types.DefaultBlock
      --
      param pars = case txnBlock of

        Just block -> pars {
            AI._to       = contract
          , AI._gasLimit = fmap fromIntegral txnGasLimit
          , AI._gasPrice = fmap fromIntegral txnGasPrice
          , AI._block    = block
          }

        Nothing -> pars {
            AI._to       = contract
          , AI._gasLimit = fmap fromIntegral txnGasLimit
          , AI._gasPrice = fmap fromIntegral txnGasPrice
          }

  lift $ Ethereum.Account.withParam param action
