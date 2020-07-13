{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Urbit.Azimuth.Contract (
    Contracts(..)
  , getContracts
  , withContract

  , Azimuth(..)
  , runAzimuth
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT(..), ask)
import Data.Solidity.Prim.Address (Address)
import Network.Ethereum.Account (LocalKeyAccount)
import qualified Network.Ethereum.Account as Ethereum.Account
import qualified Network.Ethereum.Account.Internal as AI
import Network.Ethereum.Api.Types (DefaultBlock(..), Quantity)
import qualified Network.Ethereum.Ens as Ethereum.Ens
import Network.JsonRpc.TinyClient (JsonRpc)

data Contracts = Contracts {
    azimuth  :: Address
  , ecliptic :: Address
  } deriving Show

newtype Azimuth m a =
    Azimuth (ReaderT (Contracts, Quantity) (LocalKeyAccount m) a)
  deriving newtype (Functor, Applicative, Monad)

runAzimuth :: Contracts -> Quantity -> Azimuth m a -> LocalKeyAccount m a
runAzimuth contracts block (Azimuth action) =
  runReaderT action (contracts, block)

getContracts :: (JsonRpc m, MonadFail m) => LocalKeyAccount m Contracts
getContracts = do
  azimuth  <- Ethereum.Ens.resolve "azimuth.eth"
  ecliptic <- Ethereum.Ens.resolve "ecliptic.eth"
  pure Contracts {..}

withContract
  :: Monad m
  => (Contracts -> Address)
  -> LocalKeyAccount m a
  -> Azimuth m a
withContract selector action = Azimuth $ do
  (contracts, block) <- ask
  lift $ Ethereum.Account.withParam (\param -> param {
    AI._to    = Just (selector contracts)
  , AI._block = BlockWithNumber block
  -- NB hardcoded to 40 gwei at present
  -- going to want a better way to specify gas price
  , AI._gasPrice = Just 40_000_000_000
  }) action

