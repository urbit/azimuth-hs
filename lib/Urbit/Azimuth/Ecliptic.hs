{-# LANGUAGE RecordWildCards #-}

module Urbit.Azimuth.Ecliptic (
    RevisionType(..)

  , configureKeys
  ) where

import Network.Ethereum.Account (LocalKeyAccount)
import qualified Network.Ethereum.Api.Types as Api
import Network.JsonRpc.TinyClient (JsonRpc)
import Urbit.Azimuth.Contract
import qualified Urbit.Azimuth.Ecliptic.Internal as I
import Urbit.Azimuth.Point
import qualified Urbit.Ob as Ob
import qualified Urbit.Ob.Extended as Ob

data RevisionType =
    Rotate
  | Breach
  deriving (Eq, Show)

configureKeys
  :: (JsonRpc m, MonadFail m)
  => Ob.Patp
  -> Keys
  -> RevisionType
  -> Azimuth m Api.TxReceipt
configureKeys patp Keys {..} breach = do
  let point = Ob.patpToPoint patp
      ck    = fromCryptKey keyCrypt
      ak    = fromAuthKey keyAuth
      cs    = fromCryptoSuite keyCryptoSuite
      bc    = breach == Breach

  withContract ecliptic $ I.configureKeys point ck ak cs bc
