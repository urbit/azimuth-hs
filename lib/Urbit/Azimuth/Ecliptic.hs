{-# LANGUAGE RecordWildCards #-}

module Urbit.Azimuth.Ecliptic (
    RevisionType(..)

  , exists
  , getApproved
  , isApprovedForAll
  , getSpawnLimit
  , canEscapeTo
  , configureKeys
  , createGalaxy
  ) where

import Network.Ethereum.Account (LocalKeyAccount)
import qualified Network.Ethereum.Api.Types as Api
import Network.JsonRpc.TinyClient (JsonRpc)
import Numeric.Natural
import Urbit.Azimuth.Contract
import qualified Urbit.Azimuth.Ecliptic.Internal as I
import Urbit.Azimuth.Point
import qualified Urbit.Ob as Ob
import qualified Urbit.Ob.Extended as Ob

data RevisionType =
    Rotate
  | Breach
  deriving (Eq, Show)

-- | Check if a point is active.
exists :: (JsonRpc m, MonadFail m) => Ob.Patp -> Azimuth m Bool
exists patp = withContract ecliptic $ do
  let point = Ob.patpToSolidity256 patp
  I.exists point

-- | Get the approved transfer proxy for a point.
getApproved :: (JsonRpc m, MonadFail m) => Ob.Patp -> Azimuth m Address
getApproved patp = withContract ecliptic $ do
  let point = Ob.patpToSolidity256 patp
  I.getApproved point

-- | Check if an address is an operator for an owner.
isApprovedForAll
  :: (JsonRpc m, MonadFail m)
  => Address
  -> Address
  -> Azimuth m Bool
isApprovedForAll owner operator = withContract ecliptic $
  I.isApprovedForAll owner operator

-- | Get the total number of children a point can spawn at some time.
getSpawnLimit
  :: (JsonRpc m, MonadFail m)
  => Ob.Patp
  -> Natural
  -> Azimuth m Natural
getSpawnLimit patp time = withContract ecliptic $ do
  let point = Ob.patpToPoint patp
  limit <- I.getSpawnLimit point (fromIntegral time)
  pure $ fromIntegral limit

-- | Check if a point can escape to a sponsor.
canEscapeTo
  :: (JsonRpc m, MonadFail m)
  => Ob.Patp
  -> Ob.Patp
  -> Azimuth m Bool
canEscapeTo point sponsor = withContract ecliptic $ do
  let pt = Ob.patpToPoint point
      sp = Ob.patpToPoint sponsor
  I.canEscapeTo pt sp

-- | Configure a point's keys.
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

-- | Create the specified galaxy.
createGalaxy
  :: (JsonRpc m, MonadFail m)
  => Ob.Patp
  -> Address
  -> Azimuth m Api.TxReceipt
createGalaxy patp addr = case Ob.patpToGalaxy patp of
  -- NB a proper way to handle this sort of error would be nice
  Left _    -> error (show patp <> " is not a galaxy")
  Right gal -> withContract ecliptic $ I.createGalaxy gal addr
