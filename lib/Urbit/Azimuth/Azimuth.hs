{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Urbit.Azimuth.Azimuth (
    getPoint
  , getDetails
  , getRights
  ) where

import qualified Data.Default.Class as Default
import Data.Solidity.Prim.Address (Address)
import Network.Ethereum.Account (LocalKeyAccount)
import Network.JsonRpc.TinyClient (JsonRpc)
import Urbit.Azimuth.Azimuth.Internal as I
import Urbit.Azimuth.Contract
import Urbit.Azimuth.Point
import qualified Urbit.Ob as Ob
import qualified Urbit.Ob.Extended as Ob

-- | Fetch a point's details and rights.
getPoint
  :: (JsonRpc m, MonadFail m)
  => Ob.Patp
  -> Azimuth (LocalKeyAccount m) Point
getPoint patp = do
  deets  <- getDetails patp
  rights <- getRights patp
  pure (Point patp deets rights)

-- | Fetch a point's details.
getDetails
  :: (JsonRpc m, MonadFail m)
  => Ob.Patp
  -> Azimuth (LocalKeyAccount m) Details
getDetails patp = withContract azimuth $ do
  let point = Ob.patpToPoint patp

  ( CryptKey -> detailsCryptKey,
    AuthKey -> detailsAuthKey,
    detailsHasSponsor,
    detailsActive,
    detailsEscapeRequested,
    Ob.pointToPatp -> detailsSponsor,
    Ob.pointToPatp -> detailsEscapeRequestedTo,
    CryptoSuite -> detailsCryptoSuite,
    Life -> detailsLife,
    Rift -> detailsRift ) <- I.points point

  pure Details { .. }

-- | Fetch a point's rights.
getRights
  :: (JsonRpc m, MonadFail m)
  => Ob.Patp
  -> Azimuth (LocalKeyAccount m) Rights
getRights patp = withContract azimuth $ do
  let unzero x = if isZeroAddress x then Nothing else Just x
      point    = Ob.patpToPoint patp

  ( rightsOwner,
    unzero -> rightsManagementProxy,
    unzero -> rightsSpawnProxy,
    unzero -> rightsVotingProxy,
    unzero -> rightsTransferProxy ) <- I.rights point

  pure Rights { .. }

isZeroAddress :: Address -> Bool
isZeroAddress addr = addr == Default.def

