{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Urbit.Azimuth.Azimuth (
    getPoint
  , getDetails
  , getRights
  , getSpawnCount
  , getSpawned
  , getSponsoringCount
  , getSponsoring
  , getOwnedPointCount
  , getOwnedPoints
  , getManagerForCount
  , getManagerFor
  , getVotingForCount
  , getVotingFor
  , getSpawningForCount
  , getSpawningFor
  , getTransferringForCount
  , getTransferringFor
  , getEscapeRequests
  , getEscapeRequestsCount
  ) where

import qualified Data.Default.Class as Default
import Network.JsonRpc.TinyClient (JsonRpc)
import Numeric.Natural
import qualified Urbit.Azimuth.Azimuth.Internal as I
import Urbit.Azimuth.Contract
import Urbit.Azimuth.Point
import qualified Urbit.Ob as Ob
import qualified Urbit.Ob.Extended as Ob

-- | Fetch a point's details and rights.
getPoint :: JsonRpc m => Ob.Patp -> Azimuth m Point
getPoint patp = do
  deets  <- getDetails patp
  rights <- getRights patp
  pure (Point patp deets rights)

-- | Fetch a point's details.
getDetails :: JsonRpc m => Ob.Patp -> Azimuth m Details
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
getRights :: JsonRpc m => Ob.Patp -> Azimuth m Rights
getRights patp = withContract azimuth $ do
  let unzero x = if isZeroAddress x then Nothing else Just x
      point    = Ob.patpToPoint patp

  ( rightsOwner,
    unzero -> rightsManagementProxy,
    unzero -> rightsSpawnProxy,
    unzero -> rightsVotingProxy,
    unzero -> rightsTransferProxy ) <- I.rights point

  pure Rights { .. }

-- | Get the number of points a point has spawned.
getSpawnCount :: JsonRpc m => Ob.Patp -> Azimuth m Natural
getSpawnCount patp = withContract azimuth $ do
  let point = Ob.patpToPoint patp
  count <- I.getSpawnCount point
  pure $ fromIntegral count

-- | Get a list of points that a point has spawned.
getSpawned :: JsonRpc m => Ob.Patp -> Azimuth m [Ob.Patp]
getSpawned patp = withContract azimuth $ do
  let point = Ob.patpToPoint patp
  children <- I.getSpawned point
  pure $ fmap Ob.pointToPatp children

-- | Get a list of points that a point is sponsoring.
getSponsoring :: JsonRpc m => Ob.Patp -> Azimuth m [Ob.Patp]
getSponsoring patp = withContract azimuth $ do
  let point = Ob.patpToPoint patp
  children <- I.getSponsoring point
  pure $ fmap Ob.pointToPatp children

-- | Get the number of points a point is sponsoring.
getSponsoringCount :: JsonRpc m => Ob.Patp -> Azimuth m Natural
getSponsoringCount patp = withContract azimuth $ do
  let point = Ob.patpToPoint patp
  count <- I.getSponsoringCount point
  pure $ fromIntegral count

-- | Get a list of points that are requesting escape from a point.
getEscapeRequests :: JsonRpc m => Ob.Patp -> Azimuth m [Ob.Patp]
getEscapeRequests patp = withContract azimuth $ do
  let point = Ob.patpToPoint patp
  children <- I.getEscapeRequests point
  pure $ fmap Ob.pointToPatp children

-- | Get the number of points that are requesting escape from a point.
getEscapeRequestsCount
  :: JsonRpc m
  => Ob.Patp
  -> Azimuth m Natural
getEscapeRequestsCount patp = withContract azimuth $ do
  let point = Ob.patpToPoint patp
  count <- I.getEscapeRequestsCount point
  pure $ fromIntegral count

-- | Get a list of points that an address owns.
getOwnedPoints :: JsonRpc m => Address -> Azimuth m [Ob.Patp]
getOwnedPoints addr = withContract azimuth $ do
  children <- I.getOwnedPoints addr
  pure $ fmap Ob.pointToPatp children

-- | Get the number of points that an address owns.
getOwnedPointCount :: JsonRpc m => Address -> Azimuth m Natural
getOwnedPointCount addr = withContract azimuth $ do
  count <- I.getOwnedPointCount addr
  pure $ fromIntegral count

-- | Get a list of points that an address manages.
getManagerFor :: JsonRpc m => Address -> Azimuth m [Ob.Patp]
getManagerFor addr = withContract azimuth $ do
  children <- I.getManagerFor addr
  pure $ fmap Ob.pointToPatp children

-- | Get the number of points that an address manages.
getManagerForCount :: JsonRpc m => Address -> Azimuth m Natural
getManagerForCount addr = withContract azimuth $ do
  count <- I.getManagerForCount addr
  pure $ fromIntegral count

-- | Get a list of points that an address can vote on behalf of.
getVotingFor :: JsonRpc m => Address -> Azimuth m [Ob.Patp]
getVotingFor addr = withContract azimuth $ do
  children <- I.getVotingFor addr
  pure $ fmap Ob.pointToPatp children

-- | Get the number of points that an address can vote on behalf of.
getVotingForCount :: JsonRpc m => Address -> Azimuth m Natural
getVotingForCount addr = withContract azimuth $ do
  count <- I.getVotingForCount addr
  pure $ fromIntegral count

-- | Get a list of points that an address is a spawn proxy for.
getSpawningFor :: JsonRpc m => Address -> Azimuth m [Ob.Patp]
getSpawningFor addr = withContract azimuth $ do
  children <- I.getSpawningFor addr
  pure $ fmap Ob.pointToPatp children

-- | Get the number of points that an address is a spawn proxy for.
getSpawningForCount :: JsonRpc m => Address -> Azimuth m Natural
getSpawningForCount addr = withContract azimuth $ do
  count <- I.getSpawningForCount addr
  pure $ fromIntegral count

-- | Get a list of points that an address is a transfer proxy for.
getTransferringFor
  :: JsonRpc m
  => Address
  -> Azimuth m [Ob.Patp]
getTransferringFor addr = withContract azimuth $ do
  children <- I.getTransferringFor addr
  pure $ fmap Ob.pointToPatp children

-- | Get the number of points that an address is a transfer proxy for.
getTransferringForCount
  :: JsonRpc m
  => Address
  -> Azimuth m Natural
getTransferringForCount addr = withContract azimuth $ do
  count <- I.getTransferringForCount addr
  pure $ fromIntegral count

isZeroAddress :: Address -> Bool
isZeroAddress addr = addr == Default.def

