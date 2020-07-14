{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Urbit.Azimuth.Point (
    Point(..)
  , Details(..)
  , Rights(..)

  , Keys(..)
  , CryptKey(..)
  , AuthKey(..)
  , CryptoSuite(..)
  , keyInformation

  , Life(..)
  , Rift(..)

  , Address

  , isOwner
  , isLive
  , isSponsor
  , isRequestingEscapeTo
  , isSpawnProxy
  , isTransferProxy
  , isManagementProxy
  , isVotingProxy

  , hasBeenLinked
  , canManageNetworkKeys
  , canSponsor
  , canManage
  , canSpawn
  , canVote
  , canTransfer
  ) where

import qualified Data.ByteArray.Sized as DBS
import Data.Solidity.Prim.Address (Address)
import qualified Data.Solidity.Prim as Solidity.Prim (UIntN, BytesN)
import GHC.Generics (Generic)
import Prelude
import Urbit.Ob (Patp)
import qualified Urbit.Ob as Ob

-- | An Azimuth point, represented by its \@p, 'Details', and 'Rights'.
data Point = Point {
    pointPatp    :: Patp
  , pointDetails :: Details
  , pointRights  :: Rights
  }
  deriving stock (Show, Eq, Generic)

-- | Various information about a 'Point'.
data Details = Details {
    detailsCryptKey          :: CryptKey
  , detailsAuthKey           :: AuthKey
  , detailsHasSponsor        :: Bool
  , detailsActive            :: Bool
  , detailsEscapeRequested   :: Bool
  , detailsSponsor           :: Patp
  , detailsEscapeRequestedTo :: Patp
  , detailsCryptoSuite       :: CryptoSuite
  , detailsLife              :: Life
  , detailsRift              :: Rift
  }
  deriving stock (Show, Eq, Generic)

-- | Ownership and proxy information for a 'Point'.
data Rights = Rights {
    rightsOwner           :: Address
  , rightsManagementProxy :: Maybe Address
  , rightsSpawnProxy      :: Maybe Address
  , rightsVotingProxy     :: Maybe Address
  , rightsTransferProxy   :: Maybe Address
  }
  deriving stock (Show, Eq, Generic)

-- | Grab a point's key information.
keyInformation :: Point -> Keys
keyInformation Point {..} = Keys {
      keyCrypt       = detailsCryptKey
    , keyAuth        = detailsAuthKey
    , keyCryptoSuite = detailsCryptoSuite
    }
  where
    Details {..} = pointDetails
-- | A point's public encryption key.
newtype CryptKey = CryptKey {
    fromCryptKey :: Solidity.Prim.BytesN 32
  }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | A point's public authentication key.
newtype AuthKey = AuthKey {
    fromAuthKey :: Solidity.Prim.BytesN 32
  }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | A point's crypto suite version.
newtype CryptoSuite = CryptoSuite {
    fromCryptoSuite :: Solidity.Prim.UIntN 32
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Bounded, Integral)

-- | A point's life (i.e., recorded number of key revisions).
newtype Life = Life {
    fromLife :: Solidity.Prim.UIntN 32
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Bounded, Integral)

-- | A point's rift (i.e., recorded number of continuity breaches).
newtype Rift = Rift {
    fromRift :: Solidity.Prim.UIntN 32
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Bounded, Integral)

-- | Public key information for a 'Point'.
data Keys = Keys {
    keyCrypt       :: CryptKey
  , keyAuth        :: AuthKey
  , keyCryptoSuite :: CryptoSuite
  } deriving stock (Show, Eq)

-- | Check if an address is the owner of a point.
isOwner :: Point -> Address -> Bool
isOwner Point {..} addr = rightsOwner pointRights == addr

-- | Check if a point has been booted.
hasBeenLinked :: Point -> Bool
hasBeenLinked = (> 0) . detailsLife . pointDetails

-- | Check if a point is live.
isLive :: Point -> Bool
isLive Point {..} =
       fromCryptKey detailsCryptKey /= DBS.zero
    && fromAuthKey detailsAuthKey /= DBS.zero
    && fromCryptoSuite detailsCryptoSuite /= 0
  where
    Details {..} = pointDetails

-- | Check if a point is the sponsor of another.
isSponsor :: Point -> Patp -> Bool
isSponsor Point {..} point =
    detailsHasSponsor && detailsSponsor == point
  where
    Details {..} = pointDetails

-- | Check if a point is requesting escape to another point.
isRequestingEscapeTo :: Point -> Patp -> Bool
isRequestingEscapeTo Point {..} patp =
    detailsEscapeRequested && detailsEscapeRequestedTo == patp
  where
    Details {..} = pointDetails

-- | Check if an address is a spawn proxy for a point.
isSpawnProxy :: Point -> Address -> Bool
isSpawnProxy Point {..} addr = case rightsSpawnProxy pointRights of
  Just a  -> a == addr
  Nothing -> False

-- | Check if an address is a transfer proxy for a point.
isTransferProxy :: Point -> Address -> Bool
isTransferProxy Point {..} addr = case rightsTransferProxy pointRights of
  Just a  -> a == addr
  Nothing -> False

-- | Check if an address is a management proxy for a point.
isManagementProxy :: Point -> Address -> Bool
isManagementProxy Point {..} addr = case rightsManagementProxy pointRights of
  Just a  -> a == addr
  Nothing -> False

-- | Check if an address is a voting proxy for a point.
isVotingProxy :: Point -> Address -> Bool
isVotingProxy Point {..} addr = case rightsVotingProxy pointRights of
  Just a  -> a == addr
  Nothing -> False

-- | Check if an address can manage a point's networking keys.
canManageNetworkKeys :: Point -> Address -> Bool
canManageNetworkKeys point addr =
     canManage point addr
  && hasBeenLinked point

-- | Check if a point can sponsor another.
canSponsor :: Point -> Bool
canSponsor Point {..} = case Ob.clan pointPatp of
  Ob.Galaxy -> True
  Ob.Star   -> True
  _         -> False

-- | Check if an address can manage a point.
canManage :: Point -> Address -> Bool
canManage point addr =
     isOwner point addr
  || isManagementProxy point addr

-- | Check if an address can spawn for a point.
canSpawn :: Point -> Address -> Bool
canSpawn point addr =
     canSponsor point
  && hasBeenLinked point
  && (isOwner point addr || isSpawnProxy point addr)

-- | Check if an address can vote for a point.
canVote :: Point -> Address -> Bool
canVote point@Point {..} addr =
       Ob.Galaxy == Ob.clan pointPatp
    && detailsActive
    && (isOwner point addr || isVotingProxy point addr)
  where
    Details {..} = pointDetails

-- | Check if an address can transfer ownership of a point.
canTransfer :: Point -> Address -> Bool
canTransfer point addr =
     isOwner point addr
  || isTransferProxy point addr

