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
  , Life(..)
  , Rift(..)

  , Address(..)

  , isOwner
  , isLive
  , isSponsor
  , isRequestingEscapeTo
  , isSpawnProxy
  , isTransferProxy
  , isManagementProxy
  , isVotingProxy

  , hasBeenLinked
  , keyInformation
  ) where

import Data.Solidity.Prim.Address (Address)
import qualified Data.Solidity.Prim as Solidity.Prim (UIntN, BytesN)
import GHC.Generics (Generic)
import Prelude
import Urbit.Ob (Patp)

data Point = Point {
    pointPatp    :: Patp
  , pointDetails :: Details
  , pointRights  :: Rights
  }
  deriving stock (Show, Eq, Generic)

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

data Rights = Rights {
    rightsOwner           :: Address
  , rightsManagementProxy :: Maybe Address
  , rightsSpawnProxy      :: Maybe Address
  , rightsVotingProxy     :: Maybe Address
  , rightsTransferProxy   :: Maybe Address
  }
  deriving stock (Show, Eq, Generic)

newtype CryptKey = CryptKey {
    fromCryptKey :: Solidity.Prim.BytesN 32
  }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

newtype AuthKey = AuthKey {
    fromAuthKey :: Solidity.Prim.BytesN 32
  }
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

newtype CryptoSuite = CryptoSuite {
    fromCryptoSuite :: Solidity.Prim.UIntN 32
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Bounded, Integral)

newtype Life = Life {
    fromLife :: Solidity.Prim.UIntN 32
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Bounded, Integral)

newtype Rift = Rift {
    fromRift :: Solidity.Prim.UIntN 32
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum, Bounded, Integral)

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
       detailsCryptKey /= mempty
    && detailsAuthKey /= mempty
    && detailsCryptoSuite /= 0
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

-- | Grab a point's key information.
keyInformation :: Point -> Keys
keyInformation Point {..} = Keys {
      keyCrypt       = detailsCryptKey
    , keyAuth        = detailsAuthKey
    , keyCryptoSuite = detailsCryptoSuite
    }
  where
    Details {..} = pointDetails
