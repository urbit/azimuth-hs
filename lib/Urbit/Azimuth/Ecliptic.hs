{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Urbit.Azimuth.Ecliptic (
    RevisionType(..)
  , Approval(..)
  , Reset(..)

  , exists
  , getApproved
  , isApprovedForAll
  , getSpawnLimit
  , canEscapeTo
  , configureKeys
  , createGalaxy
  , safeTransferFrom
  , transferFrom
  , approve
  , setApprovalForAll
  , setManagementProxy
  , spawn
  , setSpawnProxy
  , transferPoint
  , setTransferProxy
  , escape
  , cancelEscape
  , adopt
  , reject
  , detach
  , setVotingProxy
  , setDnsDomains
  ) where

import qualified Data.Text as T
import qualified Network.Ethereum.Api.Types as Api
import Network.JsonRpc.TinyClient (JsonRpc)
import Numeric.Natural
import Urbit.Azimuth.Contract
import qualified Urbit.Azimuth.Ecliptic.Internal as I
import Urbit.Azimuth.Point
import qualified Urbit.Ob as Ob
import qualified Urbit.Ob.Extended as Ob

-- | Key revision options.
data RevisionType =
    Rotate   -- ^ Assign new keys
  | Breach   -- ^ Assign new keys, breaching continuity in the process
  deriving stock (Eq, Show)

-- | Operator approval options.
data Approval =
    Approved   -- ^ Operator is allowed to transfer ownership of all points
  | Disallowed -- ^ Operator is not allowed to transfer ownership of all points
  deriving stock (Eq, Show)

-- | Point transfer options.
data Reset =
    Preserve -- ^ Preserve existing key and permissions information
  | Clear    -- ^ Reset (clear) existing key and permissions information
  deriving stock (Eq, Show)

-- | Check if a point is active.
exists :: JsonRpc m => Ob.Patp -> Azimuth m Bool
exists patp = withContract ecliptic $ do
  let point = Ob.patpToSolidity256 patp
  I.exists point

-- | Get the approved transfer proxy for a point.
getApproved :: JsonRpc m => Ob.Patp -> Azimuth m Address
getApproved patp = withContract ecliptic $ do
  let point = Ob.patpToSolidity256 patp
  I.getApproved point

-- | Check if an address is an operator for an owner.
isApprovedForAll
  :: JsonRpc m
  => Address
  -> Address
  -> Azimuth m Bool
isApprovedForAll owner operator = withContract ecliptic $
  I.isApprovedForAll owner operator

-- | Get the total number of children a point can spawn at some time.
getSpawnLimit
  :: JsonRpc m
  => Ob.Patp
  -> Natural
  -> Azimuth m Natural
getSpawnLimit patp time = withContract ecliptic $ do
  let point = Ob.patpToPoint patp
  limit <- I.getSpawnLimit point (fromIntegral time)
  pure $ fromIntegral limit

-- | Check if a point can escape to a sponsor.
canEscapeTo
  :: JsonRpc m
  => Ob.Patp
  -> Ob.Patp
  -> Azimuth m Bool
canEscapeTo point sponsor = withContract ecliptic $ do
  let pt = Ob.patpToPoint point
      sp = Ob.patpToPoint sponsor
  I.canEscapeTo pt sp

-- | Safely transfer a point between addresses (call recipient if it's a
--   contract).
safeTransferFrom
  :: JsonRpc m
  => Address
  -> Address
  -> Ob.Patp
  -> Azimuth m Api.TxReceipt
safeTransferFrom from to point = withContract ecliptic $ do
  let pt = Ob.patpToSolidity256 point
  I.safeTransferFrom from to pt

-- | Transfer a point between addresses (without notifying recipient contract).
transferFrom
  :: JsonRpc m
  => Address
  -> Address
  -> Ob.Patp
  -> Azimuth m Api.TxReceipt
transferFrom from to point = withContract ecliptic $ do
  let pt = Ob.patpToSolidity256 point
  I.transferFrom from to pt

-- | Approve an address to transfer ownership of a point.
approve
  :: JsonRpc m
  => Address
  -> Ob.Patp
  -> Azimuth m Api.TxReceipt
approve addr point = withContract ecliptic $ do
  let pt = Ob.patpToSolidity256 point
  I.approve addr pt

-- | Allow or disallow an operator to transfer ownership of all points owned by
--   the message sender.
setApprovalForAll
  :: JsonRpc m
  => Address
  -> Approval
  -> Azimuth m Api.TxReceipt
setApprovalForAll addr approval = withContract ecliptic $
  I.setApprovalForAll addr (approval == Approved)

-- | Configure the management address for a point owned by the message sender.
setManagementProxy
  :: JsonRpc m
  => Ob.Patp
  -> Address
  -> Azimuth m Api.TxReceipt
setManagementProxy patp addr = withContract ecliptic $ do
  let pt = Ob.patpToPoint patp
  I.setManagementProxy pt addr

-- | Configure a point's keys, optionally incrementing the continuity number.
configureKeys
  :: JsonRpc m
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

-- | Spawn a point, giving ownership of it to the target address.
spawn
  :: JsonRpc m
  => Ob.Patp
  -> Address
  -> Azimuth m Api.TxReceipt
spawn point addr = withContract ecliptic $ do
  let pt = Ob.patpToPoint point
  I.spawn pt addr

-- | Give an address the right to spawn points with the given prefix.
setSpawnProxy
  :: JsonRpc m
  => Ob.Patp
  -> Address
  -> Azimuth m Api.TxReceipt
setSpawnProxy patp addr = case Ob.patpToSolidity16 patp of
  -- NB a proper way to handle this sort of error would be nice
  Left _    -> error (show patp <> " is not a star or galaxy")
  Right pt  -> withContract ecliptic $ I.setSpawnProxy pt addr

-- | Transfer a point between addresses (without notifying recipient contract).
transferPoint
  :: JsonRpc m
  => Ob.Patp
  -> Address
  -> Reset
  -> Azimuth m Api.TxReceipt
transferPoint point addr reset = withContract ecliptic $ do
  let pt = Ob.patpToPoint point
  I.transferPoint pt addr (reset == Clear)

-- | Configure the transfer proxy address for a point owned by the message
--   sender.
setTransferProxy
  :: JsonRpc m
  => Ob.Patp
  -> Address
  -> Azimuth m Api.TxReceipt
setTransferProxy patp addr = withContract ecliptic $ do
  let pt = Ob.patpToPoint patp
  I.setTransferProxy pt addr

-- | Request escape from 'point' to 'sponsor'.
escape
  :: JsonRpc m
  => Ob.Patp
  -> Ob.Patp
  -> Azimuth m Api.TxReceipt
escape point sponsor = withContract ecliptic $ do
  let pt = Ob.patpToPoint point
      sp = Ob.patpToPoint sponsor
  I.escape pt sp

-- | Cancel a point's escape request.
cancelEscape
  :: JsonRpc m
  => Ob.Patp
  -> Azimuth m Api.TxReceipt
cancelEscape point = withContract ecliptic $ do
  let pt = Ob.patpToPoint point
  I.cancelEscape pt

-- | As a sponsor, accept a point's escape request.
adopt
  :: JsonRpc m
  => Ob.Patp
  -> Azimuth m Api.TxReceipt
adopt escapee = withContract ecliptic $ do
  let pt = Ob.patpToPoint escapee
  I.adopt pt

-- | As a sponsor, reject a point's escape request.
reject
  :: JsonRpc m
  => Ob.Patp
  -> Azimuth m Api.TxReceipt
reject escapee = withContract ecliptic $ do
  let pt = Ob.patpToPoint escapee
  I.reject pt

-- | As a sponsor, stop sponsoring a point.
detach
  :: JsonRpc m
  => Ob.Patp
  -> Azimuth m Api.TxReceipt
detach escapee = withContract ecliptic $ do
  let pt = Ob.patpToPoint escapee
  I.detach pt

-- | Configure the voting proxy address for a point owned by the message
--   sender.
setVotingProxy
  :: JsonRpc m
  => Ob.Patp
  -> Address
  -> Azimuth m Api.TxReceipt
setVotingProxy patp addr = case Ob.patpToGalaxy patp of
  -- NB a proper way to handle this sort of error would be nice
  Left _    -> error (show patp <> " is not a galaxy")
  Right gal -> withContract ecliptic $ I.setVotingProxy gal addr

-- | Set primary, secondary, and tertiary DNS domains for the ecliptic.
setDnsDomains
  :: JsonRpc m
  => T.Text
  -> T.Text
  -> T.Text
  -> Azimuth m Api.TxReceipt
setDnsDomains prim seco tert = withContract ecliptic $
  I.setDnsDomains prim seco tert

-- | Create the specified galaxy.
createGalaxy
  :: JsonRpc m
  => Ob.Patp
  -> Address
  -> Azimuth m Api.TxReceipt
createGalaxy patp addr = case Ob.patpToGalaxy patp of
  -- NB a proper way to handle this sort of error would be nice
  Left _    -> error (show patp <> " is not a galaxy")
  Right gal -> withContract ecliptic $ I.createGalaxy gal addr
