module Urbit.Azimuth
    (
    -- * Running Azimuth
      Web3(..)
    , runWeb3
    , Azimuth
    , runAzimuth

    -- * Contract
    , Contract(..)
    , getContract

    -- * Latest Block
    , Quantity
    , getLatestBlock

    -- * Point
    , Point(..)
    , getPoint
    , hasNetworkKeys

    -- * Rights
    , Rights(..)
    , getRights
    , isOwner
    , isManagementProxy
    , isSpawnProxy
    , isVotingProxy
    , isTransferProxy

    -- * Unify Point and Rights
    , Details(..)
    , getDetails
    , canManageNetworkKeys
    , canManage
    , canSpawn
    , canVote
    , canTransfer
    )
where

import Control.Monad.Catch           (MonadThrow)
import Control.Monad.Except          (ExceptT, MonadError)
import Control.Monad.IO.Class        (MonadIO (liftIO))
import Control.Monad.State.Strict    (MonadState, StateT)
import Control.Monad.Trans           (lift)
import Data.Solidity.Prim.Address    (Address)
import GHC.Generics                  (Generic)
import Network.Ethereum.Account      (DefaultAccount)
import Network.Ethereum.Api.Provider (Web3Error (..))
import Network.Ethereum.Api.Types    (Quantity)
import Network.JsonRpc.TinyClient    (JsonRpcClient)
import Prelude
import Urbit.Ob                      (Patp)

import qualified Control.Exception                 as Exception
import qualified Control.Monad.Except              as Except
import qualified Control.Monad.State.Strict        as State
import qualified Data.Default.Class                as Default
import qualified Data.Solidity.Prim                as Solidity.Prim
import qualified Network.Ethereum.Account          as Ethereum.Account
import qualified Network.Ethereum.Account.Internal as Ethereum.Account.Internal
import qualified Network.Ethereum.Api.Eth          as Ethereum.Eth
import qualified Network.Ethereum.Api.Types        as Ethereum.Types
import qualified Network.Ethereum.Ens              as Ethereum.Ens
import qualified Network.JsonRpc.TinyClient        as Web3.Client
import qualified Urbit.Azimuth.Contract
import qualified Urbit.Ob

-- | A wrapper for the 'Ethereum.Provider.Web3' monad that provides a
-- 'MonadFail' instance to satisfy the functions in 'Urbit.Azimuth' generated
-- from the ethereum contract.
newtype Web3 a = Web3 (ExceptT String (StateT JsonRpcClient IO) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadError String
        , MonadState JsonRpcClient
        )

instance MonadFail Web3 where
    fail = Web3 . Except.liftEither . Left

instance Web3.Client.JsonRpc Web3

runWeb3 :: JsonRpcClient -> Web3 a -> IO a
runWeb3 client (Web3 action) =
    State.evalStateT (Except.runExceptT action) client >>= \case
        Left  err -> Exception.throwIO (UserFail err)
        Right ok  -> pure ok

-- The functions that use an account are just specialised to 'DefaultAccount'
-- for now to avoid the batshit constraints of the @web3@ library.
type Azimuth = DefaultAccount Web3

instance MonadIO Azimuth where
    liftIO = lift . liftIO

runAzimuth :: Contract -> Quantity -> Azimuth a -> Web3 a
runAzimuth (Contract address) block action =
    Ethereum.Account.withAccount () $ Ethereum.Account.withParam
        (\param -> param
            { Ethereum.Account.Internal._to    = Just address
            , Ethereum.Account.Internal._block = Ethereum.Types.BlockWithNumber
                block
            }
        )
        action

-- | The ethereum address of the @azimuth.eth@ contract.
newtype Contract = Contract Solidity.Prim.Address
    deriving stock (Show, Eq)

getContract :: Web3 Contract
getContract = Contract
    <$> Ethereum.Account.withAccount () (Ethereum.Ens.resolve "azimuth.eth")

getLatestBlock :: Web3 Quantity
getLatestBlock = Ethereum.Eth.blockNumber

-- .points

data Point = Point
    { pointEncryptionKey      :: Solidity.Prim.BytesN 32
    , pointAuthenticationKey  :: Solidity.Prim.BytesN 32
    , pointHasSponsor         :: Bool
    , pointActive             :: Bool
    , pointEscapeRequested    :: Bool
    , pointSponsor            :: Solidity.Prim.UIntN 32
    , pointEscapeRequestedTo  :: Solidity.Prim.UIntN 32
    , pointCryptoSuiteVersion :: Solidity.Prim.UIntN 32
    , pointKeyRevisionNumber  :: Solidity.Prim.UIntN 32
    , pointContinuityNumber   :: Solidity.Prim.UIntN 32
    }
    deriving stock (Show, Eq, Generic)

getPoint :: Patp -> Azimuth Point
getPoint ship = do
    (pointEncryptionKey, pointAuthenticationKey, pointHasSponsor, pointActive, pointEscapeRequested, pointSponsor, pointEscapeRequestedTo, pointCryptoSuiteVersion, pointKeyRevisionNumber, pointContinuityNumber) <-
        Urbit.Azimuth.Contract.points (fromIntegral (Urbit.Ob.fromPatp ship))

    pure Point { .. }

hasNetworkKeys :: Point -> Bool
hasNetworkKeys point = pointKeyRevisionNumber point > 0

-- .rights

data Rights = Rights
    { rightsOwner           :: Solidity.Prim.Address
    , rightsManagementProxy :: Maybe Solidity.Prim.Address
    , rightsSpawnProxy      :: Maybe Solidity.Prim.Address
    , rightsVotingProxy     :: Maybe Solidity.Prim.Address
    , rightsTransferProxy   :: Maybe Solidity.Prim.Address
    }
    deriving stock (Show, Eq, Generic)

getRights :: Patp -> Azimuth Rights
getRights ship = do
    let
        unzero x
            | isZeroAddress x = Nothing
            | otherwise       = Just x

    (rightsOwner, unzero -> rightsManagementProxy, unzero -> rightsSpawnProxy, unzero -> rightsVotingProxy, unzero -> rightsTransferProxy) <-
        Urbit.Azimuth.Contract.rights (fromIntegral (Urbit.Ob.fromPatp ship))

    pure Rights { .. }

isOwner :: Rights -> Address -> Bool
isOwner rights = (==) (rightsOwner rights)

isManagementProxy :: Rights -> Address -> Bool
isManagementProxy rights = (==) (rightsManagementProxy rights) . Just

isSpawnProxy :: Rights -> Address -> Bool
isSpawnProxy rights = (==) (rightsSpawnProxy rights) . Just

isVotingProxy :: Rights -> Address -> Bool
isVotingProxy rights = (==) (rightsVotingProxy rights) . Just

isTransferProxy :: Rights -> Address -> Bool
isTransferProxy rights = (==) (rightsTransferProxy rights) . Just

-- Unify .points + .rights calls to keep ship+point+rights consistent

data Details = Details
    { detailsShip   :: Patp
    , detailsPoint  :: Point
    , detailsRights :: Rights
    }
    deriving stock (Show, Eq, Generic)

getDetails :: Patp -> Azimuth Details
getDetails ship = Details ship <$> getPoint ship <*> getRights ship

-- The can* series of functions uses Details to provide a consistent interface,
-- even if it violates the law of Demeter.

canManageNetworkKeys :: Details -> Address -> Bool
canManageNetworkKeys details@Details { detailsPoint } address =
    canManage details address && hasNetworkKeys detailsPoint

canManage :: Details -> Address -> Bool
canManage Details { detailsRights } address =
    isOwner detailsRights address || isManagementProxy detailsRights address

canSpawn :: Details -> Address -> Bool
canSpawn Details { detailsShip, detailsPoint, detailsRights } address =
    isParent detailsShip
        && hasNetworkKeys detailsPoint
        && (isOwner detailsRights address || isSpawnProxy detailsRights address)

canVote :: Details -> Address -> Bool
canVote Details { detailsShip, detailsPoint, detailsRights } address =
    (Urbit.Ob.Galaxy == Urbit.Ob.clan detailsShip)
        && pointActive detailsPoint
        && (isOwner detailsRights address || isVotingProxy detailsRights address
           )

canTransfer :: Details -> Address -> Bool
canTransfer Details { detailsRights } address =
    isOwner detailsRights address || isTransferProxy detailsRights address

-- Utilities

isZeroAddress :: Address -> Bool
isZeroAddress = (==) Default.def

isParent :: Patp -> Bool
isParent ship = case Urbit.Ob.clan ship of
    Urbit.Ob.Galaxy -> True
    Urbit.Ob.Star   -> True
    _               -> False

