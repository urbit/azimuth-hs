module Urbit.Azimuth
    (
    -- * Running Web3 Actions
      Web3(..)
    , runWeb3

    -- ** Latest Block
    , Quantity
    , getLatestBlock

    -- ** Resolving Contracts
    , Contracts(..)
    , getContracts

    -- * Running Azimuth Contracts
    , Azimuth(..)
    , runAzimuth

    -- * Additional Type Wrappers
    , CryptKey(..)
    , AuthKey(..)
    , CryptoSuite(..)

    -- * azimuth.eth

    -- ** Point
    , Point(..)
    , getPoint
    , hasNetworkKeys

    -- ** Rights
    , Rights(..)
    , getRights
    , isOwner
    , isManagementProxy
    , isSpawnProxy
    , isVotingProxy
    , isTransferProxy

    -- ** Unify Point and Rights
    , Details(..)
    , getDetails
    , canManageNetworkKeys
    , canManage
    , canSpawn
    , canVote
    , canTransfer

    -- * ecliptic.eth
    , Breach(..)
    , configureKeys
    )
where

import Control.Monad.Catch           (MonadThrow)
import Control.Monad.Except          (ExceptT, MonadError)
import Control.Monad.IO.Class        (MonadIO (liftIO))
import Control.Monad.Reader          (ReaderT)
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
import qualified Control.Monad.Reader              as Reader
import qualified Control.Monad.State.Strict        as State
import qualified Data.Default.Class                as Default
import qualified Data.Solidity.Prim                as Solidity.Prim
import qualified Network.Ethereum.Account          as Ethereum.Account
import qualified Network.Ethereum.Account.Internal as Ethereum.Account.Internal
import qualified Network.Ethereum.Api.Eth          as Ethereum.Eth
import qualified Network.Ethereum.Api.Types        as Ethereum.Types
import qualified Network.Ethereum.Ens              as Ethereum.Ens
import qualified Network.JsonRpc.TinyClient        as Web3.Client
import qualified Urbit.Azimuth.Azimuth             as Azimuth
import qualified Urbit.Azimuth.Ecliptic            as Ecliptic
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

getLatestBlock :: Web3 Quantity
getLatestBlock = Ethereum.Eth.blockNumber

-- Azimuth: azimuth.eth / 0x223c067f8cf28ae173ee5cafea60ca44c335fecb
-- Ecliptic: ecliptic.eth / 0x6ac07b7c4601b5ce11de8dfe6335b871c7c4dd4d
-- Polls: 0x7fecab617c868bb5996d99d95200d2fa708218e4
-- Linear Star Release: 0x86cd9cd0992f04231751e3761de45cecea5d1801
-- Conditional Star Release: 0x8c241098c3d3498fe1261421633fd57986d74aea
-- Claims: 0xe7e7f69b34d7d9bd8d61fb22c33b22708947971a
-- Censures: 0x325f68d32bdee6ed86e7235ff2480e2a433d6189
-- Delegated Sending: 0xf6b461fe1ad4bd2ce25b23fe0aff2ac19b3dfa76

data Contracts = Contracts
    { azimuth  :: Solidity.Prim.Address
    , ecliptic :: Solidity.Prim.Address
    }

getContracts :: Web3 Contracts
getContracts = do
    let resolve = Ethereum.Account.withAccount () . Ethereum.Ens.resolve

    azimuth  <- resolve "azimuth.eth"
    ecliptic <- resolve "ecliptic.eth"

    pure Contracts { .. }

-- The functions that use an account are just specialised to 'DefaultAccount'
-- for now to avoid the batshit constraints of the @web3@ library.
newtype Azimuth a = Azimuth (ReaderT (Contracts, Quantity) Web3 a)
    deriving newtype (Functor, Applicative, Monad)

instance MonadIO Azimuth where
    liftIO = Azimuth . liftIO

runAzimuth :: Contracts -> Quantity -> Azimuth a -> Web3 a
runAzimuth contracts block (Azimuth action) =
    Reader.runReaderT action (contracts, block)

withContract
    :: (Contracts -> Solidity.Prim.Address)
    -> DefaultAccount Web3 a
    -> Azimuth a
withContract selector action = Azimuth $ do
    (contracts, block) <- Reader.ask

    lift $ Ethereum.Account.withAccount () $ Ethereum.Account.withParam
        (\param -> param
            { Ethereum.Account.Internal._to    = Just (selector contracts)
            , Ethereum.Account.Internal._block = Ethereum.Types.BlockWithNumber
                block
            }
        )
        action

-- Extra type wrappings

newtype CryptKey = CryptKey { fromCryptKey :: Solidity.Prim.BytesN 32 }
    deriving stock (Show, Eq)

newtype AuthKey = AuthKey { fromAuthKey :: Solidity.Prim.BytesN 32 }
    deriving stock (Show, Eq)

newtype CryptoSuite = CryptoSuite { fromCryptoSuite :: Solidity.Prim.UIntN 32 }
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num, Real, Enum, Bounded, Integral)

-- azimuth.eth

-- azimuth.points

data Point = Point
    { pointCryptKey          :: CryptKey
    , pointAuthKey           :: AuthKey
    , pointHasSponsor        :: Bool
    , pointActive            :: Bool
    , pointEscapeRequested   :: Bool
    , pointSponsor           :: Solidity.Prim.UIntN 32
    , pointEscapeRequestedTo :: Solidity.Prim.UIntN 32
    , pointCryptoSuite       :: CryptoSuite
    , pointKeyRevision       :: Solidity.Prim.UIntN 32
    , pointContinuity        :: Solidity.Prim.UIntN 32
    }
    deriving stock (Show, Eq, Generic)

getPoint :: Patp -> Azimuth Point
getPoint ship = withContract azimuth $ do
    (CryptKey -> pointCryptKey, AuthKey -> pointAuthKey, pointHasSponsor, pointActive, pointEscapeRequested, pointSponsor, pointEscapeRequestedTo, CryptoSuite -> pointCryptoSuite, pointKeyRevision, pointContinuity) <-
        Azimuth.points (shipToPoint ship)

    pure Point { .. }

hasNetworkKeys :: Point -> Bool
hasNetworkKeys point = pointKeyRevision point > 0

-- azimuth.rights

data Rights = Rights
    { rightsOwner           :: Solidity.Prim.Address
    , rightsManagementProxy :: Maybe Solidity.Prim.Address
    , rightsSpawnProxy      :: Maybe Solidity.Prim.Address
    , rightsVotingProxy     :: Maybe Solidity.Prim.Address
    , rightsTransferProxy   :: Maybe Solidity.Prim.Address
    }
    deriving stock (Show, Eq, Generic)

getRights :: Patp -> Azimuth Rights
getRights ship = withContract azimuth $ do
    let
        unzero x
            | isZeroAddress x = Nothing
            | otherwise       = Just x

    (rightsOwner, unzero -> rightsManagementProxy, unzero -> rightsSpawnProxy, unzero -> rightsVotingProxy, unzero -> rightsTransferProxy) <-
        Azimuth.rights (shipToPoint ship)

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

-- ecliptic.eth

data Breach
    = Discontinuous
    | Continuous
    deriving stock (Show, Eq)

configureKeys
    :: Patp
    -> CryptKey
    -> AuthKey
    -> CryptoSuite
    -> Breach
    -> Azimuth Ethereum.Types.TxReceipt
configureKeys ship crypt auth suite breach =
    withContract ecliptic $ Ecliptic.configureKeys
        (shipToPoint ship)
        (fromCryptKey crypt)
        (fromAuthKey auth)
        (fromCryptoSuite suite)
        (breach == Discontinuous)

-- Utilities

shipToPoint :: Patp -> Solidity.Prim.UIntN 32
shipToPoint = fromIntegral . Urbit.Ob.fromPatp

isZeroAddress :: Address -> Bool
isZeroAddress = (==) Default.def

isParent :: Patp -> Bool
isParent ship = case Urbit.Ob.clan ship of
    Urbit.Ob.Galaxy -> True
    Urbit.Ob.Star   -> True
    _               -> False
