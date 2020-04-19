module Urbit.Azimuth
    ( Web3    (..)
    , runWeb3

    , Azimuth
    , runAzimuth
    , runAzimuthWith

    , Contract (..)
    , getContract

    , Quantity
    , getLatestBlock

    , Point   (..)
    , getPoint

    , Rights  (..)
    , getRights
    ) where

import Control.Monad.Catch        (MonadThrow)
import Control.Monad.Except       (ExceptT, MonadError)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.State.Strict (MonadState, StateT)

import Network.Ethereum.Account      (DefaultAccount)
import Network.Ethereum.Api.Provider (Web3Error (..))
import Network.Ethereum.Api.Types    (Quantity)
import Network.JsonRpc.TinyClient    (JsonRpcClient)

import Prelude

import qualified Control.Exception                 as Exception
import qualified Control.Monad.Except              as Except
import qualified Control.Monad.State.Strict        as State
import qualified Data.Solidity.Prim                as Solidity.Prim
import qualified Network.Ethereum.Account          as Ethereum.Account
import qualified Network.Ethereum.Account.Internal as Ethereum.Account.Internal
import qualified Network.Ethereum.Api.Eth          as Ethereum.Eth
import qualified Network.Ethereum.Api.Types        as Ethereum.Types
import qualified Network.Ethereum.Ens              as Ethereum.Ens
import qualified Network.JsonRpc.TinyClient        as Web3.Client
import qualified Urbit.Azimuth.Contract

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

runWeb3
    :: JsonRpcClient
    -> Web3 a
    -> IO a
runWeb3 client (Web3 action) =
    State.evalStateT (Except.runExceptT action) client >>= \case
        Left  err -> Exception.throwIO (UserFail err)
        Right ok  -> pure ok

-- The functions that use an account are just specialised to 'DefaultAccount'
-- for now to avoid the batshit constraints of the @web3@ library.
type Azimuth = DefaultAccount Web3

runAzimuth
    :: Azimuth a
    -> Web3 a
runAzimuth action = do
    contract <- getContract
    block    <- getLatestBlock

    runAzimuthWith contract block action

runAzimuthWith
    :: Contract
    -> Quantity
    -> Azimuth a
    -> Web3 a
runAzimuthWith (Contract address) block action =
    Ethereum.Account.withAccount () $
        Ethereum.Account.withParam (\param ->
            param { Ethereum.Account.Internal._to    = Just address
                  , Ethereum.Account.Internal._block = Ethereum.Types.BlockWithNumber block
                  }) action

-- | The ethereum address of the @azimuth.eth@ contract.
newtype Contract = Contract Solidity.Prim.Address
    deriving stock (Show, Eq)

getContract
    :: Web3 Contract
getContract  =
    Contract <$>
        Ethereum.Account.withAccount () (Ethereum.Ens.resolve "azimuth.eth")

getLatestBlock
    :: Web3 Quantity
getLatestBlock =
    Ethereum.Eth.blockNumber

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
    } deriving stock (Show, Eq)

getPoint
    :: Solidity.Prim.UIntN 32
    -> Azimuth Point
getPoint point = do
    ( pointEncryptionKey
        , pointAuthenticationKey
        , pointHasSponsor
        , pointActive
        , pointEscapeRequested
        , pointSponsor
        , pointEscapeRequestedTo
        , pointCryptoSuiteVersion
        , pointKeyRevisionNumber
        , pointContinuityNumber
        ) <- Urbit.Azimuth.Contract.points point

    pure Point{..}

data Rights = Rights
    { rightsOwner           :: Solidity.Prim.Address
    , rightsManagementProxy :: Solidity.Prim.Address
    , rightsSpawnProxy      :: Solidity.Prim.Address
    , rightsVotingProxy     :: Solidity.Prim.Address
    , rightsTransferProxy   :: Solidity.Prim.Address
    } deriving stock (Show, Eq)

getRights
    :: Solidity.Prim.UIntN 32
    -> Azimuth Rights
getRights point = do
    ( rightsOwner
        , rightsManagementProxy
        , rightsSpawnProxy
        , rightsVotingProxy
        , rightsTransferProxy
        ) <- Urbit.Azimuth.Contract.rights point

    pure Rights{..}
