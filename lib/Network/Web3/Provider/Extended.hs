{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Network.Web3.Provider.Extended (
    Web3(..)
  , Web3Error(..)
  , runWeb3

  , Web3.Client.defaultSettings
  ) where

import qualified Control.Exception as Exception
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (ExceptT, MonadError)
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (MonadState, StateT)
import Network.Web3.Provider (Web3Error(..))
import Network.JsonRpc.TinyClient (JsonRpcClient)
import qualified Network.JsonRpc.TinyClient as Web3.Client

-- | A wrapper for the 'Ethereum.Provider.Web3' monad that provides a
--   'MonadFail' instance.
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

