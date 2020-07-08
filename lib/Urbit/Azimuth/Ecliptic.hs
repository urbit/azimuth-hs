
module Urbit.Azimuth.Ecliptic (
    RevisionType(..)

  , configureKeys
  ) where

import Network.Ethereum.Account (LocalKeyAccount)
import qualified Network.Ethereum.Api.Types as Api
import Network.JsonRpc.TinyClient (JsonRpc)
import Urbit.Azimuth.Contract
import qualified Urbit.Azimuth.Ecliptic.Internal as I
import Urbit.Azimuth.Point
import qualified Urbit.Ob as Ob
import qualified Urbit.Ob.Extended as Ob

data RevisionType =
    Rotate
  | Breach
  deriving (Eq, Show)

configureKeys
  :: (JsonRpc m, MonadFail m)
  => Ob.Patp
  -> CryptKey
  -> AuthKey
  -> CryptoSuite
  -> RevisionType
  -> Azimuth (LocalKeyAccount m) Api.TxReceipt
configureKeys patp (CryptKey ck) (AuthKey ak) (CryptoSuite cs) breach =
  withContract ecliptic $
    I.configureKeys (Ob.patpToPoint patp) ck ak cs (breach == Breach)
