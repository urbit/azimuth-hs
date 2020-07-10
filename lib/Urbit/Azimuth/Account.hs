module Urbit.Azimuth.Account (
    A.Account(..)
  , A.LocalKey(..)
  , A.LocalKeyAccount(..)

  , C.PrivateKey(..)
  , C.PublicKey(..)
  , C.importKey
  , C.derivePubKey

  , K.DerivPath(..)
  , K.DerivPathI(..)
  , K.ParsedPath(..)
  , K.parsePath

  , K.Mnemonic
  , K.Passphrase

  , getLocalKey
  ) where

import qualified Crypto.Ethereum as C
import qualified Haskoin.Keys as K
import qualified Network.Ethereum.Account as A

getLocalKey
  :: K.Mnemonic
  -> K.Passphrase
  -> K.DerivPath
  -> Either String A.LocalKey
getLocalKey mnem pass path = do
  seed <- K.mnemonicToSeed pass mnem
  pure . flip A.LocalKey 1 -- FIXME this targets mainnet only!
    . C.importKey
    . K.getSecKey
    . K.xPrvKey
    . K.derivePath path
    . K.makeXPrvKey
    $ seed

