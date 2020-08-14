{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Urbit.Azimuth.Account (
    A.Account(..)
  , A.LocalKey(..)
  , A.LocalKeyAccount
  , A.PersonalAccount
  , A.DefaultAccount

  , Unlockable

  , C.PrivateKey
  , C.PublicKey
  , C.importKey
  , C.derivePubKey

  , K.DerivPath
  , K.DerivPathI(..)
  , K.ParsedPath(..)
  , K.parsePath

  , K.Mnemonic
  , K.Passphrase

  , toPrivateKey
  ) where

import qualified Crypto.Ethereum as C
import qualified Network.Haskoin.Keys as K
import qualified Network.Ethereum.Account as A
import qualified Network.Ethereum.Account.Internal as AI
import Network.JsonRpc.TinyClient (JsonRpc)

type Unlockable p m = (JsonRpc m, A.Account p (AI.AccountT p))

-- | Recover a private key from a BIP39 mnemonic, passphrase, HD derivation
--   path, and chain ID.
--
--   Note that you can use the monoid unit, 'mempty', to denote an empty or
--   absent passphrase.  It's also worth noting that you'll typically want to
--   use a chain ID of '1' (i.e., the Ethereum mainnet).
--
--   >>> let mnem = "benefit crew supreme gesture quantum web media hazard theory mercy wing kitten"
--   >>> let hdpath = "m/44'/60'/0'/0/0" :: DerivPath
--   >>> toPrivateKey mnem mempty hdpath 1
toPrivateKey
  :: K.Mnemonic
  -> K.Passphrase
  -> K.DerivPath
  -> Integer
  -> Either String A.LocalKey
toPrivateKey mnem pass path chainId = do
  seed <- K.mnemonicToSeed pass mnem
  pure . flip A.LocalKey chainId
    . C.importKey
    . K.getSecKey
    . K.xPrvKey
    . K.derivePath path
    . K.makeXPrvKey
    $ seed

