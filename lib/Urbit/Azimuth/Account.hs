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

-- | Recover a private key from a BIP39 mnemonic, passphrase, and HD derivation
--   path.
--
--   Note that you can use the monoid unit, 'mempty', to denote an empty or
--   absent passphrase.
--
--   >>> let mnem = "benefit crew supreme gesture quantum web media hazard theory mercy wing kitten"
--   >>> let hdpath = Deriv :| 44 :| 60 :| 0 :/ 0 :/ 0 -- m/44'/60'/0'/0/0
--   >>> toPrivateKey mnem mempty hdpath
toPrivateKey
  :: K.Mnemonic
  -> K.Passphrase
  -> K.DerivPath
  -> Either String A.LocalKey
toPrivateKey mnem pass path = do
  seed <- K.mnemonicToSeed pass mnem
  pure . flip A.LocalKey 1 -- NB this targets mainnet only
    . C.importKey
    . K.getSecKey
    . K.xPrvKey
    . K.derivePath path
    . K.makeXPrvKey
    $ seed

