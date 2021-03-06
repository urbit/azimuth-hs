{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Shelly as S
import Test.Hspec
import qualified Urbit.Azimuth as A
import qualified Urbit.Ob as Ob

ginit :: IO T.Text
ginit = S.shelly $ S.verbosely $ do
  pod <- S.run "docker" [
      "run"
    , "-d"
    , "-p", "8545:8545"
    , "trufflesuite/ganache-cli:latest"
    , "--networkId", "1"
    , "-m", "'benefit crew supreme gesture quantum web media hazard " <>
            "theory mercy wing kitten'"
    ]

  -- give it a sec to get going
  _ <- S.run_ "sleep" [ "5" ]

  pure (T.takeWhile (/= '\n') pod)

gkill :: T.Text -> IO ()
gkill pod = S.shelly $ S.verbosely $
  S.run_ "docker" [
      "stop"
    , pod
    ]

-- FIXME need to add a contract deployment action here
withGanache :: IO () -> IO ()
withGanache action = bracket ginit gkill (const action)

data Config = Config {
    endpt        :: String
  , mnem         :: A.Mnemonic
  , pbase        :: A.DerivPath
  , pk0          :: A.LocalKey
  , pk1          :: A.LocalKey
  , pk2          :: A.LocalKey
  , contracts    :: A.Contracts
  }

testConfig :: Config
testConfig = Config {..} where
  endpt        = "http://localhost:8545"
  pbase        = A.Deriv A.:| 44 A.:| 60 A.:| 0 A.:/ 0
  Right pk0    = A.toPrivateKey mnem mempty (pbase A.:/ 0) 1
  Right pk1    = A.toPrivateKey mnem mempty (pbase A.:/ 1) 1
  Right pk2    = A.toPrivateKey mnem mempty (pbase A.:/ 2) 1

  mnem         = "benefit crew supreme gesture quantum web media hazard " <>
                 "theory mercy wing kitten"

  contracts    = A.Contracts {
      azimuth  = "0x863d9c2e5c4c133596cfac29d55255f0d0f86381"
    , ecliptic = "0x56db68f29203ff44a803faa2404a44ecbb7a7480"
    }

main :: IO ()
main = do
  let Config {..} = testConfig
      zod         = Ob.patp 0
      nec         = Ob.patp 1
      bud         = Ob.patp 2
      wes         = Ob.patp 3
      nidsut      = Ob.patp 15663360

      bytes = "0x7768617465766572000000000000000000000000000000000000000000000000"

      keys  = A.Keys {
          keyCrypt       = A.CryptKey bytes
        , keyAuth        = A.AuthKey bytes
        , keyCryptoSuite = A.CryptoSuite 1
        }

      getRift = A.detailsRift . A.pointDetails
      getLife = A.detailsLife . A.pointDetails

  endpoint <- A.defaultSettings endpt

  -- NB eventually this should use the 'around_ withGanache' context, but a
  --    contract deployment action will need to be added first.
  --
  --    until then, you should run these tests by booting a ganache node in
  --    the background and deploying the contracts with truffle.  you may
  --    want to use the 'test:setup' and 'test:cleanup' npm actions in
  --    azimuth-js.

  hspec $ do
    it "creates galaxies properly" $ do
      (z0, n0) <- A.runWeb3 endpoint $
        A.runAzimuth contracts pk0 $ do
          zp <- A.getPoint zod
          np <- A.getPoint nec
          pure (zp, np)

      A.pointDetails z0 `shouldNotSatisfy` A.detailsActive
      A.pointDetails n0 `shouldNotSatisfy` A.detailsActive

      (z1, n1) <- A.runWeb3 endpoint $ do
        A.runAzimuth contracts pk0 $
          A.createGalaxy zod "0x6DEfFb0caFDB11D175F123F6891AA64F01c24F7d"

        A.runAzimuth contracts pk0 $
          A.createGalaxy nec "0x6DEfFb0caFDB11D175F123F6891AA64F01c24F7d"

        A.runAzimuth contracts pk0 $ do
          zp <- A.getPoint zod
          np <- A.getPoint nec
          pure (zp, np)

      A.pointDetails z1 `shouldSatisfy` A.detailsActive
      A.pointDetails n1 `shouldSatisfy` A.detailsActive

    it "rotates keys properly" $ do
      (kz, kn) <- A.runWeb3 endpoint $ do
        A.runAzimuth contracts pk0 $ A.configureKeys zod keys A.Rotate
        A.runAzimuth contracts pk0 $ A.configureKeys nec keys A.Rotate
        A.runAzimuth contracts pk0 $ do
          zp <- A.getPoint zod
          np <- A.getPoint nec
          pure (A.keyInformation zp, A.keyInformation np)

      A.keyCrypt kz `shouldBe` (A.CryptKey bytes)
      A.keyAuth kz `shouldBe` (A.AuthKey bytes)
      A.keyCrypt kn `shouldBe` (A.CryptKey bytes)
      A.keyAuth kn `shouldBe` (A.AuthKey bytes)

    it "breaches continuity properly" $ do
      (z0, n0) <- A.runWeb3 endpoint $
        A.runAzimuth contracts pk0 $ do
          zp <- A.getPoint zod
          np <- A.getPoint nec
          pure (zp, np)

      A.runWeb3 endpoint $ do
        A.runAzimuth contracts pk0 $ A.configureKeys zod keys A.Breach
        A.runAzimuth contracts pk0 $ A.configureKeys nec keys A.Breach

      (z1, n1) <- A.runWeb3 endpoint $
        A.runAzimuth contracts pk0 $ do
          zp <- A.getPoint zod
          np <- A.getPoint nec
          pure (zp, np)

      getRift z1 `shouldSatisfy` (> (getRift z0))
      getRift n1 `shouldSatisfy` (> (getRift n0))

    it "works with optional transaction parameters" $ do
      let txnParams = A.defaultTxnParams {
            A.txnGasPrice = Just 100_000_000_000
          }

      (z0, z1) <- A.runWeb3 endpoint $ do
        bef <- A.runAzimuth contracts () $ A.getPoint zod

        A.runAzimuth' contracts txnParams pk0 $
          A.configureKeys zod keys A.Breach

        aft <- A.runAzimuth contracts () $ A.getPoint zod

        pure (bef, aft)

      getRift z1 `shouldSatisfy` (> (getRift z0))

