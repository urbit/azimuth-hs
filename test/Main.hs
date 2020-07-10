{-# LANGUAGE OverloadedStrings #-}
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

withGanache :: IO () -> IO ()
withGanache action = bracket ginit gkill (const action)

data Config = Config {
    endpt  :: String
  , mnem   :: A.Mnemonic
  , pbase  :: A.DerivPath
  , pk0    :: A.LocalKey
  , pk1    :: A.LocalKey
  , pk2    :: A.LocalKey
  }

testConfig :: Config
testConfig = Config {..} where
  endpt     = "http://localhost:8545"
  pbase     = A.Deriv A.:| 44 A.:| 60 A.:| 0 A.:/ 0
  Right pk0 = A.getLocalKey mnem mempty (pbase A.:/ 0)
  Right pk1 = A.getLocalKey mnem mempty (pbase A.:/ 1)
  Right pk2 = A.getLocalKey mnem mempty (pbase A.:/ 2)
  mnem      = "benefit crew supreme gesture quantum web media hazard " <>
              "theory mercy wing kitten"


rotate
  :: (A.JsonRpc m, MonadFail m)
  => Ob.Patp
  -> A.Azimuth m A.TxReceipt
rotate patp = do
  point <- A.getPoint patp
  let keys = A.keyInformation point
  A.configureKeys patp keys A.Rotate

main :: IO ()
main = do
  let Config {..} = infuraConfig
      nidsut      = Ob.patp 15663360

  client <- A.defaultSettings endpt

  hspec $
    it "rotates keys properly" $ do
      result <- A.runWeb3 client $ do
        block <- A.blockNumber
        A.withAccount pk0 $ do
          contracts <- A.getContracts
          A.runAzimuth contracts block $ do
            point <- A.getPoint nidsut
            rotate nidsut

      print result
