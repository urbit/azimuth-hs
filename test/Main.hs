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
    mnem  :: A.Mnemonic
  , pbase :: A.DerivPath
  , pk0   :: A.LocalKey
  , pk1   :: A.LocalKey
  , pk2   :: A.LocalKey
  }

testConfig :: Config
testConfig = Config {..} where
  pbase     = A.Deriv A.:| 44 A.:| 60 A.:| 0 A.:/ 0
  Right pk0 = A.getLocalKey mnem mempty (pbase A.:/ 0)
  Right pk1 = A.getLocalKey mnem mempty (pbase A.:/ 1)
  Right pk2 = A.getLocalKey mnem mempty (pbase A.:/ 2)
  mnem      = "benefit crew supreme gesture quantum web media hazard " <>
              "theory mercy wing kitten"

simple
  :: (A.JsonRpc m, MonadFail m)
  => A.LocalKey
  -> Ob.Patp
  -> m A.Point
simple acct patp = do
  block <- A.blockNumber
  A.withAccount acct $ do
    contracts <- A.getContracts
    A.runAzimuth contracts block (A.getPoint patp)

main :: IO ()
main = do
  client <- A.defaultSettings "http://localhost:8545"

  let Config {..} = testConfig

  hspec $ around_ withGanache $
    it "does something" $ do
      point <- A.runWeb3 client $ do
        block <- A.blockNumber
        simple pk0 (Ob.patp 0)

      print point
