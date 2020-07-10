{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Shelly as S
import Test.Hspec
import qualified Urbit.Azimuth as A
import qualified Urbit.Ob as Ob

ginit :: IO T.Text
ginit = S.shelly $ S.verbosely $
  S.run "docker" [
      "run"
    , "-d"
    , "-p", "8545:8545"
    , "trufflesuite/ganache-cli:latest"
    , "--networkId", "1"
    , "-m", "'benefit crew supreme gesture quantum web media hazard " <>
            "theory mercy wing kitten'"
    ]

gkill :: T.Text -> IO ()
gkill pod = S.shelly $ S.verbosely $
  S.run_ "docker" [
      "stop"
    , pod
    ]

withGanache :: IO () -> IO ()
withGanache action = bracket ginit gkill (const action)

mnem :: A.Mnemonic
mnem =
     "benefit crew supreme gesture quantum web media hazard theory mercy wing "
  <> "kitten"

pbase :: A.DerivPath
pbase = A.Deriv A.:| 44 A.:| 60 A.:| 0 A.:/ 0

path0 :: A.DerivPath
path0 = pbase A.:/ 0

path1 :: A.DerivPath
path1 = pbase A.:/ 1

path2 :: A.DerivPath
path2 = pbase A.:/ 2

pk0 :: A.LocalKey
pk0 = case A.getLocalKey mnem mempty path0 of
  Right x -> x
  _       -> error "bang"


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

  hspec $ around_ withGanache $
    it "does something" $ do
      point <- A.runWeb3 client $ do
        block <- A.blockNumber
        simple pk0 (Ob.patp 0)

      print point
