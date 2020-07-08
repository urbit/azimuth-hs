{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Control.Exception (bracket)
import Crypto.Secp256k1
import qualified Data.Text as T
import qualified Shelly as S
import Test.Hspec
import qualified Urbit.Azimuth as A
import qualified Urbit.Ob as Ob
import Haskoin.Keys

initGanache :: IO T.Text
initGanache = S.shelly $ S.verbosely $
  S.run "docker" [
      "run"
    , "-d"
    , "-p"
    , "8545:8545"
    , "trufflesuite/ganache-cli:latest"
    , "--networkId"
    , "1"
    , "-m"
    , "'benefit crew supreme gesture quantum web media hazard " <>
      "theory mercy wing kitten'"
    ]

killGanache :: T.Text -> IO ()
killGanache pod = S.shelly $ S.verbosely $ do
  _ <- S.run "docker" [
      "stop"
    , pod
    ]

  pure ()

withGanache :: IO () -> IO ()
withGanache action = bracket initGanache killGanache (const action)

mnem :: T.Text
mnem = "benefit crew supreme gesture quantum web media hazard "
    <> "theory mercy wing kitten"

seed :: Either String Seed
seed = mnemonicToSeed mempty mnem

hd = case seed of
  Left _  -> error "bang"
  Right x -> makeXPrvKey x

path :: DerivPathI AnyDeriv
path = Deriv :| 44 :| 60 :| 0 :/ 0

pk0 :: A.LocalKey
pk0 = A.LocalKey key 0
  where
    key =
        A.importKey
      . getSecKey
      . xPrvKey
      . derivePath (path :/ 0)
      $ hd

simple :: A.LocalKey -> Ob.Patp -> IO A.Point
simple acct patp = do
  client <- A.defaultSettings "http://localhost:8545"
  A.runWeb3 client $ do
    block <- A.blockNumber
    A.withAccount acct $ do
      contracts <- A.getContracts
      A.runAzimuth contracts block (A.getPoint patp)

main :: IO ()
main = hspec $ around_ withGanache $
  it "does something" $ do
    point <- simple pk0 (Ob.patp 0)
    print point
