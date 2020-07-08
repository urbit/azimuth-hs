{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket)
import qualified Data.Text as T
import qualified Shelly as S
import Test.Hspec

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
killGanache pod = S.shelly $ S.verbosely $
  _ <- S.run "docker" [
      "stop"
    , pod
    ]

  pure ()

withGanache :: IO () -> IO ()
withGanache action = bracket initGanache killGanache (const action)

main :: IO ()
main = hspec $ around_ withGanache $ do
  undefined
