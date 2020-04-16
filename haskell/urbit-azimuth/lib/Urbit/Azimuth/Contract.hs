{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}

module Urbit.Azimuth where

import qualified Network.Ethereum.Contract.TH as Ethereum.Contract

[Ethereum.Contract.abiFrom|azimuth.json|]

