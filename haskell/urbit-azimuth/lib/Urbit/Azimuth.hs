{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Urbit.Azimuth where

import qualified Network.Ethereum.Contract.TH as Ethereum.Contract

[Ethereum.Contract.abiFrom|azimuth.json|]
