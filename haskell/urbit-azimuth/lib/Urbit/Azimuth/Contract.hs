{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Urbit.Azimuth.Contract where

import qualified Network.Ethereum.Contract.TH as Ethereum.Contract

[Ethereum.Contract.abiFrom|azimuth.json|]
