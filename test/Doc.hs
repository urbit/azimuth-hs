{-# LANGUAGE OverloadedStrings #-}

import qualified Urbit.Azimuth as A
import qualified Urbit.Ob as Ob

-- A simple test endpoint.  You'll probably want to set up your own.
infura :: String
infura = "https://mainnet.infura.io/v3/b7d2af9f01534031ba773374f766ef65"

-- A simple example of setting up an endpoint, fetching the Azimuth contracts,
-- getting a private key from a BIP39 mnemonic and HD path, and fetching ~zod's
-- public information.

main :: IO ()
main = do
  endpoint  <- A.defaultSettings infura
  contracts <- A.runWeb3 endpoint A.getContracts

  let zod = Ob.patp 0
      nec = Ob.patp 1

  -- fetch ~zod's public info, using endpoint's default account
  zodInfo <- A.runWeb3 endpoint $
    A.runAzimuth contracts () $
      A.getPoint zod

  -- to use an account..

  -- use a test mnemonic
  let mnem = "benefit crew supreme gesture quantum "
          <> "web media hazard theory mercy wing kitten"

  -- and a standard HD path
  let hdpath  = "m/44'/60'/0'/0/0" :: A.DerivPath

  let account = case A.toPrivateKey mnem mempty hdpath of
        Left _    -> error "bogus creds"
        Right acc -> acc

  -- fetch ~nec's public info, using a local account
  necInfo <- A.runWeb3 endpoint $
    A.runAzimuth contracts account $
      A.getPoint zod

  -- print the details
  print zodInfo
  print necInfo
