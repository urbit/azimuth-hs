# azimuth-hs

[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://opensource.org/licenses/MPL-2.0)

Interact with [Azimuth](https://github.com/urbit/azimuth-solidity) from
Haskell.

## Basic Usage

You can get started by pulling in `Urbit.Azimuth` (probably qualified):

```
import qualified Urbit.Azimuth as A
```

Friendly wrappers for the Azimuth and Ecliptic contracts can be found in
`Urbit.Azimuth.Azimuth` and `Urbit.Azimuth.Ecliptic`.  They'll both be pulled
in via `import Urbit.Azimuth`.

If you want to work with the raw hs-web3 functions generated from the contract
ABIs, you can import `Urbit.Azimuth.Azimuth.Internal` or
`Urbit.Azimuth.Ecliptic.Internal` directly.

To use the various functions provided, you'll generally want to:

* define a web3 endpoint (probably via
  [hs-web3](https://github.com/airalab/hs-web3)'s `defaultSettings` function,
  re-exported here),

* provide a `Contracts` object (this can be procured via `getContracts`),

* provide a private key, and then,

* use `runAzimuth` to call the desired contract function with the necessary
  information.

You can check out the [quickstart section](#quickstart) below to see an example
of the whole dance.

## Quickstart

This example uses an [Infura](https://infura.io/) endpoint as a provider for
web3:

```
import qualified Urbit.Azimuth as A
import qualified Urbit.Ob as Ob

infura :: String
infura = "https://mainnet.infura.io/v3/MY_INFURA_PROJECT_ID"

main :: IO ()
main = do
  endpoint  <- A.defaultSettings infura
  contracts <- A.runWeb3 endpoint A.getContracts

  -- use a test mnemonic
  let mnem = "benefit crew supreme gesture quantum web media hazard theory mercy wing kitten"

  -- and a standard HD path, m/44'/60'/0'/0/0
  let hdpath  = A.Deriv A.:| 44 A.:| 60 A.:| 0 A.:/ 0 A.:/ 0

  -- unlock the corresponding account
  let account = case A.toPrivateKey mnem mempty hdpath of
        Left _  -> error "bogus creds"
        Right a -> a

  let zod = Ob.patp 0

  -- fetch ~zod's public info
  point <- A.runWeb3 endpoint $
    A.runAzimuth contracts account $
      A.getPoint zod

  print point
```

