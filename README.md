# azimuth-hs

[![Hackage Version](https://img.shields.io/hackage/v/azimuth-hs.svg)](http://hackage.haskell.org/package/azimuth-hs)
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

* provide a `Contracts` object (this can be procured via `getContracts` for
  mainnet),

* provide an account, and then,

* use `runAzimuth` to call the desired contract function with the necessary
  information.

By 'provide an account', I mean you'll typically need to pass a private key.
But if you're just reading from the chain, you can typically use the endpoint's
default account, specified via unit (i.e. `()`).

Check out the [quickstart section](#quickstart) below to see an examples of all
the above.

## Quickstart

This example uses an [Infura](https://infura.io/) endpoint as a provider for
web3:

```
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Urbit.Azimuth as A
import qualified Urbit.Ob as Ob

-- A test endpoint definition.  You'll want to set up your own to do anything
-- interesting.
infura :: String
infura = "MY_INFURA_ENDPOINT"

-- A simple example of setting up an endpoint, fetching the Azimuth contracts,
-- getting a private key from a BIP39 mnemonic and HD path, and fetching the
-- public information for a few ships.

main :: IO ()
main = do
  endpoint  <- A.defaultSettings infura

  -- fetch the mainnet contract addresses
  contracts <- A.runWeb3 endpoint A.getContracts

  let zod = Ob.patp 0
      nec = Ob.patp 1
      bud = Ob.patp 2

  -- fetch ~zod's public info, using endpoint's default account ()
  zodInfo <- A.runWeb3 endpoint $
    A.runAzimuth contracts () $
      A.getPoint zod

  print zodInfo

  -- to use a nontrivial account:

  -- take a test BIP39 mnemonic
  let mnem = "benefit crew supreme gesture quantum "
          <> "web media hazard theory mercy wing kitten"

  -- a standard HD path
  let hdpath  = "m/44'/60'/0'/0/0" :: A.DerivPath

  -- and the ethereum mainnet chain ID
  let chainId = 1

  -- and then use them to derive a suitable private key
  let account = case A.toPrivateKey mnem mempty hdpath chainId of
        Left _    -> error "bogus creds"
        Right acc -> acc

  -- fetch ~nec's public info, using this private key to auth
  necInfo <- A.runWeb3 endpoint $
    A.runAzimuth contracts account $
      A.getPoint zod

  print necInfo

  -- you can also set the gas price, etc. as follows
  let params = A.defaultTxnParams { A.txnGasPrice = Just 100_000_000_000 }

  -- (see Urbit.Azimuth.Transaction for details on those)

  -- use runAzimuth' (notice the apostrophe) to supply those parameters
  budInfo <- A.runWeb3 endpoint $
    A.runAzimuth' contracts params account $
      A.getPoint bud

  print budInfo
```

## Building

Note that depending on your system and GHC installation, a few transitive
dependencies may prove tricky to build.  In particular, if you've installed GHC
with [Nix](https://nixos.org/nix), you may need to build from within a Nix
shell with [zlib-dev](https://www.zlib.net/),
[libsecp256k1](https://github.com/bitcoin-core/secp256k1), and
[pkg-config](https://en.wikipedia.org/wiki/Pkg-config) present.

A suitable [Nix](https://nixos.org/) shell is provided in `shell.nix`, so you
can get a repl via:

```
~/src/azimuth-hs$ nix-shell
[nix-shell:~/src/azimuth-hs]$ cabal new-repl
```

## See Also

* [azimuth-js](https://github.com/urbit/azimuth-js) -- JavaScript bindings

