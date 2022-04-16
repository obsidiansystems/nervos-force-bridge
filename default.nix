{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    terms.security.acme.acceptTerms = true;
  }
}:
with obelisk;
let
  nix-thunk = import ./deps/nix-thunk {};
  sources = nix-thunk.mapSubdirectories nix-thunk.thunkSource ./deps;

  foldExtensions = lib.foldr lib.composeExtensions (_: _: {});

  ckb = import sources.ckb {};
  ckb-cli = import sources.ckb-cli {};
  ckb-indexer = import sources.ckb-indexer {};
  capsule = import sources.capsule {};
  pkgs = obelisk.pkgs;

in project ./. ({ pkgs, ... }: let
  haskellLib = pkgs.haskell.lib;
in with pkgs.haskell.lib; {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  staticFiles = import ./static { inherit pkgs; };

  packages = {
    web3 = sources.hs-web3 + "/packages/web3";
    web3-bignum = sources.hs-web3 + "/packages/bignum";
    web3-crypto = sources.hs-web3 + "/packages/crypto";
    # TODO: Make Types from this package usable from ghcjs; currently unbuildable in ghcjs as it pulls in web3-jsonrpc
    web3-ethereum = sources.hs-web3 + "/packages/ethereum";
    # fork buildable in ghcjs of Types extracted from web3-ethereum
    web3-ethereum-core = sources.hs-web3 + "/packages/ethereum-core";
    web3-hexstring = sources.hs-web3 + "/packages/hexstring";
    web3-jsonrpc = sources.hs-web3 + "/packages/jsonrpc";
    web3-provider = sources.hs-web3 + "/packages/provider";
    web3-scale = sources.hs-web3 + "/packages/scale";
    web3-solidity = sources.hs-web3 + "/packages/solidity";
  };

  overrides =
    foldExtensions [
      # haskell overlay for cardano pkgs
      (import ./cardano-overlays { inherit haskellLib pkgs lib; }).combined
      (self: super: {
        snap-core = haskellLib.dontCheck (self.callCabal2nix "snap-core" sources.snap-core {}); # unreleased 1.0.4.3
        map-syntax = haskellLib.doJailbreak super.map-syntax;
        xmlhtml = haskellLib.doJailbreak super.xmlhtml;
        # TODO: upstream
        hspec-webdriver = self.callCabal2nix "hspec-webdriver" sources.hspec-webdriver-clone {};
        websockets = haskellLib.doJailbreak (self.callHackage "websockets" "0.12.7.2" {});
        patch = haskellLib.doJailbreak super.patch;
        reflex-dom-core = haskellLib.doJailbreak super.reflex-dom-core;
        reflex = haskellLib.doJailbreak (haskellLib.dontCheck (self.callCabal2nix "reflex" sources.reflex {}));
        browser-extension = (self.callCabal2nix "browser-extension" sources.browser-extension {});
        aeson-gadt-th = self.callCabal2nix "aeson-gadt-th" sources.aeson-gadt-th {};
        deriving-compat = self.callHackage "deriving-compat" "0.6" {};
        constraints-extras = self.callCabal2nix "constraints-extras" sources.constraints-extras {};
        vessel = haskellLib.doJailbreak (self.callCabal2nix "vessel" sources.vessel {});
        dependent-monoidal-map = haskellLib.doJailbreak super.dependent-monoidal-map;
        entropy = self.callCabal2nix "entropy" sources.entropy {};
        ghcjs-dom = self.callHackage "ghcjs-dom" "0.9.5.0" {};
        jsaddle-dom = self.callHackage "jsaddle-dom" "0.9.5.0" {};
        ghcjs-dom-jsffi = self.callHackage "ghcjs-dom-jsffi" "0.9.5.0" {};
        ghcjs-dom-jsaddle = self.callHackage "ghcjs-dom-jsaddle" "0.9.5.0" {};
        validation-selective = haskellLib.doJailbreak super.validation-selective;
        tomland = haskellLib.doJailbreak super.tomland;

        hexstring = self.callCabal2nix "hexstring" sources.haskell-hexstring {};
        base-orphans = self.callHackage "base-orphans" "0.8.6" {};
        hashable = self.callHackage "hashable" "1.3.5.0" {};
        time-compat = dontCheck super.time-compat;

        OneTuple = doJailbreak super.OneTuple;
        generics-sop = doJailbreak super.generics-sop;
        wss-client = dontCheck (markUnbroken super.wss-client);
        web3-scale = dontCheck (doJailbreak super.web3-scale);
        web3-jsonrpc = doJailbreak super.web3-jsonrpc;
        web3-hexstring = doJailbreak super.web3-hexstring;
        web3-bignum = dontCheck (doJailbreak super.web3-bignum);
        web3-crypto = dontCheck (doJailbreak super.web3-crypto);
        web3-solidity = doJailbreak (dontCheck super.web3-solidity);
        web3-ethereum-core = doJailbreak (dontCheck super.web3-ethereum-core);
        web3-ethereum = doJailbreak (dontCheck super.web3-ethereum);
        basement = doJailbreak (self.callHackage "basement" "0.0.11" {});

        # web3-ethereum dependency
        relapse = dontCheck (markUnbroken super.relapse);

        # This jailbreak has a suspicious type warning
        singletons = self.callHackage "singletons" "3.0.1" {};
        vinyl = dontCheck (doJailbreak (markUnbroken super.vinyl));
        web3-polkadot = doJailbreak (dontHaddock (dontCheck (self.callCabal2nix "web3-polkadot" "${sources.hs-web3}/packages/polkadot" { hspec-expectations = null; hspec-expectations-json = null; })));

        # Add executables to backend depends
        backend = haskellLib.overrideCabal super.backend {
          librarySystemDepends = [
            ckb ckb-cli ckb-indexer capsule
          ];
        };
      })];
})
