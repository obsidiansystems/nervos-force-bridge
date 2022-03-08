{ haskellLib, pkgs, lib, libsodium-vrf }:

let deps = pkgs.thunkSet ./cardano-packages/dep;
in self: super: {
  # this ensures same libsodium as ghc is used
  cardano-crypto-class = haskellLib.overrideCabal (super.cardano-crypto-class.override { libsodium = libsodium-vrf; }) (drv: {
    buildTools = [pkgs.pkg-config];
  });
  # this ensures same libsodium as ghc is used
  cardano-crypto-praos = haskellLib.overrideCabal (super.cardano-crypto-praos.override { libsodium = libsodium-vrf; }) (drv: {
    buildTools = [pkgs.pkg-config];
  });

  # TODO: upstream to reflex-platform ghc810 branch
  network = haskellLib.overrideCabal super.network (drv: {
    doCheck = false;
    # avoid custom Setup.hs which has trouble configuring the pkg
    preCompileBuildDriver = "rm Setup.hs";
    preConfigure = "./configure";
    # TODO: correctly include generated HsNetworkConfig.h in pkg lib dir
    preFixup = ''
      cp include/HsNetworkConfig.* $prefix/lib/ghcjs-8.10.7/js-ghcjs-ghcjs-8.10.7-ghc8_10_7/network-3.1.2.1-Bq1IGsPSGY2B7ccccStsE/include/
    '';
  });

  # ghcjs build fixes
  terminal-size = self.callCabal2nix "terminal-size" (pkgs.hackGet ./dep/terminal-size) {};
  unix-bytestring = self.callCabal2nix "unix-bytestring" (pkgs.hackGet ./dep/plutus-ghcjs-cross + "/contrib/unix-bytestring-0.3.7.3") {};

  # for fixing 32-bit build
  plutus-core = haskellLib.overrideCabal super.plutus-core (drv: {
    doCheck = false;
    doHaddock = false;
    doBenchmark = false;
    preConfigure = ''
      substituteInPlace plutus-core/src/PlutusCore/Evaluation/Machine/ExMemory.hs \
        --replace "WORD_SIZE_IN_BITS < 64" "0"
    '';
  });

  # cryptonite with entropy support for ghcjs.
  cryptonite = self.callCabal2nix "cryptonite" (pkgs.hackGet ./dep/cryptonite-ghcjs) {};

  # this is just to force entropy to be generated properly for ghcjs.
  entropy = self.callHackage "entropy" "0.4.1.6" {};

  # From cardano-addresses cabal.project
  foundation = self.callCabal2nix "foundation"
    (pkgs.fetchFromGitHub {
      owner = "hamishmack";
      repo = "foundation";
      rev = "421e8056fabf30ef2f5b01bb61c6880d0dfaa1c8";
      sha256 = "0cbsj3dyycykh0lcnsglrzzh898n2iydyw8f2nwyfvfnyx6ac2im";
    } + "/foundation") {};

  digest = haskellLib.overrideCabal super.digest (drv: {
    librarySystemDepends = [ pkgs.zlib ];
  });

  # cardano-addresses
  cardano-addresses = haskellLib.dontCheck super.cardano-addresses;
  cardano-addresses-jsbits = let
    jsbits = pkgs.runCommand "cardano-addresses-jsbits" {} ''
      script=$(mktemp -d)
      cp -r ${deps.cardano-addresses + "/jsbits/emscripten"}/* $script
      ln -s ${pkgs.srcOnly {name = "cryptonite-src"; src = self.cryptonite.src;}}/cbits $script/cryptonite
      ln -s ${pkgs.srcOnly {name = "cardano-crypto-src"; src = deps.cardano-crypto;}}/cbits $script/cardano-crypto
      patchShebangs $script/build.sh
      (cd $script && PATH=${
          # The extra buildPackages here is for closurecompiler.
          # Without it we get `unknown emulation for platform: js-unknown-ghcjs` errors.
          lib.makeBinPath (with pkgs.buildPackages.buildPackages;
            [emscripten closurecompiler coreutils])
        }:$PATH ./build.sh)
      mkdir -p $out
      cp $script/cardano-crypto.js $out
    '';
    addJsbits = ''
      mkdir -p jsbits
      cp ${jsbits}/* jsbits
    '';
  in haskellLib.overrideCabal (self.callCabal2nixWithOptions "cardano-addresses-jsbits" (deps.cardano-addresses + "/jsbits") "--no-hpack" {}) (drv: {
    preConfigure = ''
      ${addJsbits}
      substituteInPlace cardano-addresses-jsbits.cabal --replace "ghc-options: jsbits/cardano-crypto.js" ""
    '';
  });
}
