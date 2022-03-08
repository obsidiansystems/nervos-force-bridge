{ haskellLib, pkgs, lib }:

let
  deps = pkgs.thunkSet ./dep;
  libsodium-vrf = pkgs.callPackage (deps.iohk-nix + "/overlays/crypto/libsodium.nix") {};
in rec {

  optionalExtension = cond: overlay: if cond then overlay else _: _: {};
  foldExtensions = lib.foldr lib.composeExtensions (_: _: {});

  combined = self: super: foldExtensions [
    cardanoPackages
    (optionalExtension (!(super.ghc.isGhcjs or false)) ghc)
    (optionalExtension (super.ghc.isGhcjs or false) ghcjs)
  ] self super;

  cardanoPackages = import ./cardano-packages {
    inherit haskellLib pkgs lib libsodium-vrf;
  };

  ghc = self: super: {
    cardano-crypto-class = haskellLib.addPkgconfigDepend super.cardano-crypto-class libsodium-vrf;
    cardano-crypto-praos = haskellLib.addPkgconfigDepend super.cardano-crypto-praos libsodium-vrf;
  };

  ghcjs = import ./ghcjs.nix {
    inherit haskellLib pkgs lib libsodium-vrf;
  };

}
