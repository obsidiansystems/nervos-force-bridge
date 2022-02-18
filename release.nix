{ self ? import ./.
, supportedSystems ? ["x86_64-linux"]
}:
let
  native-reflex-platform = (self {}).reflex;
  inherit (native-reflex-platform.nixpkgs) lib;

  perPlatform = lib.genAttrs supportedSystems (system: lib.recurseIntoAttrs {
    inherit (self { inherit system; }) exe;
    inherit (self { inherit system; }) ghcjs;
    # browser-extension = lib.recurseIntoAttrs (self { inherit system; }).browser-extension;
    inherit ((self { inherit system; }).shells) ghc;
  });
in lib.recurseIntoAttrs perPlatform
