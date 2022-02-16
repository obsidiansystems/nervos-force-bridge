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

  ckb = import sources.ckb {};
  ckb-cli = import sources.ckb-cli {};

in project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = self: super: {
    # canonical-json =
    # nothunks = self.callHackageDirect {
    # pkg = "nothunks";
    # ver = "0.1.3";
    # sha256 = "1zv28np3k3hg378vqm89v802xr0g8cwk7gy3mr77xrzy5jbgpa39";
    # /a};
    # cardano-prelude-test = self.callCabal2nix "cardano-prelude-test"  (sources.cardano-prelude + "/cardano-prelude-test") {};
    # cardano-prelude = self.callCabal2nix "cardano-prelude"  (sources.cardano-prelude + "/cardano-prelude") {};
    # cardano-binary = self.callCabal2nix "cardano-binary" (sources.cardano-base + "/binary") {};
  };

  tools = _: [ ckb ckb-cli pkgs.coreutils ];
})
