{ haskellLib, lib, pkgs, libsodium-vrf
, ...
}:

let deps = pkgs.thunkSet ./dep;
in self: super: {

  # cardano-prelude
  cardano-prelude = self.callCabal2nix "cardano-prelude" (deps.cardano-prelude + "/cardano-prelude") {};
  cardano-prelude-test = self.callCabal2nix "cardano-prelude-test" (deps.cardano-prelude + "/cardano-prelude-test") {};

  # cardano-base
  cardano-binary = haskellLib.enableCabalFlag (self.callCabal2nix "cardano-binary" (deps.cardano-base + "/binary") {}) "development";
  cardano-binary-test = haskellLib.enableCabalFlag (self.callCabal2nix "cardano-binary-test" (deps.cardano-base + "/binary/test") {}) "development";
  cardano-slotting = self.callCabal2nix "cardano-slotting" (deps.cardano-base + "/slotting") {};
  cardano-crypto-praos = self.callCabal2nix "cardano-crypto-praos" (deps.cardano-base + "/cardano-crypto-praos") {};
  cardano-crypto-class = self.callCabal2nix "cardano-crypto-class" (deps.cardano-base + "/cardano-crypto-class") {};
  cardano-crypto-tests = self.callCabal2nix "cardano-crypto-tests" (deps.cardano-base + "/cardano-crypto-tests") {};
  strict-containers = self.callCabal2nix "strict-containers" (deps.cardano-base + "/strict-containers") {};
  base-deriving-via = self.callCabal2nix "base-deriving-via" (deps.cardano-base + "/base-deriving-via") {};
  orphans-deriving-via = self.callCabal2nix "orphans-deriving-via" (deps.cardano-base + "/orphans-deriving-via") {};
  measures = self.callCabal2nix "measures" (deps.cardano-base + "/measures") {};

  # cardano-ledger
  # tests fail on some env var not being set
  cardano-ledger-byron = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-byron" (deps.cardano-ledger + "/eras/byron/ledger/impl") {})) "development");
  cardano-ledger-byron-test = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-byron-test" (deps.cardano-ledger + "/eras/byron/ledger/impl/test") {})) "development");
  cardano-ledger-alonzo = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-alonzo" (deps.cardano-ledger + "/eras/alonzo/impl") {})) "development");
  cardano-ledger-core = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-core" (deps.cardano-ledger + "/libs/cardano-ledger-core") {});
  cardano-ledger-shelley = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-shelley" (deps.cardano-ledger + "/eras/shelley/impl") {});
  cardano-ledger-shelley-test = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-shelley-test" (deps.cardano-ledger + "/eras/shelley/test-suite") {});
  cardano-ledger-shelley-ma = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-shelley-ma" (deps.cardano-ledger + "/eras/shelley-ma/impl") {});
  cardano-protocol-tpraos = haskellLib.dontCheck (self.callCabal2nix "cardano-protocol-tpraos" (deps.cardano-ledger + "/libs/cardano-protocol-tpraos") {});
  non-integral = haskellLib.dontCheck (self.callCabal2nix "non-integral" (deps.cardano-ledger + "/libs/non-integral") {});
  small-steps = haskellLib.dontCheck (self.callCabal2nix "small-steps" (deps.cardano-ledger + "/libs/small-steps") {});
  small-steps-test = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "small-steps-test" (deps.cardano-ledger + "/libs/small-steps-test") {}));
  byron-spec-chain = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "byron-spec-chain" (deps.cardano-ledger + "/eras/byron/chain/executable-spec") {}));
  byron-spec-ledger = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "byron-spec-ledger" (deps.cardano-ledger + "/eras/byron/ledger/executable-spec") {}));
  cardano-crypto-wrapper = haskellLib.dontCheck (self.callCabal2nix "cardano-crypto-wrapper" (deps.cardano-ledger + "/eras/byron/crypto") {});
  cardano-crypto-test = haskellLib.doJailbreak (haskellLib.dontCheck (self.callCabal2nix "cardano-crypto-test" (deps.cardano-ledger + "/eras/byron/crypto/test") {}));
  # deprecated
  shelley-spec-non-integral = haskellLib.dontCheck (self.callCabal2nix "shelley-spec-non-integral" (deps.cardano-ledger + "/eras/shelley/chain-and-ledger/dependencies/non-integer") {});
  shelley-spec-ledger = haskellLib.dontCheck (self.callCabal2nix "shelley-spec-ledger" (deps.cardano-ledger + "/eras/shelley/chain-and-ledger/executable-spec") {});
  shelley-spec-ledger-test = haskellLib.doJailbreak (haskellLib.dontCheck (self.callCabal2nix "shelley-spec-ledger-test" (deps.cardano-ledger + "/eras/shelley/chain-and-ledger/shelley-spec-ledger-test") {}));

  # iohk-monitoring
  contra-tracer = haskellLib.dontCheck (self.callCabal2nix "contra-tracer" (deps.iohk-monitoring-framework + "/contra-tracer") {});
  iohk-monitoring = haskellLib.dontCheck (self.callCabal2nix "iohk-monitoring" (deps.iohk-monitoring-framework + "/iohk-monitoring") {});
  tracer-transformers = haskellLib.dontCheck (self.callCabal2nix "tracer-transformers" (deps.iohk-monitoring-framework + "/tracer-transformers") {});
  lobemo-backend-trace-forwarder = self.callCabal2nix "lobemo-backend-trace-forwarder" (deps.iohk-monitoring-framework + "/plugins/backend-trace-forwarder") {};
  lobemo-backend-monitoring = self.callCabal2nix "lobemo-backend-monitoring" (deps.iohk-monitoring-framework + "/plugins/backend-monitoring") {};
  lobemo-backend-aggregation = self.callCabal2nix "lobemo-backend-aggregation" (deps.iohk-monitoring-framework + "/plugins/backend-aggregation") {};
  lobemo-backend-ekg = self.callCabal2nix "lobemo-backend-ekg" (deps.iohk-monitoring-framework + "/plugins/backend-ekg") {};
  lobemo-scribe-systemd = self.callCabal2nix "lobemo-scribe-systemd" (deps.iohk-monitoring-framework + "/plugins/scribe-systemd") {};

  # ouroboros-network
  ouroboros-network = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "ouroboros-network" (deps.ouroboros-network + "/ouroboros-network") {}));
  ouroboros-network-framework = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-network-framework" (deps.ouroboros-network + "/ouroboros-network-framework") {});
  ouroboros-network-testing = self.callCabal2nix "ouroboros-network-testing" (deps.ouroboros-network + "/ouroboros-network-testing") {};
  ouroboros-consensus = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-consensus" (deps.ouroboros-network + "/ouroboros-consensus") {});
  ouroboros-consensus-byron = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-consensus-byron" (deps.ouroboros-network + "/ouroboros-consensus-byron") {});
  ouroboros-consensus-shelley = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-consensus-shelley" (deps.ouroboros-network + "/ouroboros-consensus-shelley") {});
  ouroboros-consensus-cardano = haskellLib.doJailbreak (self.callCabal2nix "ouroboros-consensus-cardano" (deps.ouroboros-network + "/ouroboros-consensus-cardano") {});
  io-sim = self.callCabal2nix "io-sim" (deps.ouroboros-network + "/io-sim") {};
  io-classes = self.callCabal2nix "io-classes" (deps.ouroboros-network + "/io-classes") {};
  monoidal-synchronisation = self.callCabal2nix "monoidal-synchronisation" (deps.ouroboros-network + "/monoidal-synchronisation") {};
  typed-protocols = self.callCabal2nix "typed-protocols" (deps.ouroboros-network + "/typed-protocols") {};
  typed-protocols-cborg = self.callCabal2nix "typed-protocols-cborg" (deps.ouroboros-network + "/typed-protocols-cborg") {};
  typed-protocols-examples = self.callCabal2nix "typed-protocols-examples" (deps.ouroboros-network + "/typed-protocols-examples") {};
  network-mux = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "network-mux" (deps.ouroboros-network + "/network-mux") {}));
  ntp-client = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "ntp-client" (deps.ouroboros-network + "/ntp-client") {}));

  # cardano-node
  cardano-api = haskellLib.dontCheck (self.callCabal2nix "cardano-api" (deps.cardano-node + "/cardano-api") {});
  cardano-node = ((self.callCabal2nix "cardano-node" (deps.cardano-node + "/cardano-node") {}));
  cardano-cli = haskellLib.overrideCabal (self.callCabal2nix "cardano-cli" (deps.cardano-node + "/cardano-cli") {}) (drv: {
    doCheck = false;
    preCheck = ''
      export CARDANO_CLI=$PWD/dist/build/cardano-cli
      export CARDANO_NODE_SRC=$PWD
    '';
    buildTools = (drv.buildTools or []) ++ [ pkgs.jq pkgs.shellcheck pkgs.coreutils ];
    configureFlags = [ "--dependency=cardano-api:gen=cardano-api-1.32.1-Fx8Wd6R8QrDmKMaXBLt3v-gen" ]; # gross, but it works
  });
  cardano-config = ((self.callCabal2nix "cardano-config" (deps.cardano-node + "/cardano-config") {}));
  hedgehog-extras = self.callCabal2nix "hedgehog-extras" deps.hedgehog-extras {};

  # cardano-wallet
  cardano-wallet = haskellLib.dontCheck (self.callCabal2nix "cardano-wallet" (deps.cardano-wallet + "/lib/shelley") {}); # pending setting up envars to fix tests
  cardano-wallet-cli = haskellLib.dontCheck (self.callCabal2nix "cardano-wallet-cli" (deps.cardano-wallet + "/lib/cli") {});
  cardano-wallet-core = haskellLib.overrideCabal (self.callCabal2nix "cardano-wallet-core" (deps.cardano-wallet + "/lib/core") {}) (drv: {
    doCheck = false;
    preBuild = ''export SWAGGER_YAML=${deps.cardano-wallet + "/specifications/api/swagger.yaml"}'';
  });
  cardano-wallet-core-integration = (self.callCabal2nix "cardano-wallet-core-integration" (deps.cardano-wallet + "/lib/core-integration") {});
  cardano-wallet-launcher = self.callCabal2nix "cardano-wallet-launcher" (deps.cardano-wallet + "/lib/launcher") {};
  cardano-wallet-test-utils = self.callCabal2nix "cardano-wallet-test-utils" (deps.cardano-wallet + "/lib/test-utils") {};
  cardano-numeric = self.callCabal2nix "cardano-numeric" (deps.cardano-wallet + "/lib/numeric") {};
  text-class = self.callCabal2nix "text-class" (deps.cardano-wallet + "/lib/text-class") {};
  strict-non-empty-containers = self.callCabal2nix "strict-non-empty-containers" (deps.cardano-wallet + "/lib/strict-non-empty-containers") {};

  # plutus (uh oh!)
  plutus-core = self.callCabal2nix "plutus-core" (deps.plutus + "/plutus-core") {};
  plutus-ledger = haskellLib.overrideCabal (self.callCabal2nix "plutus-ledger" (deps.plutus + "/plutus-ledger") {}) (drv: {
    doHaddock = false; # to avoid plutus-tx-plugin errors
    # github.com/haskell/cabal/issues/7270
    configureFlags = [ "--dependency=cardano-api:gen=cardano-api-1.32.1-Fx8Wd6R8QrDmKMaXBLt3v-gen" ]; # gross, but it works
  });
  freer-extras = self.callCabal2nix "freer-extras" (deps.plutus + "/freer-extras") {};
  playground-common = self.callCabal2nix "playground-common" (deps.plutus + "/playground-common") {};
  plutus-chain-index = self.callCabal2nix "plutus-chain-index" (deps.plutus + "/plutus-chain-index") {};
  plutus-contract = haskellLib.dontHaddock (self.callCabal2nix "plutus-contract" (deps.plutus + "/plutus-contract") {}); # FIXME fix tests
  plutus-pab = haskellLib.dontHaddock (self.callCabal2nix "plutus-pab" (deps.plutus + "/plutus-pab") {});
  plutus-tx = self.callCabal2nix "plutus-tx" (deps.plutus + "/plutus-tx") {};
  plutus-tx-plugin = self.callCabal2nix "plutus-tx-plugin" (deps.plutus + "/plutus-tx-plugin") {};
  plutus-ledger-api = self.callCabal2nix "plutus-ledger-api" (deps.plutus + "/plutus-ledger-api") {};
  # only nix-build of test seems broken, but running cabal test on pkg works :/
  plutus-use-cases = haskellLib.dontHaddock (haskellLib.dontCheck (self.callCabal2nix "plutus-use-cases" (deps.plutus + "/plutus-use-cases") {}));
  prettyprinter-configurable = self.callCabal2nix "prettyprinter-configurable" (deps.plutus + "/prettyprinter-configurable") {};
  quickcheck-dynamic = self.callCabal2nix "quickcheck-dynamic" (deps.plutus + "/quickcheck-dynamic") {};
  word-array = self.callCabal2nix "word-array" (deps.plutus + "/word-array") {};
  # plutus misc
  flat = self.callCabal2nix "flat" deps.flat {};
  size-based = haskellLib.doJailbreak super.size-based;
  row-types = self.callCabal2nix "row-types" deps.row-types {}; # "1.0.1.1"
  pcg-random = self.callHackage "pcg-random" "0.1.3.7" {};

  # cardano-addresses
  cardano-addresses = haskellLib.doJailbreak (self.callCabal2nixWithOptions "cardano-addresses" (deps.cardano-addresses + "/core") "--no-hpack" {});
  cardano-addresses-cli = haskellLib.dontCheck (self.callCabal2nixWithOptions "cardano-addresses-cli" (deps.cardano-addresses + "/command-line") "--no-hpack" { cardano-address = null; });

  # other iohk
  Win32-network = self.callCabal2nix "Win32-network" deps.Win32-network {};
  cardano-sl-x509 = self.callCabal2nix "cardano-sl-x509" deps.cardano-sl-x509 {};
  goblins = haskellLib.dontCheck (self.callCabal2nix "goblins" deps.goblins {});
  cardano-crypto = self.callCabal2nix "cardano-crypto" deps.cardano-crypto {};
  bech32 = haskellLib.dontCheck (self.callCabal2nix "bech32" (deps.bech32 + "/bech32") {}); # 1.1.1 ; tests rely on bech32 executable
  bech32-th = self.callCabal2nix "bech32-th" (deps.bech32 + "/bech32-th") {}; #1.1.1
  optparse-applicative-fork = haskellLib.dontCheck (self.callCabal2nix "optparse-applicative-fork" deps.optparse-applicative {});
  servant-purescript = self.callCabal2nix "servant-purescript" deps.servant-purescript {};
  purescript-bridge = self.callCabal2nix "purescript-bridge" deps.purescript-bridge {};

  # other misc
  aeson = self.callCabal2nix "aeson" deps.aeson {}; # 1.5.6.0
  openapi3 = haskellLib.doJailbreak (self.callHackage "openapi3" "3.1.0" {});
  servant-openapi3 = haskellLib.doJailbreak (self.callHackage "servant-openapi3" "2.0.1.2" {});
  servant = self.callHackage "servant" "0.18.3" {};
  servant-client = self.callHackage "servant-client" "0.18.3" {};
  servant-client-core = self.callHackage "servant-client-core" "0.18.3" {};
  servant-foreign = self.callHackage "servant-foreign" "0.15.4" {};
  servant-options = self.callHackage "servant-options" "0.1.0.0" {};
  servant-server = self.callHackage "servant-server" "0.18.3" {};
  servant-subscriber = self.callHackage "servant-subscriber" "0.7.0.0" {};
  servant-websockets = self.callHackage "servant-websockets" "2.0.0" {};
  http2 = haskellLib.dontCheck super.http2;
  http-media = haskellLib.doJailbreak super.http-media;
  tasty-bench = self.callHackage "tasty-bench" "0.2.5" {};
  async-timer = haskellLib.dontCheck (haskellLib.markUnbroken super.async-timer);
  OddWord = haskellLib.dontCheck (haskellLib.markUnbroken super.OddWord);
  quickcheck-state-machine = haskellLib.dontCheck (haskellLib.markUnbroken super.quickcheck-state-machine);
  # tests are not compatible with base16-bytestring 1.x
  cryptohash-md5 = haskellLib.dontCheck super.cryptohash-md5;
  # tests are not compatible with base16-bytestring 1.x
  cryptohash-sha1 = haskellLib.dontCheck super.cryptohash-sha1;
  # tests are not compatible with base16-bytestring 1.x
  monoidal-containers = haskellLib.dontCheck super.monoidal-containers;
  witherable = self.callHackage "witherable" "0.4" {};
  indexed-traversable = self.callHackage "indexed-traversable" "0.1.1" {};
  # QuickCheck constraints
  indexed-traversable-instances = haskellLib.dontCheck (self.callHackage "indexed-traversable-instances" "0.1" {});
  hs-rqlite = haskellLib.doJailbreak super.hs-rqlite;
  tls = self.callHackage "tls" "1.5.5" {};
  libsystemd-journal = haskellLib.overrideCabal (self.callHackage "libsystemd-journal" "1.4.5" {}) (drv: {
    librarySystemDepends = drv.librarySystemDepends or [] ++ [ pkgs.systemd ];
  });
  beam-sqlite = haskellLib.doJailbreak (self.callHackage "beam-sqlite" "0.5.0.0" {});
  scientific = self.callHackage "scientific" "0.3.7.0" {}; # min version compat with plutus-contract tests
  integer-logarithms = haskellLib.doJailbreak (self.callHackage "integer-logarithms" "1.0.3.1" {});
  smallcheck = self.callHackage "smallcheck" "1.2.1" {};
  memory = self.callCabal2nix "memory" deps.hs-memory {}; # 0.16
  katip = haskellLib.dontCheck super.katip; # tests also disabled in iohk-monitoring
  heist = haskellLib.dontCheck super.heist; # tests failing
  hspec-golden-aeson = haskellLib.dontCheck (self.callHackage "hspec-golden-aeson" "0.9.0.0" {}); # tests fail :/
  tasty-golden = self.callHackage "tasty-golden" "2.3.4" {};
  tasty = self.callHackage "tasty" "1.4.1" {};
  # tasty 1.4.1
  blaze-markup = haskellLib.doJailbreak super.blaze-markup;
  natural-transformation = haskellLib.doJailbreak super.natural-transformation;
  tdigest = haskellLib.doJailbreak super.tdigest;
  binary-orphans = haskellLib.doJailbreak super.binary-orphans;
  text-short = haskellLib.doJailbreak super.text-short;
  bytestring-type = haskellLib.doJailbreak super.bytestring-type;
  base64-bytestring-type = haskellLib.doJailbreak super.base64-bytestring-type;
  tree-diff = haskellLib.doJailbreak super.tree-diff;
  lattices = haskellLib.doJailbreak super.lattices;
  insert-ordered-containers = haskellLib.doJailbreak super.insert-ordered-containers;
  swagger2 = haskellLib.dontCheck (self.callHackage "swagger2" "2.6" {});
  lzma = haskellLib.dontCheck super.lzma;
  aeson-casing = haskellLib.dontCheck super.aeson-casing;
  servant-swagger-ui-core = self.callHackage "servant-swagger-ui-core" "0.3.5" {};
  servant-swagger-ui = self.callHackage "servant-swagger-ui" "0.3.5.3.47.1" {};
  recursion-schemes = self.callHackage "recursion-schemes" "5.2.2.2" {};

  persistent = self.callCabal2nix "persistent" (deps.persistent + "/persistent") {}; # 2.13.1.2
  persistent-test = self.callHackage "persistent-test" "2.13.0.0" {};
  persistent-sqlite = haskellLib.addPkgconfigDepend (haskellLib.enableCabalFlag (haskellLib.enableCabalFlag (self.callHackage "persistent-sqlite" "2.13.0.2" {}) "systemlib") "use-pkgconfig") pkgs.sqlite;
  persistent-postgresql = haskellLib.dontCheck (self.callHackage "persistent-postgresql" "2.13.0.3" {}); # tests use network
  persistent-template = self.callHackage "persistent-template" "2.12.0.0" {};
  persistent-qq = haskellLib.dontCheck super.persistent-qq;
  lift-type = self.callHackage "lift-type" "0.1.0.0" {};
  sqlite = null; #haskellLib.markUnbroken super.sqlite;

  nothunks = haskellLib.dontCheck (self.callHackage "nothunks" "0.1.3" {});
  moo = haskellLib.dontCheck (haskellLib.markUnbroken super.moo); # tests are failing :/
  gray-code = haskellLib.overrideCabal (haskellLib.markUnbroken super.gray-code) {
    preCompileBuildDriver = "rm Setup.hs";
  };
  typerep-map = haskellLib.doJailbreak (haskellLib.markUnbroken super.typerep-map);
  primitive = haskellLib.dontCheck (self.callHackage "primitive" "0.7.1.0" {}); # cardano-crypto-class min bound
  streaming-bytestring = self.callHackage "streaming-bytestring" "0.2.1" {}; # cardano-crypto-class min bound
  canonical-json = haskellLib.dontCheck (haskellLib.doJailbreak (haskellLib.markUnbroken super.canonical-json));
  cborg = haskellLib.dontCheck super.cborg; # tests don't build for base16-bytestring >=1
  text-conversions = self.callHackage "text-conversions" "0.3.1" {}; # compatible with base16-bytestring 1.x
  base16-bytestring = self.callHackage "base16-bytestring" "1.0.1.0" {}; # for cardano-prelude
  base64-bytestring = self.callCabal2nix "base64-bytestring" deps.base64-bytestring {}; # 1.2.1.0
  unordered-containers = self.callHackage "unordered-containers" "0.2.12.0" {}; # for cardano-addresses
  protolude = self.callHackage "protolude" "0.3.0" {}; # for cardano-prelude
  formatting = self.callHackage "formatting" "7.1.0" {};
  fmt = self.callCabal2nix "fmt" deps.fmt {};
  fgl = haskellLib.doJailbreak super.fgl;
  fgl-arbitrary = haskellLib.doJailbreak super.fgl-arbitrary;
  string-interpolate = self.callHackage "string-interpolate" "0.3.1.1" {};
  wide-word = haskellLib.dontCheck (self.callHackage "wide-word" "0.1.1.2" {});
  graphviz = haskellLib.dontCheck super.graphviz;
  # misc expensive. 21.05 should have more recent versions.
  hspec = self.callHackage "hspec" "2.8.2" {};
  hspec-core = self.callHackage "hspec-core" "2.8.2" {};
  hspec-discover = self.callHackage "hspec-discover" "2.8.2" {};
  hspec-expectations = self.callHackage "hspec-expectations" "0.8.2" {};
  hspec-meta = self.callHackage "hspec-meta" "2.7.8" {};
  QuickCheck = self.callHackage "QuickCheck" "2.14.2" {};
  quickcheck-instances = self.callHackage "quickcheck-instances" "0.3.25.2" {};
  hedgehog = self.callHackage "hedgehog" "1.0.5" {};
  hedgehog-quickcheck = self.callHackage "hedgehog-quickcheck" "0.1.1" {};
  # allow newer QuickCheck/hspec
  time-compat = self.callHackage "time-compat" "1.9.6" {};
  strict = self.callHackage "strict" "0.4.0.1" {};
  vector = haskellLib.doJailbreak super.vector;
  attoparsec = haskellLib.doJailbreak super.attoparsec;
  random = haskellLib.dontCheck (self.callHackage "random" "1.2.0" {});
  generic-random = haskellLib.dontCheck (self.callHackage "generic-random" "1.4.0.0" {});
  splitmix = haskellLib.dontCheck (self.callHackage "splitmix" "0.1.0.3" {}); # dontCheck to avoid cycle with random
  http-api-data = haskellLib.doJailbreak super.http-api-data;
  algebraic-graphs = haskellLib.doJailbreak (haskellLib.dontCheck super.algebraic-graphs);
  cassava = haskellLib.doJailbreak super.cassava;
  psqueues = haskellLib.doJailbreak super.psqueues;
  tasty-hedgehog = haskellLib.doJailbreak super.tasty-hedgehog;
  tasty-hspec = self.callHackage "tasty-hspec" "1.2" {};
  tasty-discover = haskellLib.dontCheck super.tasty-discover;
  test-framework = haskellLib.doJailbreak super.test-framework;
  test-framework-quickcheck2 = haskellLib.doJailbreak super.test-framework-quickcheck2;
  hashable = self.callHackage "hashable" "1.3.2.0" {};
  base-orphans = self.callHackage "base-orphans" "0.8.4" {};
  dom-lt = haskellLib.markUnbroken super.dom-lt;
  # # Bump to fix build with ghcjs
  network = self.callHackage "network" "3.1.2.1" {};
  network-bsd = self.callHackage "network-bsd" "2.8.1.0" {};
  jsaddle = haskellLib.doJailbreak super.jsaddle; # allow newer base64-bytestring
}
