let
  this = import ./. {};
in {
  inherit (this) exe;
  inherit (this.shells) ghc;
  inherit (this.ghc) backend;
  inherit (this.ghc) frontend;
  inherit (this.ghc) common;
}
