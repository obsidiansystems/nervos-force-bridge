{ pkgs }:
let
  nodePkgs = (pkgs.callPackage ./src {
    inherit pkgs;
    nodejs = pkgs.nodejs-12_x;
  }).shell.nodeDependencies;

  frontendSrcFiles = ../frontend;
in pkgs.stdenv.mkDerivation {
  name = "static";
  src = ./src;
  buildInputs = [pkgs.nodejs];
  installPhase = ''
    mkdir -p $out/css
    mkdir -p $out/js
    # mkdir -p $out/images

    # Setting up the node environment:
    ln -s ${nodePkgs}/lib/node_modules ./node_modules
    export PATH="${nodePkgs}/bin:$PATH"

    # We make the frontend haskell source files available here:
    # This corresponds to the path specified in tailwind.config.js
    ln -s ${frontendSrcFiles} frontend

    # Run the postcss compiler:
    # postcss css/styles.css -o $out/styles.css
    tailwindcss -i ./input.css -o $out/css/output.css

    webpack

    cp -r dist/* $out/js/

    # We can write other commands to produce more static files as well:
    # cp -r images/* $out/images/
  '';
}
