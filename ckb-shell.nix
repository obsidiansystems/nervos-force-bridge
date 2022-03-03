{ pkgs ? import <nixpkgs> {}
# , capsule ? import ./nix/capsule {}
# , ckb-cli ? import ./nix/ckb-cli {}
}:

let
  # TODO(skylar): What do we strip from here...
  # To use this shell.nix on NixOS your user needs to be part of the podman group
  # and have the podman daemon enabled via configuration.nix virtualisation.podman.enable = true;

  nix-thunk = import ./deps/nix-thunk {};
  sources = nix-thunk.mapSubdirectories nix-thunk.thunkSource ./deps;

  # foldExtensions = lib.foldr lib.composeExtensions (_: _: {});

  ckb = import sources.ckb {};
  ckb-cli = import sources.ckb-cli {};
  capsule = import sources.capsule {};

  # Provides a script that copies required files to ~/
  podmanSetupScript = let
    registriesConf = pkgs.writeText "registries.conf" ''
      [registries.search]
      registries = ['docker.io']
      [registries.block]
      registries = []
    '';
  in pkgs.writeScript "podman-setup" ''
    #!${pkgs.runtimeShell}
    # Dont overwrite customised configuration
    if ! test -f ~/.config/containers/policy.json; then
      install -Dm555 ${pkgs.skopeo.src}/default-policy.json ~/.config/containers/policy.json
    fi
    if ! test -f ~/.config/containers/registries.conf; then
      install -Dm555 ${registriesConf} ~/.config/containers/registries.conf
    fi
  '';

  # Provides a fake "docker" binary mapping to podman
  dockerCompat = pkgs.runCommandNoCC "docker-podman-compat" {} ''
    mkdir -p $out/bin
    ln -s ${pkgs.podman}/bin/podman $out/bin/docker
    ln -s ${pkgs.podman-compose}/bin/podman-compose $out/bin/docker-compose
  '';

in pkgs.mkShell {

  buildInputs = [
    dockerCompat
    ckb
    capsule
    ckb-cli
    pkgs.rustc
    pkgs.cargo
    pkgs.bash
    pkgs.podman  # Docker compat
    pkgs.podman-compose # Docker compose compat
    pkgs.runc  # Container runtime
    pkgs.conmon  # Container runtime monitor
    pkgs.skopeo  # Interact with container registry
    pkgs.slirp4netns  # User-mode networking for unprivileged namespaces
    pkgs.fuse-overlayfs  # CoW for images, much faster than default vfs
    pkgs.nodejs
    pkgs.yarn
  ];

  shellHook = ''
    # Install required configuration
    ${podmanSetupScript}
    # ln -s /bin/bash ${pkgs.bash}/bin/bash # What else can we do instead?
  '';

  DOCKER_HOST="unix:///run/podman/podman.sock";
}
