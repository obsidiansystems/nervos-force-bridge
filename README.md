# Force Bridge

## Quick Start

### Install Obelisk

Obelisk is a framework for developing applications and a development and deployment tool.  Follow the instructions [here](https://github.com/obsidiansystems/obelisk/tree/e7ccc91806b94b424b086bf75087a0a5fd3ff0b5#installing-obelisk) to install Obelisk.

### Add additional binary caches

To speed up build times, we recommend enabling the following binary caches:

* The Obsidian Open Source binary cache:
  * Address: s3://obsidian-open-source
  * Public Key: `obsidian-open-source:KP1UbL7OIibSjFo9/2tiHCYLm/gJMfy8Tim7+7P4o0I=`
* The reflex-platform binary cache:
  * Address: https://nixcache.reflex-frp.org
  * Public Key: `ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=`
* The NixOS binary cache:
  * Address: https://cache.nixos.org
  * Public Key: `cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=`

These binary caches can be added to your Nix configuration by following the directions [here](https://github.com/obsidiansystems/obelisk/tree/e7ccc91806b94b424b086bf75087a0a5fd3ff0b5#installing-obelisk).

### Running the project

To run force-bridge in rapid development mode do

```bash
ob run
```

This will run the frontend (available at http://localhost:8000), the backend and any nodes specified to run (which can be done in Backend.hs).

You will see in the terminal where `ob run` is executed any errors, warning or other output from: Compilation, services and nodes. 

For nodes any required setup is done once and then re-used throughout development in a node related directory like ckb or cardano and is unique to each node, it may involve running a test/dev node, creating/funding accounts, doing initial transfers, healthchecking, and caching those results making them available to the application.

To recreate these nodes simply delete the node folder.

### Development

When running the project in rapid development mode: `ob run`

Changes to the static folder will cause a nix-build of the static assets in the background. 

Any changes to haskell code or files referenced from the haskell code will cause a hot reload.

#### Updating and working on dependencies

To work on dependencies (like libraries) you can locate the thunk in the deps folder and unpack via `nix-thunk` or `ob thunk`

You can find nix-thunk (here)[https://github.com/obsidiansystems/nix-thunk]

Simply unpack the thunk via `nix-thunk unpack <thunk>` or `ob thunk unpack <thunk>` and this will be pulled into the rapid development
workflow giving you realtime feedback on whatever dependency you are working on.

You can then commit, upstream and repack the thunk with `nix-thunk pack <thunk>` or `ob thunk pack <thunk>`

