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

To run force-bridge, the ui, and nodes:

```bash
ob run
```
