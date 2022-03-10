# Changelog for nervos-force-bridge

## Alpha Release

* Cardano congestion flow on BridgeIn Transactions
  * Monitoring of wallet to mempool
  * Floating transaction UI
  * Persistent transaction UI with auto updating status and monitoring

* Bridge In UI
  * Used address display
  * Short CKB address input
  * Fee calculation and display
  * Bridge button responds to valid and invalid states of the BridgIn form

* Nami wallet integration
  * Transaction signing
  * Transaction building
  * Detection and error handling

* CKB dev node runner and utilities
  * Creation, initial setup, and monitoring of the CKB node
  * RPC Communication with the node
  * Localized ckb-cli to keep dev node accounts and deployments out of global configuration
