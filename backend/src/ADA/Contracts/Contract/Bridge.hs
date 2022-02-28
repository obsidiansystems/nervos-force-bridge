{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module ADA.Contracts.Bridge where

import GHC.Generics
import           Control.Monad         (void, when)
import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Ledger                ( Address
                                       , Datum (Datum)
                                       , ScriptContext
                                       , Redeemer(..)
                                       , Validator
                                       , Value
                                       , PubKeyHash
                                       , TxOutRef
                                       , scriptContextTxInfo
                                       , txSignedBy
                                       , txId
                                       )
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Address
import           Ledger.Tx             (ChainIndexTxOut (..))
import qualified Ledger.Typed.Scripts  as Scripts
import           Plutus.Contract
import           Plutus.Contract.Trace as X
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import           Prelude               ((<$>))
import qualified Prelude               as Haskell
import           Plutus.Trace.Emulator (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx.Builtins.Class as Builtins
import qualified Prelude as Haskell (Semigroup (..), Show, foldMap)
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON)

-- On-chain components

data Bridge
instance Scripts.ValidatorTypes Bridge where
  type instance RedeemerType Bridge = BridgeRedeemer
  type instance DatumType Bridge = BridgeDatum

bridgeAddress :: Address
bridgeAddress = scriptAddress bridgeValidator

bridgeValidator :: Validator
bridgeValidator = Scripts.validatorScript bridgeInstance

bridgeInstance :: Scripts.TypedValidator Bridge
bridgeInstance = Scripts.mkTypedValidator @Bridge
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @BridgeDatum @BridgeRedeemer

data BridgeSettings =
  BridgeSettings { bridgeSettingsVerifiers :: !([PaymentPubKeyHash])
                 , bridgeSettingsFeeAddress :: !PaymentPubKeyHash
                 , bridgeSettingsThreshold :: !Haskell.Integer
                 , bridgeSettingsMinimum :: !Haskell.Integer
                 , bridgeSettingsFees :: !Haskell.Integer
                 }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

data BridgeDatum
  = LockRecord CKBAddress
  | CurrentSettings BridgeSettings
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

data BridgeRedeemer
  = Lock !Integer
  | Unlock
  | Update !BridgeSettings
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

validator :: Validator
validator = Scripts.validatorScript bridgeInstance

mkValidator :: BridgeDatum -> BridgeRedeemer -> ScriptContext -> Bool
mkValidator bd r ctx =
  case bd of
    CurrentSettings bs ->
      let
        presentVerifiers = length (filter (txSignedBy (scriptContextTxInfo ctx) . unPaymentPubKeyHash) vs)
        enoughSignatures = presentVerifiers >= threshold

        fees = bridgeSettingsFees bs
        minValue = bridgeSettingsMinimum bs

        vs = bridgeSettingsVerifiers bs
        threshold = bridgeSettingsThreshold bs
      in case r of
        Update _ ->
          traceIfFalse "Not enough signatures" enoughSignatures &&
          traceIfFalse "Invalid bridge settings" (validSettings bs)

        Lock v ->
          traceIfFalse "Not over the minimum bridge threshold" (v <= minValue) &&
          traceIfFalse "You cannot pay fees (Bridge settings should be adjusted, contact bridge operators)" (v > fees)

        Unlock ->
          traceIfFalse "Not enough signatures" enoughSignatures

    _ -> traceIfFalse "Current bridge settings are required to validate" False


-- TODO(skylar): Break this into multiple traceIfFalses for better settings
validSettings :: BridgeSettings -> Bool
validSettings bs =
  traceIfFalse "Need at least one verifier" (numVerifiers > 0) &&
  traceIfFalse "Threshold can't be more than the amount of verifiers" (threshold <= numVerifiers) &&
  traceIfFalse "Minimum shouldn't be negative" (minAmount > 0) &&
  traceIfFalse "Fees shouldn't be negative" (fees > 0)
  where
    numVerifiers = length $ bridgeSettingsVerifiers bs
    threshold = bridgeSettingsThreshold bs
    minAmount = bridgeSettingsMinimum bs
    fees = bridgeSettingsFees bs

-- Off-chain components

data InitParams = InitParams
  { initDatum :: BridgeSettings
  , initValue :: Value
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

data UpdateParams = UpdateParams
  { updateDatum :: BridgeSettings
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

type CKBAddress = BuiltinByteString

data LockParams = LockParams
  { lockValue :: Integer
  , lockAddress :: CKBAddress
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

data UnlockParams =
  UnlockParams
  { unlockParams :: Integer
  , unlockTo :: PaymentPubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

type BridgeSchema =
  Endpoint "init" InitParams
  .\/ Endpoint "update" UpdateParams
  .\/ Endpoint "lock" LockParams
  .\/ Endpoint "unlock" UnlockParams

bridge :: Contract () BridgeSchema T.Text ()
bridge = do
  logInfo @Haskell.String "Waiting for verifiers..."
  selectList [init, update] >> bridge

init :: AsContractError e => Promise () BridgeSchema e ()
init = endpoint @"init" @InitParams $ \(InitParams bs v) -> do
  logInfo @Haskell.String $ "Initilazing bridge using " <> Haskell.show bs
  let tx = Constraints.mustPayToTheScript (CurrentSettings bs) v
  void $ submitTxConstraints bridgeInstance tx
  logInfo @Haskell.String $ "Bridge is open"

update :: Promise () BridgeSchema T.Text ()
update = endpoint @"update" @UpdateParams $ \(UpdateParams nbs) -> do
  when (not . validSettings $ nbs) $ throwError "Not a valid bridge datum"
  logInfo @Haskell.String $ "Updating bridge to " <> Haskell.show nbs
  (oref, o, bs) <- getBridgeSettings
  logInfo @Haskell.String $ "Found prev bridge datum " <> Haskell.show bs
  let
    r = Redeemer $ PlutusTx.toBuiltinData $ Update nbs
    bd' = CurrentSettings nbs
    v = Ada.lovelaceValueOf minLovelace

    lookups = Constraints.typedValidatorLookups bridgeInstance Haskell.<>
              Constraints.otherScript validator Haskell.<>
              Constraints.unspentOutputs (Map.singleton oref o)

    tx = Constraints.mustPayToTheScript bd' v Haskell.<>
         Constraints.mustSpendScriptOutput oref r

  void $ submitTxConstraintsWith lookups tx
  logInfo @Haskell.String "Updated bridge settings"
  return ()

lock :: Promise () BridgeSchema T.Text ()
lock = endpoint @"lock" @LockParams $ \(LockParams amount ckbAddress) -> do
  (_, _, bs) <- getBridgeSettings
  let
    feeAddress = bridgeSettingsFeeAddress bs
    fees = bridgeSettingsFees bs
    amountAfterFees = amount - fees

    tx = Constraints.mustPayToTheScript (LockRecord ckbAddress) (Ada.lovelaceValueOf amountAfterFees) <>
         Constraints.mustPayToPubKey feeAddress (Ada.lovelaceValueOf fees)
  void $ submitTxConstraints bridgeInstance tx
  logInfo @Haskell.String "Funds locked"

unlock :: Promise () BridgeSchema T.Text ()
unlock = endpoint @"unlock" @UnlockParams $ \(UnlockParams amount toAddr) -> do
  utxos <- utxosAt bridgeAddress
  (_, _, _) <- getBridgeSettings
  let
    lookups = Constraints.typedValidatorLookups bridgeInstance Haskell.<>
              Constraints.otherScript validator Haskell.<>
              Constraints.unspentOutputs utxos
    tx = Constraints.mustPayToPubKey toAddr $ Ada.lovelaceValueOf amount
  void $ submitTxConstraintsWith lookups tx
  logInfo @Haskell.String "Funds unlocked"

minLovelace :: Integer
minLovelace = 2000000

getBridgeSettings :: Contract w s T.Text (TxOutRef, ChainIndexTxOut, BridgeSettings)
getBridgeSettings = do
  utxos <- utxosAt bridgeAddress
  maybe (throwError "No bridge datum found") return $ listToMaybe $ catMaybes $ fmap (\(oref, o) -> (oref, o,) <$> datumMay o) $ Map.toList utxos
  where
    datumMay o = do
      Datum d <- either (const Nothing) Just (_ciTxOutDatum o)
      v <- PlutusTx.fromBuiltinData d
      case v of
        CurrentSettings bs -> Just bs
        _ -> Nothing
