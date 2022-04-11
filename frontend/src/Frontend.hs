{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

{-|
Description: Frontend UI for force-bridge
-}

module Frontend where

import Text.Read (readMaybe)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Applicative (liftA2)
import Control.Lens
import Data.Time
import Data.Bool (bool)
import qualified Data.Map as Map
import Data.Maybe (isJust, maybe)
import qualified Data.Text as T
import Language.Javascript.JSaddle ( MonadJSM
                                   , liftJSM
                                   , toJSVal
                                   , maybeNullOrUndefined
                                   , MonadJSM
                                   , ToJSVal
                                   , JSVal
                                   , JSM
                                   , jsg
                                   , js
                                   , js1
                                   )

import qualified Data.Map as Map

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core hiding (now)

import qualified Reflex.Dom.Widget.SVG as S
import Reflex.Dom.Widget.SVG.Types (SVG_Rect)
import qualified Reflex.Dom.Widget.SVG.Types as S
import qualified Reflex.Dom.Widget.SVG.Types.SVG_Path as S
import Control.Lens ((^?), (+~), (?~), (#), (^.), from, at, _Wrapped)
import Data.Function ((&))
import Data.Monoid (mempty, mappend)
import Reflex (Dynamic)
import Control.Monad.Fix (MonadFix)
import qualified Data.List.NonEmpty as NE


import Common.Api
import Common.Route
import Common.Bridge

import qualified Nami
import Nami (BridgeInTx(..))

import Cardano

-- | The direction assets are flowing: In for into nervos, Out for out of nervos
data BridgeDirection =
  BridgeIn | BridgeOut
  deriving (Eq, Show)

-- | Makes any showable type into Text instead of a String
tShow :: (Show a) => a -> T.Text
tShow = T.pack . show

-- | Flips the bridge direction
changeBridgeDirection :: BridgeDirection -> BridgeDirection
changeBridgeDirection _ = BridgeIn

fbSvg :: forall t m. (PostBuild t m, DomBuilder t m) => m ()
fbSvg = elAttr "img" ("src" =: $(static "svgs/forcebridgeLogo.svg") <> "class" =: "w-svg-logo h-svg-logo") blank

svgClass = "w-svg h-svg inline"

nervosSvg :: (PostBuild t m, DomBuilder t m) => m ()
nervosSvg = elAttr "img" ("src" =: $(static "svgs/nervos.svg") <> "class" =: svgClass) blank

cardanoSvg :: (PostBuild t m, DomBuilder t m) => m ()
cardanoSvg = elAttr "img" ("src" =: $(static "svgs/cardano.svg") <> "class" =: svgClass) blank

filledCircleArrowSvg :: forall t m. (PostBuild t m, DomBuilder t m) => m ()
filledCircleArrowSvg = elAttr "img" ("src" =: $(static "svgs/filledCircleArrow.svg") <> "class" =: "w-svg-lg h-svg-lg") blank

arrowSvg :: forall t m. (PostBuild t m, DomBuilder t m) => m ()
arrowSvg = elAttr "img" ("src" =: $(static "svgs/arrow.svg") <> "class" =: svgClass) blank

menuSvg :: (PostBuild t m, DomBuilder t m) => m ()
menuSvg = elAttr "img" ("src" =: $(static "svgs/menu.svg") <> "class" =: svgClass) blank

copySvg :: (PostBuild t m, DomBuilder t m) => m ()
copySvg = elAttr "img" ("src" =: $(static "svgs/copy.svg") <> "class" =: svgClass) blank

greenCheckmarkSvg :: (PostBuild t m, DomBuilder t m) => m ()
greenCheckmarkSvg = elAttr "img" ("src" =: $(static "svgs/greenCheckmark.svg") <> "class" =: svgClass) blank

copyToClipboard :: MonadJSM m => T.Text -> m ()
copyToClipboard addr = liftJSM $ do

  navJSVal <- jsg ("navigator" :: T.Text)
  mNavigator <- maybeNullOrUndefined navJSVal

  case mNavigator of
    Nothing -> pure ()
    Just nav -> do
      nav ^. js ("clipboard" :: T.Text) . js1 ("writeText" :: T.Text) addr
      pure ()

  pure ()

-- | Class for pretty printing of types
class Pretty a where
  pretty :: a -> T.Text

instance Pretty BridgeDirection where
  pretty BridgeIn = "Cardano -> Nervos"
  pretty BridgeOut = "Nervos -> Cardano"

-- | Type to represent if we are entering into the form or waiting for the wallet to sign something
data Action = Form | Signing

-- | What is the name of the chain we are moving assets out of
inChain :: BridgeDirection -> T.Text
inChain BridgeIn = "Cardano"
inChain BridgeOut = "Nervos"

-- | What is the name of the chain we are moving assets into
outChain :: BridgeDirection -> T.Text
outChain BridgeOut = "Cardano"
outChain BridgeIn = "Nervos"

-- | The main bridge form, this will adjust based on the bridge direction given
form :: (PostBuild t m, DomBuilder t m) => Event t () -> Double -> BridgeDirection -> m (Dynamic t (Maybe Double))
form clearForm b direction = do
  let
    balance = case direction of
      BridgeIn -> b
      _ -> 0
  
  amount <- elClass "div" "border rounded-lg p-4 mb-4" $ do
    elClass "div" "flex flex-row items-center justify-between mb-4" $ do
      elClass "div" "flex flex-row items-center" $ do
        elClass "div" "mr-2 font-bold" $ text $ inChain direction
        elClass "div" "bg-primary px-2 py-1 rounded-lg text-sm font-bold text-white drop-shadow-md" $ do
          el "span" cardanoSvg
          text "Ada | ada"
      elClass "div" "text-right text-primary font-light" $ text $ "Max " <> tShow balance

    ie <- inputElement $ def
      & initialAttributes .~ ("placeholder" =: "0.0"
                             <> "class" =: "focus:outline-none text-gray-700"
                             )
      & inputElementConfig_setValue .~ ("" <$ clearForm)

    pure $ readMaybe . T.unpack <$> _inputElement_value ie

  elClass "div" "flex justify-center" filledCircleArrowSvg

  elClass "div" "border rounded-lg p-4 mb-4" $ do
    elClass "div" "flex flex-row items-center justify-between mb-2" $ do
      elClass "div" "flex flex-row items-center" $ do
        elClass "div" "mr-2 font-bold" $ text $ outChain direction
        elClass "div" "select-none bg-gradient-to-r from-secondary to-secondary-end px-2 py-1 rounded-lg text-sm font-bold drop-shadow-md" $ do
          el "span" cardanoSvg
          text "Ada | ada"

      elClass "div" "text-right text-gray-400 font-light" $ text $ "Fee " <> "0.001"

    elClass "div" "text-gray-700" $ dynText $ ffor amount $ \case
      Nothing -> "0.0"
      Just v -> tShow $ v - fee

  pure amount


truncateMiddleText :: T.Text -> Int -> T.Text
truncateMiddleText s l
  | length (T.unpack s) <= l = s
  | l < 3 = s
  | otherwise =
    let
      s' = T.unpack s
      (a, b) = quotRem l 2
      h = take a s'
      t = drop (length s' - (a + b)) s'
    in T.pack $ h <> "..." <> t

truncateLength :: Int
truncateLength = 20

chainSvg :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m) => T.Text -> m ()
chainSvg "Nervos" = nervosSvg
chainSvg "Cardano" = cardanoSvg

-- | Main frontend component of force-bridge
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Force Bridge"
      elAttr "link" ("href" =: $(static "favicon.ico") <> "rel" =: "icon") blank
      elAttr "link" ("href" =: $(static "css/output.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src" =: $(static "js/app.bundle.js")) blank
  , _frontend_body = elClass "div" "flex flex-col w-screen h-screen text-gray-600 overflow-hidden" $ do

      rec
        currentDirection <- foldDyn ($) BridgeIn changeEvent

        changeEvent <- elClass "div" "bg-white w-full p-4 drop-shadow-md items-center flex flex-row justify-between" $ do
          el "div" fbSvg

          (buttonEl, _) <- elClass' "button" "font-bold rounded-md bg-gradient-to-r from-secondary to-secondary-end text-black px-4 py-2 drop-shadow-md" $ do
            let dInChain = inChain <$> currentDirection
                dOutChain = outChain <$> currentDirection
            dyn_ $ chainSvg <$> dInChain
            dynText $ dInChain

            arrowSvg

            dyn_ $ chainSvg <$> dOutChain
            dynText $ dOutChain

          elClass "div" "rounded-md bg-gradient-to-r from-secondary to-secondary-end text-sm w-6 h-6 text-center" menuSvg

          pure $ changeBridgeDirection <$ domEvent Click buttonEl
  
      elClass "div" "flex bg-gradient-to-br from-tertiary to-tertiary-end flex-grow justify-center items-center pt-36 pb-8" $ prerender_ blank $ do
        eApi <- liftJSM $ Nami.getApi
        case eApi of
          Right api -> mdo
            (submitTx, waiting, _) <- elClass "div" "rounded-2xl w-90 bg-white p-6 shadow-md" $ mdo
              addr <- liftJSM $ Nami.getUsedAddress api
              balance <- liftJSM $ Nami.getBalance api
              elClass "div" "group relative select-none bg-gradient-to-r from-secondary to-secondary-end px-4 py-2 \
                            \ rounded-lg mb-4 drop-shadow-md text-black text-center font-bold" $ case addr of
                Nothing -> text "Loading wallet"
                Just result -> do
                  let copiedText True = "Copied"
                      copiedText _ = "Copy"
                      copiedSvg True = greenCheckmarkSvg
                      copiedSvg _ = copySvg

                  rec
                    bCopiedAddr <- foldDyn (||) False copyAddrEvent

                    copyAddrEvent <- elClass "div" "absolute left-0 rounded-lg bg-black text-white bg-opacity-75 bottom-full px-4 py-4 \
                                                   \ break-all invisible group-hover:visible text-left" $ do
                      elClass "span" "selection:bg-secondary selection:text-black select-all" $ text result

                      elClass "span" "group-one relative ml-1" $ do
                        elClass "div" "absolute left-0 rounded-lg bg-black text-white bg-opacity-75 bottom-full px-4 py-4 \
                                      \ invisible group-one-hover:visible text-left" $ do
                          elClass "div" "break-normal" $ dynText $ copiedText <$> bCopiedAddr

                        (copyBtn, _) <- elClass' "button" "" $ dyn_ $ copiedSvg <$> bCopiedAddr

                        performEvent_ $ copyToClipboard result <$ domEvent Click copyBtn
                        pure $ True <$ domEvent Click copyBtn

                  el "div" $ text $ truncateMiddleText result truncateLength

              amountThing <- dyn $ form (() <$ submitRequest) balance <$> currentDirection
              amount <- fmap join $ holdDyn (pure Nothing) amountThing

              ckbAddress <- elClass "div" "border rounded-lg p-4 mb-4" $ do
                elClass "div" "text-bold" $ text "Recipient:"
                ie <- inputElement $ def
                  & initialAttributes .~ ("placeholder" =: "enter your ckb address"
                                        <> "class" =: "focus:outline-none text-gray-700"
                                       )
                  & inputElementConfig_setValue .~ ("" <$ submitRequest)
                pure $ _inputElement_value ie

              let
                hasError = (\x -> (not . isJust $ Nami.mkCKBAddress x) && (not $ T.null x)) <$> ckbAddress

              dyn_ $ ffor hasError $ \case
                True -> elClass "div" "text-red-300 text-sm pt-2" $ text "Not a valid ckb address"
                False -> blank

              let
                mkBridgeButtonClasses b =
                  T.intercalate " " [ "duration-500 transition-all w-full border rounded-lg px-4 py-2 font-semibold"
                                    , bool "cursor-not-allowed bg-white" "bg-primary drop-shadow-lg text-white border-transparent" b
                                    ]

              (submitButton, _) <- elDynClass' "button" (mkBridgeButtonClasses . checkAmount balance <$> amount) $ do
                dynText $ ffor (checkAmount balance <$> amount) $ \case
                  True -> "Bridge"
                  False -> "Enter an amount"

              let
                clickSubmit = domEvent Click submitButton
                bridgeRequest = liftA2 Nami.BridgeRequest <$> amount <*> (Nami.mkCKBAddress <$> ckbAddress)
                submitRequest = tagMaybe (current bridgeRequest) clickSubmit
                submitFunc s =
                  maybe (const $ pure $ Left Nami.TxSendFailure) (\a -> Nami.doPay api s a) addr

                doSubmit = attachWith submitFunc (slot <$> current cb) submitRequest

              submitting <- holdDyn False $ leftmost [ True <$ doSubmit
                                                     , False <$ resultTx
                                                     ]
              resultTx <- performEvent doSubmit

              err <- holdDyn Nothing $ Just <$> fmapMaybe (preview _Left) resultTx

              dyn_ $ ffor err $ \case
                Just msg -> elClass "div" "text-red-300 text-sm pt-2" $ text $ tShow msg
                Nothing -> blank

              pure $ (fmapMaybe (preview _Right) resultTx, submitting, err)

            ct <- currentTime
            cb <- currentBlock

            elClass "div" "mt-4" $ mdo

              elClass "div" "ml-2 font-semibold text-lg text-gray-600 mb-1" $ text "Bridge Transactions"
              txs <- Nami.readTxs
              history <- foldDyn ($) txs $ mconcat [ (\x -> Map.insert (Nami.bridgeInTxHash x) x) <$> submitTx
                                                   , updates
                                                   ]
              performEvent_ $ Nami.writeTxs <$> updated history
              updatesMap <- list history (bridgeInTx cb ct)
              let
                updates =
                  switchDyn $ mconcat . (fmap . fmap) (\(h, s) -> Map.update (\x -> Just $ x { bridgeInTxStatus = s }) h) . Map.elems <$> updatesMap
              pure ()

            let mkScrimClasses b  =
                  classList [ "absolute top-0 left-0 w-full h-full bg-gray-400/50 flex justify-center items-center"
                            , bool "opacity-0 pointer-events-none" "opacity-100 pointer-events-auto" b
                            ]
            elDynClass "div" (mkScrimClasses <$> waiting) $ do
              elClass "div" "p-4 rounded-lg bg-white text-lg drop-shadow-lg font-semibold" $ text "Waiting for wallet signature"

            recentTransactionsFeed $ Nami.bridgeInTxHash <$> submitTx
-- <<<<<<< HEAD
-- =======
--               performEvent_ $ maybe (pure ()) (\a -> Nami.pay api a Nami.deepakBech32 1) addr <$ (gate (current $ isJust <$> amount) $ domEvent Click submitButton)
-- >>>>>>> origin/add-fb-styling
          _ -> do
            text "You require nami wallet"
      pure ()
  }

-- | Widget that shows all recent transactions, removing them automatically after a 10 second interval
recentTransactionsFeed :: ( MonadFix m
                          , MonadHold t m
                          , PostBuild t m
                          , PerformEvent t m
                          , TriggerEvent t m
                          , MonadIO (Performable m)
                          , DomBuilder t m
                          ) => Event t Nami.TxHash -> m ()
recentTransactionsFeed newHash = mdo
  tickEv <- dyn $ ffor (null <$> hashes) $ \case
    False -> tickLossyFromPostBuildTime 10
    True -> pure never
  tick <-switchHold never tickEv
  hashes <- foldDyn ($) [] $ leftmost [ (:) <$> newHash
                                      , tail <$ tick
                                      ]
  _ <- elClass "div" "absolute pointer-events-none top-0 left-0 w-full h-full overflow-hidden flex flex-col justify-start items-end p-4" $ do
      simpleList hashes txPrompt
  pure ()

-- | Widget to show a new transaction blurb
txPrompt :: (PostBuild t m, DomBuilder t m) => Dynamic t Nami.TxHash -> m ()
txPrompt txHash = do
  elDynAttr "a" (mkAttrs <$> txHash) $ do
    elClass "div" "text-xs" $ text "click to view on cardanoscan"
    elClass "div" "font-semibold" $ text "New transaction:"
    elClass "div" "text-lg font-bold text-blue-400" $ dynText $ T.take 8 <$> txHash
  where
    mkAttrs t =
      "class" =: "bg-white rounded-lg drop-shadow-lg p-4 w-1/4 mb-4 pointer-events-auto" <>
      "target" =: "_blank" <>
      "href" =: ("https://testnet.cardanoscan.io/transaction/" <>  t)

-- | Combine a list of class strings into a single string that is space separated
classList :: [T.Text] -> T.Text
classList = T.intercalate " "

-- | Current fee
fee :: Double
fee = 0.001

-- | Current time as a dynamic
currentTime :: (Prerender t m, PostBuild t m) => m (Dynamic t UTCTime)
currentTime = do
  result <- prerender (pure $ pure $ UTCTime undefined undefined) $ do
    t <- liftIO $ getCurrentTime
    update <- tickLossyFromPostBuildTime 60
    newTime <- performEvent $ liftIO getCurrentTime <$ update
    holdDyn t newTime
  pure $ join result

-- | Convert a time difference into a readable duration
mkDuration :: UTCTime -> UTCTime -> T.Text
mkDuration s c
  | diff < 60 = "less than 1 minute"
  | diff < hour = tShow (floorInt $ diff/ 60) <> " minute(s)"
  | otherwise = tShow (floorInt $ diff/hour) <> " hour(s)"
  where
    floorInt :: NominalDiffTime -> Int
    floorInt = floor

    diff = diffUTCTime c s

    hour :: NominalDiffTime
    hour = 60 * 60

-- | Get status of transaction from the current time, block and transaction information
toStatus :: UTCTime -> Block -> Maybe Tx -> Nami.Status
toStatus ct b mtx = case mtx of
  Nothing -> Nami.LockSubmitted
  Just _ ->
    case confirmations >= 10 of
      True -> Nami.LockAwaitingConfirmations confirmations
      False -> Nami.Bridged ct
  where
    confirmations = maybe 0 (min 0 . subtract (height b) . block_height) mtx

-- | A dynamic representing the current block on cardano's chain
currentBlock :: (DomBuilder t m,MonadJSM (Performable m), MonadFix m, TriggerEvent t m, PerformEvent t m, MonadHold t m, PostBuild t m) => m (Dynamic t Block)
currentBlock = do
  pb <- getPostBuild
  b <- tickLossyFromPostBuildTime 60
  newBlock <- getBlock $ leftmost [() <$ b, pb]

  dBlock <- holdDyn (Block 0 0) newBlock
  display dBlock
  pure dBlock

-- | Given a tx hash will give you a dynamic holding the current transaction info (if it exists)
pollTx :: (MonadJSM (Performable m), MonadFix m, TriggerEvent t m, PerformEvent t m, MonadHold t m, PostBuild t m) => Dynamic t Nami.TxHash -> m (Dynamic t (Maybe Tx))
pollTx txHash = do
  pb <- getPostBuild
  t <- tickLossyFromPostBuildTime 60
  newTx <- getTransaction $ tag (current txHash) $ leftmost [ () <$ t
                                                            , pb
                                                            ]
  holdDyn Nothing newTx

-- | Widget for visualizing the state of a bridge in transaction
bridgeInTx :: (MonadHold t m, MonadFix m, MonadJSM (Performable m), PerformEvent t m, PostBuild t m, DomBuilder t m, TriggerEvent t m) => Dynamic t Block -> Dynamic t UTCTime -> Dynamic t Nami.BridgeInTx -> m (Event t (Nami.TxHash, Nami.Status))
bridgeInTx block now btx = do
  ret <- dyn $ ffor (bridgeInTxStatus <$> btx) $ \case
    Nami.Bridged _ -> pure never
    _ -> do
      ct <- pollTx tx >>= holdUniqDyn
      let
        status = toStatus <$> now <*> block <*> ct
      pure $ attach (current tx) $ updated status
  elClass "div" "bg-white rounded-lg drop-shadow-lg p-4 w-full mb-4" $ do
    elClass "div" "border-b flex justify-between items-center" $ do
      elClass "div" "font-semibold text-lg" $ text "Lock"
      elClass "div" "text-sm" $ dyn_ $ ffor (bridgeInTxStatus <$> btx) $ \case
        Nami.LockSubmitted -> do
          text "Waiting for wallet to submit transaction"
        Nami.LockAwaitingConfirmations n -> do
          text $ "Awaiting confirmations " <> tShow n <> "/10"
        Nami.Bridged atTime -> do
          text "Completed "
          dynText $ mkDuration atTime <$> now
          text " ago"
        _ -> blank
    elDynAttr "a" (mkHashAttrs <$> tx) $ dynText $ bridgeInTxHash <$> btx
    elClass "div" "mt-2 mb-2 flex justify-between" $ do
      elClass "div" "font-semibold" $ text "To "
      elClass "div" "" $ dynText $ unCKBAddress . bridgeInToAddress <$> btx
    elClass "div" "flex justify-between" $ do
      elClass "div" "font-semibold" $ text "Ada "
      elClass "div" "" $ dynText $ tShow . bridgeInAmount <$> btx

    dyn_ $ ffor (isBridged . bridgeInTxStatus <$> btx) $ \case
      False -> do
        elClass "div" "flex justify-between mt-6" $ do
          elClass "div" "font-semibold text-sm" $ text "Duration "
          elClass "div" "text-xs" $ dynText $ zipDynWith mkDuration startTime now
        elClass "div" "mt-1 text-xs" $ text "*This could take up to 10 hours with current congestion"
      True -> blank
  switchHold never ret
  where
    mkHashAttrs t =
      "class" =: "truncate font-semibold text-blue-700/50" <>
      "target" =: "_blank" <>
      "href" =: ("https://testnet.cardanoscan.io/transaction/" <>  t)
    tx = bridgeInTxHash <$> btx
    startTime = bridgeInStart <$> btx

    isBridged (Nami.Bridged _) = True
    isBridged _ = False

-- | Validate if amount is enough to bridge
checkAmount :: Double -> Maybe Double -> Bool
checkAmount balance (Just amount)
  | balance > amount && amount > 0  = True
checkAmount _ _ = False
