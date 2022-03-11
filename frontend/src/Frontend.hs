{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Text.Read (readMaybe)
import Control.Monad
import Control.Applicative (liftA2)
import Data.Bool (bool)
import qualified Data.Map as Map
import Data.Maybe (isJust, maybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle ( eval
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

import JSDOM

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Cardano.Binary

import Reflex.Dom.Core

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

import qualified Nami

data BridgeDirection =
  BridgeIn | BridgeOut
  deriving (Eq, Show)

tShow :: (Show a) => a -> T.Text
tShow = T.pack . show

changeBridgeDirection :: BridgeDirection -> BridgeDirection
changeBridgeDirection BridgeIn = BridgeOut
changeBridgeDirection BridgeOut = BridgeIn

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


class Pretty a where
  pretty :: a -> T.Text

instance Pretty BridgeDirection where
  pretty BridgeIn = "Cardano -> Nervos"
  pretty BridgeOut = "Nervos -> Cardano"

type CKBAddress = T.Text

data BridgeInTx =
  BridgeInTx { bridgeInAmount :: Double
             , bridgeInToAddress :: CKBAddress
             }

inChain :: BridgeDirection -> T.Text
inChain BridgeIn = "Cardano"
inChain BridgeOut = "Nervos"

outChain :: BridgeDirection -> T.Text
outChain BridgeOut = "Cardano"
outChain BridgeIn = "Nervos"

form :: (PostBuild t m, DomBuilder t m, Prerender t m) => Double -> BridgeDirection -> m (Dynamic t (Maybe Double))
form b direction = do
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

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Force Bridge"
      elAttr "link" ("href" =: $(static "favicon.ico") <> "rel" =: "icon") blank
      elAttr "link" ("href" =: $(static "css/output.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src" =: $(static "js/app.bundle.js")) blank
  , _frontend_body = elClass "div" "flex flex-col w-screen h-screen text-gray-600" $ do

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
          Right api -> do
            elClass "div" "rounded-2xl w-90 bg-white p-6 shadow-md" $ do
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

              amountThing <- dyn $ form balance <$> currentDirection
              amount <- fmap join $ holdDyn (pure Nothing) amountThing

              ckbAddress <- elClass "div" "border rounded-lg p-4 mb-4" $ do
                elClass "div" "text-bold" $ text "Recipient:"
                ie <- inputElement $ def
                  & initialAttributes .~ ("placeholder" =: "enter your ckb address"
                                        <> "class" =: "focus:outline-none text-gray-700"
                                       )
                pure $ _inputElement_value ie

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
                inTx = liftA2 BridgeInTx <$> amount <*> (pure <$> ckbAddress)

              performEvent_ $ maybe (pure ()) (\a -> Nami.pay api a Nami.deepakBech32 1) addr <$ (gate (current $ isJust <$> amount) $ domEvent Click submitButton)
          _ -> do
            text "You require nami wallet"
      pure ()
  }

fee :: Double
fee = 0.001

checkAmount :: Double -> Maybe Double -> Bool
checkAmount balance (Just amount)
  | balance > amount && amount > 0  = True
checkAmount _ _ = False
