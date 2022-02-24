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
import Data.Maybe (isJust, maybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle ( eval
                                   , liftJSM
                                   , MonadJSM
                                   , js
                                   )

import JSDOM

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Cardano.Binary

import Reflex.Dom.Core

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

-- <svg width="62px" height="32px" viewBox="0 0 627 260" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M193.6 105.8V135.7H177V61.3999H236.3V74.9999H193.7V92.1999H229.7V105.8H193.6Z" fill="black"></path><path d="M331.3 98.5C331.3 106.3 329.6 113.2 326.2 119C322.8 124.9 317.8 129.4 311.3 132.6C304.8 135.8 297.1 137.4 288.2 137.4C279.6 137.4 272.1 135.9 265.7 132.8C259.3 129.7 254.3 125.2 250.7 119.4C247.1 113.5 245.3 106.6 245.3 98.4C245.3 90.6 247 83.7 250.4 77.9C253.8 72 258.8 67.5 265.3 64.3C271.8 61.1 279.5 59.5 288.4 59.5C297 59.5 304.5 61 310.9 64.1C317.3 67.2 322.3 71.7 325.9 77.5C329.5 83.4 331.3 90.4 331.3 98.5ZM314 98.5C314 90.5 311.7 84.2 307.2 79.8C302.7 75.4 296.3 73.1 288.2 73.1C280.1 73.1 273.8 75.3 269.3 79.8C264.8 84.3 262.5 90.5 262.5 98.5C262.5 106.5 264.8 112.8 269.3 117.2C273.8 121.6 280.2 123.9 288.3 123.9C296.4 123.9 302.7 121.7 307.2 117.2C311.7 112.7 314 106.5 314 98.5Z" fill="black"></path><path d="M399 135.7L382.8 107.9H361V135.7H345V61.3999H381.5C402.2 61.3999 412.6 69.0999 412.6 84.5999C412.6 89.5999 411.4 93.7999 409.1 97.0999C406.8 100.4 403.2 103 398.4 104.9L417.1 135.7H399ZM380.3 95.0999C385.8 95.0999 389.8 94.2999 392.4 92.5999C395 90.8999 396.3 88.1999 396.3 84.4999C396.3 80.7999 395.1 78.1999 392.6 76.5999C390.1 74.9999 386.1 74.1999 380.5 74.1999H361V95.0999H380.3Z" fill="black"></path><path d="M502.7 110C501.5 118.8 497.6 125.6 490.8 130.4C484 135.2 475.1 137.6 464 137.6C455.7 137.6 448.4 136.1 442.2 133C436 129.9 431.1 125.5 427.7 119.6C424.3 113.7 422.6 106.7 422.6 98.5999C422.6 90.8999 424.2 84.1 427.5 78.3C430.8 72.4 435.6 67.8999 441.8 64.5999C448 61.2999 455.4 59.7 463.9 59.7C484.5 59.7 497 68.1999 501.5 85.1999L485 87.1999C483.3 82.2999 480.8 78.6999 477.5 76.5999C474.2 74.4999 469.7 73.4 464 73.4C456.6 73.4 450.7 75.6999 446.4 80.1999C442.1 84.6999 440 90.9 440 98.8C440 106.8 442.1 113 446.4 117.5C450.7 122 456.7 124.2 464.3 124.2C470.5 124.2 475.3 123.1 478.9 120.8C482.4 118.5 484.8 115 486 110.3H502.7V110Z" fill="black"></path><path d="M177 238.6V164.3H213.1C233.4 164.3 243.6 170.7 243.6 183.4C243.6 191.1 239.6 196.4 231.5 199.2C241.4 202.5 246.4 208.4 246.4 216.9C246.4 224 243.7 229.3 238.3 233C232.9 236.7 225.1 238.5 214.7 238.5H177V238.6ZM213.2 194C218.1 194 221.7 193.3 224 191.8C226.3 190.3 227.4 188.1 227.4 185C227.4 182.1 226.2 180 223.9 178.7C221.5 177.4 217.7 176.8 212.4 176.8H192.7V194H213.2ZM213.6 226.1C219.4 226.1 223.6 225.3 226.2 223.8C228.8 222.2 230.2 219.7 230.2 216.3C230.2 212.8 228.9 210.3 226.2 208.6C223.6 206.9 219.4 206.1 213.7 206.1H192.7V226.2H213.6V226.1Z" fill="black"></path><path d="M314.4 238.6L298.2 210.8H276.5V238.6H260.5V164.3H297C317.7 164.3 328.1 172 328.1 187.5C328.1 192.5 326.9 196.7 324.6 200C322.3 203.3 318.7 205.9 313.9 207.8L332.6 238.6H314.4ZM295.7 198.1C301.2 198.1 305.2 197.3 307.8 195.6C310.4 193.9 311.7 191.2 311.7 187.5C311.7 183.8 310.5 181.2 308 179.6C305.5 178 301.5 177.2 295.9 177.2H276.4V198.1H295.7Z" fill="black"></path><path d="M345.6 238.6V164.3H362.2V238.6H345.6Z" fill="black"></path><path d="M377.7 238.6V164.3H411C425.2 164.3 436.1 167.5 443.5 173.8C451 180.1 454.7 189.4 454.7 201.4C454.7 213.3 450.8 222.4 442.9 228.9C435 235.3 423.8 238.6 409.4 238.6H377.7ZM410.1 224.8C419.2 224.8 426 222.8 430.6 218.9C435.2 214.9 437.5 209.1 437.5 201.4C437.5 185.9 428.8 178.2 411.5 178.2H394.2V224.8H410.1Z" fill="black"></path><path d="M531.9 211H511.4V198.1H547.3V230.3C542.7 233.6 537.2 236.1 531 237.8C524.8 239.6 518.2 240.5 511.3 240.5C497.4 240.5 486.6 237.1 478.8 230.3C471 223.5 467.1 214 467.1 201.8C467.1 193.9 468.8 187 472.1 181.1C475.5 175.2 480.4 170.6 486.8 167.4C493.2 164.1 500.8 162.5 509.5 162.5C519.1 162.5 527 164.5 533.2 168.4C539.4 172.4 543.6 178 545.8 185.4L529.2 187.4C527.7 183.5 525.4 180.7 522.1 178.8C518.8 177 514.5 176 509.1 176C501.2 176 495.1 178.2 490.7 182.6C486.4 187 484.2 193.3 484.2 201.5C484.2 209.9 486.5 216.2 491.1 220.5C495.7 224.8 502.4 226.9 511.3 226.9C519.7 226.9 526.6 225.6 531.9 222.9V211Z" fill="black"></path><path d="M515.8 135.7V61.3999H580.4V74.9999H532.5V90.6999H580.4V104.3H532.5V122.1H580.4V135.7H515.8Z" fill="black"></path><path d="M562.4 238.6V164.3H627V177.9H579.1V193.6H627V207.2H579.1V225H627V238.6H562.4Z" fill="black"></path><path d="M0.1 68.8999L0 208.1C0 222.4 7.6 235.6 20 242.8L49.8 260L50 68.9999L0.1 68.8999Z" fill="#00CC9B"></path><path d="M21.3 62.1L0.0999756 40.4V74.4V131.9L114 66.2C123.3 60.8 129 50.9 129 40.2V0L21.3 62.1Z" fill="black"></path><path d="M15.1 138.5C5.79998 143.9 0.0999756 153.8 0.0999756 164.5V204.7L107.9 142.6L129.1 164.2V130.2V72.8L15.1 138.5Z" fill="black"></path></svg>
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
        elClass "div" "bg-blue-400 px-2 py-1 rounded-lg text-sm font-bold text-white drop-shadow-md" $ text "Ada | ada"
      elClass "div" "text-right text-blue-500 font-light" $ text $ "Max " <> tShow balance

    ie <- inputElement $ def
      & initialAttributes .~ ("placeholder" =: "0.0"
                             <> "class" =: "focus:outline-none text-gray-700"
                             )

    pure $ readMaybe . T.unpack <$> _inputElement_value ie

  elClass "div" "border rounded-lg p-4 mb-4" $ do
    elClass "div" "flex flex-row items-center justify-between mb-2" $ do
      elClass "div" "flex flex-row items-center" $ do
        elClass "div" "mr-2 font-bold" $ text $ outChain direction
        elClass "div" "bg-blue-100 px-2 py-1 rounded-lg text-sm font-bold drop-shadow-md" $ text "Ada | ada"

      -- TODO(skylar): Show the fee in proper encoding
      elClass "div" "text-right text-gray-400 font-light" $ text $ "Fee " <> "0.001"

    elClass "div" "text-gray-700" $ dynText $ ffor amount $ \case
      Nothing -> "0.0"
      Just v -> tShow $ v - fee

  pure amount

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Force Bridge"
      -- elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      -- <script src="https://cdn.tailwindcss.com"></script>
      elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank
  , _frontend_body = elClass "div" "flex flex-col w-screen h-screen text-gray-600" $ do

      -- address <- Nami.currentAddress
      rec
        currentDirection <- foldDyn ($) BridgeIn changeEvent

        changeEvent <- elClass "div" "bg-white w-full p-4 drop-shadow-md items-center flex flex-row justify-between" $ do
          elClass "h1" "font-semibold" $ text "Force Bridge Ob"

          (buttonEl, _) <- elClass' "button" "rounded-md bg-blue-200 font-semibold text-black px-4 py-2 drop-shadow-md" $ do
            dynText $ pretty <$> currentDirection

          elClass "div" "" $ el "div" $ text "Menu"

          pure $ changeBridgeDirection <$ domEvent Click buttonEl

      -- TODO(skylar): Do we care about the UI appearing properly in a prerender?
      elClass "div" "flex flex-grow bg-blue-100 justify-center items-center" $ prerender_ blank $ do
        eApi <- liftJSM $ Nami.getApi
        case eApi of
          Right api -> do
            elClass "div" "w-1/3 drop-shadow-xl bg-white rounded-lg p-4" $ do
              addr <- liftJSM $ Nami.getUsedAddress api
              balance <- liftJSM $ Nami.getBalance api
              elClass "div" "bg-blue-200 px-4 py-2 rounded-lg mb-4 drop-shadow-md truncate" $ case addr of
                Nothing -> text "Loading wallet"
                Just result -> text $ "Hex: " <> result

              -- amount <- form balance BridgeIn
              amountThing <- dyn $ form balance <$> currentDirection
              amount <- fmap join $ holdDyn (pure Nothing) amountThing

              ckbAddress <- elClass "div" "border rounded-lg p-4 mb-4" $ do
                elClass "div" "text-bold" $ text "input address"
                ie <- inputElement $ def
                  & initialAttributes .~ ("placeholder" =: "enter your ckb address"
                                        <> "class" =: "focus:outline-none text-gray-700"
                                       )
                pure $ _inputElement_value ie

              let
                mkBridgeButtonClasses b =
                  T.intercalate " " [ "duration-500 transition-all w-full border rounded-lg px-4 py-2 font-semibold"
                                    , bool "cursor-not-allowed bg-white" "bg-blue-400 drop-shadow-lg text-white border-transparent" b
                                    ]

              (submitButton, _) <- elDynClass' "button" (mkBridgeButtonClasses . checkAmount balance <$> amount) $ do
                dynText $ ffor (checkAmount balance <$> amount) $ \case
                  True -> "Bridge"
                  False -> "Enter an amount"

              let
                inTx = liftA2 BridgeInTx <$> amount <*> (pure <$> ckbAddress)

              performEvent_ $ maybe (pure ()) (Nami.signTest api) addr <$ (gate (current $ isJust <$> amount) $ domEvent Click submitButton)
          _ -> do
            text "You require nami wallet"

      pure ()
  }

fee :: Double
fee = 0.001

-- TODO(skylar): This isn't correct, where is the fee
checkAmount :: Double -> Maybe Double -> Bool
checkAmount balance (Just amount)
  | balance > amount && amount > 0  = True
checkAmount _ _ = False
