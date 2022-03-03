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
-- <svg viewBox="0 0 60 61" xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" fill="currentColor" aria-hidden="true" focusable="false" class=""><path d="M0 0.0227051V60.0227H15.3559V27.3292H27.1276L0 0.0227051Z" fill="black"></path><path d="M44.6441 0.0227051V32.7165H32.8727L60 60.0227V0.0227051H44.6441Z" fill="black"></path></svg>
{-
<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
	 viewBox="0 0 375 346.5" style="enable-background:new 0 0 375 346.5;" xml:space="preserve">
<style type="text/css">
	.st0{fill:#0033AD;}
</style>
<g id="Layer_2_1_">
	<g id="Layer_1-2">
		<path class="st0" d="M102.8,172c-0.8,13.9,9.9,25.8,23.8,26.6c0.5,0,1,0,1.5,0c14,0,25.3-11.3,25.2-25.3c0-14-11.3-25.3-25.3-25.2
			C114.6,148.1,103.5,158.6,102.8,172z"/>
		<path class="st0" d="M8.6,165.5c-4.5-0.3-8.4,3.2-8.6,7.7s3.2,8.4,7.7,8.6c4.5,0.3,8.3-3.2,8.6-7.7
			C16.6,169.6,13.1,165.8,8.6,165.5C8.6,165.5,8.6,165.5,8.6,165.5z"/>
		<path class="st0" d="M101.2,25.4c4-2,5.6-7,3.6-11c-2-4-7-5.6-11-3.6c-4,2-5.6,6.9-3.6,10.9C92.2,25.8,97.1,27.5,101.2,25.4
			C101.1,25.4,101.2,25.4,101.2,25.4z"/>
		<path class="st0" d="M126.8,70.1c6.2-3.1,8.7-10.7,5.6-16.9s-10.7-8.7-16.9-5.6c-6.2,3.1-8.7,10.7-5.6,16.9
			C113,70.7,120.6,73.2,126.8,70.1z"/>
		<path class="st0" d="M40.6,100.8c4.8,3.1,11.2,1.8,14.4-3c3.1-4.8,1.8-11.2-3-14.4c-4.8-3.1-11.2-1.8-14.4,3c0,0,0,0,0,0
			C34.4,91.2,35.8,97.7,40.6,100.8z"/>
		<path class="st0" d="M55.9,161c-7-0.4-12.9,4.9-13.3,11.9s4.9,12.9,11.9,13.3c7,0.4,12.9-4.9,13.3-11.9c0,0,0,0,0,0
			C68.2,167.4,62.9,161.4,55.9,161z"/>
		<path class="st0" d="M42,245.7c-5.1,2.6-7.2,8.8-4.6,14c2.6,5.1,8.8,7.2,14,4.6c5.1-2.6,7.2-8.8,4.6-14c0,0,0,0,0,0
			C53.4,245.2,47.1,243.1,42,245.7C42,245.7,42,245.7,42,245.7z"/>
		<path class="st0" d="M91,134.9c6.9,4.5,16.1,2.6,20.5-4.3c4.5-6.9,2.6-16.1-4.3-20.5c-6.9-4.5-16.1-2.6-20.5,4.3
			C82.2,121.2,84.1,130.4,91,134.9C91,134.9,91,134.9,91,134.9z"/>
		<path class="st0" d="M246.5,69.1c5.8,3.8,13.7,2.2,17.5-3.6s2.2-13.7-3.6-17.5c-5.8-3.8-13.7-2.2-17.5,3.6c0,0,0,0,0,0
			C239,57.5,240.6,65.3,246.5,69.1C246.5,69.1,246.5,69.1,246.5,69.1z"/>
		<path class="st0" d="M272.3,24.6c3.8,2.5,8.8,1.4,11.3-2.4s1.4-8.8-2.4-11.3c-3.8-2.5-8.8-1.4-11.3,2.3
			C267.5,17,268.6,22.1,272.3,24.6C272.3,24.6,272.3,24.6,272.3,24.6z"/>
		<path class="st0" d="M248.4,147.9c-13.9-0.8-25.9,9.9-26.6,23.8c-0.8,13.9,9.9,25.9,23.8,26.6c0.5,0,1,0,1.4,0
			c13.9,0,25.2-11.3,25.2-25.3C272.3,159.7,261.8,148.6,248.4,147.9L248.4,147.9z"/>
		<path class="st0" d="M135.1,133.1c4.3,8.5,13,13.9,22.6,13.9c13.9,0,25.2-11.3,25.2-25.3c0-3.9-0.9-7.8-2.7-11.4
			c-6.3-12.5-21.5-17.5-33.9-11.2C133.8,105.5,128.8,120.7,135.1,133.1L135.1,133.1z"/>
		<path class="st0" d="M333,100.8c5.1-2.6,7.1-8.9,4.5-14c-2.6-5.1-8.9-7.1-14-4.5c-5.1,2.6-7.1,8.8-4.6,13.9
			C321.6,101.3,327.8,103.4,333,100.8C333,100.8,333,100.8,333,100.8z"/>
		<path class="st0" d="M269,108.8c-7.3,3.7-10.3,12.6-6.6,19.9c3.7,7.3,12.6,10.3,19.9,6.6c7.3-3.7,10.3-12.6,6.6-19.9
			C285.2,108.1,276.3,105.2,269,108.8z"/>
		<path class="st0" d="M186.5,20.8c5.7,0.3,10.6-4.1,11-9.8s-4.1-10.6-9.8-11c-5.7-0.3-10.6,4-11,9.7
			C176.4,15.5,180.8,20.4,186.5,20.8C186.5,20.8,186.5,20.8,186.5,20.8z"/>
		<path class="st0" d="M186.4,86.1c8.2,0.5,15.2-5.8,15.6-14c0.5-8.2-5.8-15.2-14-15.6c-8.2-0.5-15.2,5.8-15.6,14
			C172,78.7,178.2,85.7,186.4,86.1C186.4,86.1,186.4,86.1,186.4,86.1z"/>
		<path class="st0" d="M106,237.7c7.3-3.7,10.3-12.6,6.6-19.9c-3.7-7.3-12.6-10.3-19.9-6.6c-7.3,3.7-10.3,12.6-6.6,19.9
			C89.8,238.4,98.7,241.4,106,237.7z"/>
		<path class="st0" d="M196,107.8c-7.6,11.7-4.4,27.3,7.3,34.9c11.7,7.6,27.3,4.4,34.9-7.3c7.6-11.7,4.4-27.3-7.3-34.9
			c-4.1-2.7-8.9-4.1-13.8-4.1C208.6,96.4,200.7,100.7,196,107.8z"/>
		<path class="st0" d="M239.9,213.4c-6.3-12.5-21.5-17.5-33.9-11.2c-12.5,6.3-17.5,21.5-11.2,33.9c6.3,12.5,21.5,17.5,33.9,11.2
			c0,0,0,0,0,0c12.4-6.2,17.5-21.2,11.3-33.7C240,213.5,240,213.5,239.9,213.4z"/>
		<path class="st0" d="M284,211.6c-6.9-4.5-16.1-2.6-20.5,4.3c-4.5,6.9-2.6,16.1,4.3,20.5c6.9,4.5,16.1,2.6,20.5-4.3
			C292.8,225.3,290.9,216.1,284,211.6C284,211.6,284,211.6,284,211.6z"/>
		<path class="st0" d="M332.4,173.7c0.4-7-4.9-12.9-11.9-13.3c-7-0.4-12.9,4.9-13.3,11.9c-0.4,7,4.9,12.9,11.9,13.3c0,0,0,0,0,0
			C326,186,332,180.6,332.4,173.7z"/>
		<path class="st0" d="M367.3,164.7c-4.5-0.3-8.4,3.2-8.6,7.7s3.2,8.4,7.7,8.6c4.5,0.3,8.3-3.2,8.6-7.7
			C375.2,168.8,371.8,165,367.3,164.7z"/>
		<path class="st0" d="M334.4,245.7c-4.8-3.1-11.2-1.8-14.4,3c-3.1,4.8-1.8,11.2,3,14.4c4.8,3.1,11.2,1.8,14.4-3
			C340.6,255.3,339.2,248.8,334.4,245.7C334.4,245.7,334.4,245.7,334.4,245.7z"/>
		<path class="st0" d="M102.6,321.9c-3.8-2.5-8.8-1.4-11.3,2.3c-2.5,3.8-1.4,8.8,2.3,11.3c3.8,2.5,8.8,1.4,11.3-2.3c0,0,0,0,0,0
			C107.5,329.5,106.4,324.4,102.6,321.9z"/>
		<path class="st0" d="M273.8,321.1c-4,2-5.6,7-3.6,11c2,4,7,5.6,11,3.6c4-2,5.6-6.9,3.6-10.9C282.8,320.7,277.9,319,273.8,321.1
			C273.9,321.1,273.8,321.1,273.8,321.1z"/>
		<path class="st0" d="M179,238.7c7.6-11.7,4.4-27.3-7.3-35c-11.7-7.6-27.3-4.4-35,7.3s-4.4,27.3,7.3,35c4.1,2.7,8.9,4.1,13.8,4.1
			C166.4,250.2,174.3,245.9,179,238.7z"/>
		<path class="st0" d="M128.5,277.4c-5.8-3.8-13.7-2.2-17.5,3.6c-3.8,5.8-2.2,13.7,3.6,17.5s13.7,2.2,17.5-3.6c0,0,0,0,0,0
			C136,289.1,134.4,281.2,128.5,277.4z"/>
		<path class="st0" d="M187.4,325.7c-5.7-0.3-10.6,4.1-11,9.8s4.1,10.6,9.8,11c5.7,0.3,10.6-4,11-9.7
			C197.5,331,193.1,326.1,187.4,325.7C187.4,325.7,187.4,325.7,187.4,325.7z"/>
		<path class="st0" d="M187.5,260.4c-8.2-0.5-15.2,5.8-15.6,14c-0.5,8.2,5.8,15.2,14,15.6c8.2,0.4,15.2-5.8,15.6-14
			C202,267.9,195.7,260.8,187.5,260.4C187.5,260.4,187.5,260.4,187.5,260.4z"/>
		<path class="st0" d="M248.2,276.4c-6.2,3.2-8.7,10.8-5.5,17c3.2,6.2,10.8,8.7,17,5.5c6.2-3.1,8.7-10.7,5.6-16.9
			C262.1,275.8,254.5,273.2,248.2,276.4C248.2,276.4,248.2,276.4,248.2,276.4z"/>
	</g>
</g>
</svg>
-}
-- <svg viewBox="0 0 22 22" fill="currentColor" xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" aria-hidden="true" focusable="false" class=""><path fill-rule="evenodd" clip-rule="evenodd" d="M0 11C0 17.0751 4.92487 22 11 22C17.0751 22 22 17.0751 22 11C22 4.92487 17.0751 0 11 0C4.92487 0 0 4.92487 0 11ZM5.01312 10.3273C4.69104 10.0051 4.68369 9.4751 4.99669 9.14354C5.29992 8.82235 5.79257 8.80521 6.11588 9.09739L6.14663 9.12663L10.1689 13.1507V6.7888C10.1689 6.31629 10.541 5.93324 11 5.93324C11.4451 5.93324 11.8085 6.29343 11.8301 6.7461L11.8311 6.7888V13.1507L15.8534 9.12663C16.1654 8.81449 16.6583 8.81184 16.9734 9.11341L17.0033 9.14354C17.3065 9.46474 17.3091 9.97215 17.0162 10.2965L16.9869 10.3273L11 16.3167L5.01312 10.3273Z" fill="black"></path></svg>
fbSvg :: forall t m. (PostBuild t m, DomBuilder t m) => m ()
fbSvg =
  elDynAttrNS (Just "xmlns=http://www.w3.org/2000/svg") "svg" (pure $ Map.fromList [("width","62px"), ("height","32px"), ("viewBox","0 0 627 260"), ("fill","none")]) $ do
    elAttr "path" (Map.fromList [("d","M193.6 105.8V135.7H177V61.3999H236.3V74.9999H193.7V92.1999H229.7V105.8H193.6Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M331.3 98.5C331.3 106.3 329.6 113.2 326.2 119C322.8 124.9 317.8 129.4 311.3 132.6C304.8 135.8 297.1 137.4 288.2 137.4C279.6 137.4 272.1 135.9 265.7 132.8C259.3 129.7 254.3 125.2 250.7 119.4C247.1 113.5 245.3 106.6 245.3 98.4C245.3 90.6 247 83.7 250.4 77.9C253.8 72 258.8 67.5 265.3 64.3C271.8 61.1 279.5 59.5 288.4 59.5C297 59.5 304.5 61 310.9 64.1C317.3 67.2 322.3 71.7 325.9 77.5C329.5 83.4 331.3 90.4 331.3 98.5ZM314 98.5C314 90.5 311.7 84.2 307.2 79.8C302.7 75.4 296.3 73.1 288.2 73.1C280.1 73.1 273.8 75.3 269.3 79.8C264.8 84.3 262.5 90.5 262.5 98.5C262.5 106.5 264.8 112.8 269.3 117.2C273.8 121.6 280.2 123.9 288.3 123.9C296.4 123.9 302.7 121.7 307.2 117.2C311.7 112.7 314 106.5 314 98.5Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M399 135.7L382.8 107.9H361V135.7H345V61.3999H381.5C402.2 61.3999 412.6 69.0999 412.6 84.5999C412.6 89.5999 411.4 93.7999 409.1 97.0999C406.8 100.4 403.2 103 398.4 104.9L417.1 135.7H399ZM380.3 95.0999C385.8 95.0999 389.8 94.2999 392.4 92.5999C395 90.8999 396.3 88.1999 396.3 84.4999C396.3 80.7999 395.1 78.1999 392.6 76.5999C390.1 74.9999 386.1 74.1999 380.5 74.1999H361V95.0999H380.3Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M502.7 110C501.5 118.8 497.6 125.6 490.8 130.4C484 135.2 475.1 137.6 464 137.6C455.7 137.6 448.4 136.1 442.2 133C436 129.9 431.1 125.5 427.7 119.6C424.3 113.7 422.6 106.7 422.6 98.5999C422.6 90.8999 424.2 84.1 427.5 78.3C430.8 72.4 435.6 67.8999 441.8 64.5999C448 61.2999 455.4 59.7 463.9 59.7C484.5 59.7 497 68.1999 501.5 85.1999L485 87.1999C483.3 82.2999 480.8 78.6999 477.5 76.5999C474.2 74.4999 469.7 73.4 464 73.4C456.6 73.4 450.7 75.6999 446.4 80.1999C442.1 84.6999 440 90.9 440 98.8C440 106.8 442.1 113 446.4 117.5C450.7 122 456.7 124.2 464.3 124.2C470.5 124.2 475.3 123.1 478.9 120.8C482.4 118.5 484.8 115 486 110.3H502.7V110Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M177 238.6V164.3H213.1C233.4 164.3 243.6 170.7 243.6 183.4C243.6 191.1 239.6 196.4 231.5 199.2C241.4 202.5 246.4 208.4 246.4 216.9C246.4 224 243.7 229.3 238.3 233C232.9 236.7 225.1 238.5 214.7 238.5H177V238.6ZM213.2 194C218.1 194 221.7 193.3 224 191.8C226.3 190.3 227.4 188.1 227.4 185C227.4 182.1 226.2 180 223.9 178.7C221.5 177.4 217.7 176.8 212.4 176.8H192.7V194H213.2ZM213.6 226.1C219.4 226.1 223.6 225.3 226.2 223.8C228.8 222.2 230.2 219.7 230.2 216.3C230.2 212.8 228.9 210.3 226.2 208.6C223.6 206.9 219.4 206.1 213.7 206.1H192.7V226.2H213.6V226.1Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M314.4 238.6L298.2 210.8H276.5V238.6H260.5V164.3H297C317.7 164.3 328.1 172 328.1 187.5C328.1 192.5 326.9 196.7 324.6 200C322.3 203.3 318.7 205.9 313.9 207.8L332.6 238.6H314.4ZM295.7 198.1C301.2 198.1 305.2 197.3 307.8 195.6C310.4 193.9 311.7 191.2 311.7 187.5C311.7 183.8 310.5 181.2 308 179.6C305.5 178 301.5 177.2 295.9 177.2H276.4V198.1H295.7Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M345.6 238.6V164.3H362.2V238.6H345.6Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M377.7 238.6V164.3H411C425.2 164.3 436.1 167.5 443.5 173.8C451 180.1 454.7 189.4 454.7 201.4C454.7 213.3 450.8 222.4 442.9 228.9C435 235.3 423.8 238.6 409.4 238.6H377.7ZM410.1 224.8C419.2 224.8 426 222.8 430.6 218.9C435.2 214.9 437.5 209.1 437.5 201.4C437.5 185.9 428.8 178.2 411.5 178.2H394.2V224.8H410.1Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M531.9 211H511.4V198.1H547.3V230.3C542.7 233.6 537.2 236.1 531 237.8C524.8 239.6 518.2 240.5 511.3 240.5C497.4 240.5 486.6 237.1 478.8 230.3C471 223.5 467.1 214 467.1 201.8C467.1 193.9 468.8 187 472.1 181.1C475.5 175.2 480.4 170.6 486.8 167.4C493.2 164.1 500.8 162.5 509.5 162.5C519.1 162.5 527 164.5 533.2 168.4C539.4 172.4 543.6 178 545.8 185.4L529.2 187.4C527.7 183.5 525.4 180.7 522.1 178.8C518.8 177 514.5 176 509.1 176C501.2 176 495.1 178.2 490.7 182.6C486.4 187 484.2 193.3 484.2 201.5C484.2 209.9 486.5 216.2 491.1 220.5C495.7 224.8 502.4 226.9 511.3 226.9C519.7 226.9 526.6 225.6 531.9 222.9V211Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M515.8 135.7V61.3999H580.4V74.9999H532.5V90.6999H580.4V104.3H532.5V122.1H580.4V135.7H515.8Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M562.4 238.6V164.3H627V177.9H579.1V193.6H627V207.2H579.1V225H627V238.6H562.4Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M0.1 68.8999L0 208.1C0 222.4 7.6 235.6 20 242.8L49.8 260L50 68.9999L0.1 68.8999Z"),("fill","#00CC9B")]) blank
    elAttr "path" (Map.fromList [("d","M21.3 62.1L0.0999756 40.4V74.4V131.9L114 66.2C123.3 60.8 129 50.9 129 40.2V0L21.3 62.1Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M15.1 138.5C5.79998 143.9 0.0999756 153.8 0.0999756 164.5V204.7L107.9 142.6L129.1 164.2V130.2V72.8L15.1 138.5Z"),("fill","black")]) blank

nervosSvg :: forall t m. (PostBuild t m, DomBuilder t m) => m ()
nervosSvg =
  elDynAttrNS (Just "xmlns=http://www.w3.org/2000/svg") "svg" (pure $ Map.fromList [("viewBox","0 0 60 61"), ("width","14px"), ("height","14px"), ("fill","currentColor"), ("aria-hidden","true"), ("focusable","false")]) $ do
    elAttr "path" (Map.fromList [("d","M0 0.0227051V60.0227H15.3559V27.3292H27.1276L0 0.0227051Z"),("fill","black")]) blank
    elAttr "path" (Map.fromList [("d","M44.6441 0.0227051V32.7165H32.8727L60 60.0227V0.0227051H44.6441Z"),("fill","black")]) blank

cardanoSvg :: forall t m. (PostBuild t m, DomBuilder t m) => m ()
cardanoSvg =
  elDynAttrNS (Just "xmlns=http://www.w3.org/2000/svg xmlns:xlink=http://www.w3.org/1999/xlink xml:space=preserve") "svg" (pure $ Map.fromList [("version","1.1"),("width", "30px"), ("height", "30px"), ("x","0px"), ("y","0px"),
	                            ("viewBox","0 0 375 346.5")]) $ do
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M102.8,172c-0.8,13.9,9.9,25.8,23.8,26.6c0.5,0,1,0,1.5,0c14,0,25.3-11.3,25.2-25.3c0-14-11.3-25.3-25.3-25.2 C114.6,148.1,103.5,158.6,102.8,172z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M8.6,165.5c-4.5-0.3-8.4,3.2-8.6,7.7s3.2,8.4,7.7,8.6c4.5,0.3,8.3-3.2,8.6-7.7 C16.6,169.6,13.1,165.8,8.6,165.5C8.6,165.5,8.6,165.5,8.6,165.5z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M101.2,25.4c4-2,5.6-7,3.6-11c-2-4-7-5.6-11-3.6c-4,2-5.6,6.9-3.6,10.9C92.2,25.8,97.1,27.5,101.2,25.4 C101.1,25.4,101.2,25.4,101.2,25.4z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M126.8,70.1c6.2-3.1,8.7-10.7,5.6-16.9s-10.7-8.7-16.9-5.6c-6.2,3.1-8.7,10.7-5.6,16.9 C113,70.7,120.6,73.2,126.8,70.1z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M40.6,100.8c4.8,3.1,11.2,1.8,14.4-3c3.1-4.8,1.8-11.2-3-14.4c-4.8-3.1-11.2-1.8-14.4,3c0,0,0,0,0,0 C34.4,91.2,35.8,97.7,40.6,100.8z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M55.9,161c-7-0.4-12.9,4.9-13.3,11.9s4.9,12.9,11.9,13.3c7,0.4,12.9-4.9,13.3-11.9c0,0,0,0,0,0 C68.2,167.4,62.9,161.4,55.9,161z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M42,245.7c-5.1,2.6-7.2,8.8-4.6,14c2.6,5.1,8.8,7.2,14,4.6c5.1-2.6,7.2-8.8,4.6-14c0,0,0,0,0,0 C53.4,245.2,47.1,243.1,42,245.7C42,245.7,42,245.7,42,245.7z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M91,134.9c6.9,4.5,16.1,2.6,20.5-4.3c4.5-6.9,2.6-16.1-4.3-20.5c-6.9-4.5-16.1-2.6-20.5,4.3 C82.2,121.2,84.1,130.4,91,134.9C91,134.9,91,134.9,91,134.9z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M246.5,69.1c5.8,3.8,13.7,2.2,17.5-3.6s2.2-13.7-3.6-17.5c-5.8-3.8-13.7-2.2-17.5,3.6c0,0,0,0,0,0 C239,57.5,240.6,65.3,246.5,69.1C246.5,69.1,246.5,69.1,246.5,69.1z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M272.3,24.6c3.8,2.5,8.8,1.4,11.3-2.4s1.4-8.8-2.4-11.3c-3.8-2.5-8.8-1.4-11.3,2.3 C267.5,17,268.6,22.1,272.3,24.6C272.3,24.6,272.3,24.6,272.3,24.6z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M248.4,147.9c-13.9-0.8-25.9,9.9-26.6,23.8c-0.8,13.9,9.9,25.9,23.8,26.6c0.5,0,1,0,1.4,0 c13.9,0,25.2-11.3,25.2-25.3C272.3,159.7,261.8,148.6,248.4,147.9L248.4,147.9z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M135.1,133.1c4.3,8.5,13,13.9,22.6,13.9c13.9,0,25.2-11.3,25.2-25.3c0-3.9-0.9-7.8-2.7-11.4 c-6.3-12.5-21.5-17.5-33.9-11.2C133.8,105.5,128.8,120.7,135.1,133.1L135.1,133.1z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M333,100.8c5.1-2.6,7.1-8.9,4.5-14c-2.6-5.1-8.9-7.1-14-4.5c-5.1,2.6-7.1,8.8-4.6,13.9 C321.6,101.3,327.8,103.4,333,100.8C333,100.8,333,100.8,333,100.8z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M269,108.8c-7.3,3.7-10.3,12.6-6.6,19.9c3.7,7.3,12.6,10.3,19.9,6.6c7.3-3.7,10.3-12.6,6.6-19.9 C285.2,108.1,276.3,105.2,269,108.8z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M186.5,20.8c5.7,0.3,10.6-4.1,11-9.8s-4.1-10.6-9.8-11c-5.7-0.3-10.6,4-11,9.7 C176.4,15.5,180.8,20.4,186.5,20.8C186.5,20.8,186.5,20.8,186.5,20.8z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M186.4,86.1c8.2,0.5,15.2-5.8,15.6-14c0.5-8.2-5.8-15.2-14-15.6c-8.2-0.5-15.2,5.8-15.6,14 C172,78.7,178.2,85.7,186.4,86.1C186.4,86.1,186.4,86.1,186.4,86.1z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M106,237.7c7.3-3.7,10.3-12.6,6.6-19.9c-3.7-7.3-12.6-10.3-19.9-6.6c-7.3,3.7-10.3,12.6-6.6,19.9 C89.8,238.4,98.7,241.4,106,237.7z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M196,107.8c-7.6,11.7-4.4,27.3,7.3,34.9c11.7,7.6,27.3,4.4,34.9-7.3c7.6-11.7,4.4-27.3-7.3-34.9 c-4.1-2.7-8.9-4.1-13.8-4.1C208.6,96.4,200.7,100.7,196,107.8z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M239.9,213.4c-6.3-12.5-21.5-17.5-33.9-11.2c-12.5,6.3-17.5,21.5-11.2,33.9c6.3,12.5,21.5,17.5,33.9,11.2 c0,0,0,0,0,0c12.4-6.2,17.5-21.2,11.3-33.7C240,213.5,240,213.5,239.9,213.4z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M284,211.6c-6.9-4.5-16.1-2.6-20.5,4.3c-4.5,6.9-2.6,16.1,4.3,20.5c6.9,4.5,16.1,2.6,20.5-4.3 C292.8,225.3,290.9,216.1,284,211.6C284,211.6,284,211.6,284,211.6z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M332.4,173.7c0.4-7-4.9-12.9-11.9-13.3c-7-0.4-12.9,4.9-13.3,11.9c-0.4,7,4.9,12.9,11.9,13.3c0,0,0,0,0,0 C326,186,332,180.6,332.4,173.7z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M367.3,164.7c-4.5-0.3-8.4,3.2-8.6,7.7s3.2,8.4,7.7,8.6c4.5,0.3,8.3-3.2,8.6-7.7 C375.2,168.8,371.8,165,367.3,164.7z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M334.4,245.7c-4.8-3.1-11.2-1.8-14.4,3c-3.1,4.8-1.8,11.2,3,14.4c4.8,3.1,11.2,1.8,14.4-3 C340.6,255.3,339.2,248.8,334.4,245.7C334.4,245.7,334.4,245.7,334.4,245.7z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M102.6,321.9c-3.8-2.5-8.8-1.4-11.3,2.3c-2.5,3.8-1.4,8.8,2.3,11.3c3.8,2.5,8.8,1.4,11.3-2.3c0,0,0,0,0,0 C107.5,329.5,106.4,324.4,102.6,321.9z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M273.8,321.1c-4,2-5.6,7-3.6,11c2,4,7,5.6,11,3.6c4-2,5.6-6.9,3.6-10.9C282.8,320.7,277.9,319,273.8,321.1 C273.9,321.1,273.8,321.1,273.8,321.1z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M179,238.7c7.6-11.7,4.4-27.3-7.3-35c-11.7-7.6-27.3-4.4-35,7.3s-4.4,27.3,7.3,35c4.1,2.7,8.9,4.1,13.8,4.1 C166.4,250.2,174.3,245.9,179,238.7z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M128.5,277.4c-5.8-3.8-13.7-2.2-17.5,3.6c-3.8,5.8-2.2,13.7,3.6,17.5s13.7,2.2,17.5-3.6c0,0,0,0,0,0 C136,289.1,134.4,281.2,128.5,277.4z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M187.4,325.7c-5.7-0.3-10.6,4.1-11,9.8s4.1,10.6,9.8,11c5.7,0.3,10.6-4,11-9.7 C197.5,331,193.1,326.1,187.4,325.7C187.4,325.7,187.4,325.7,187.4,325.7z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M187.5,260.4c-8.2-0.5-15.2,5.8-15.6,14c-0.5,8.2,5.8,15.2,14,15.6c8.2,0.4,15.2-5.8,15.6-14 C202,267.9,195.7,260.8,187.5,260.4C187.5,260.4,187.5,260.4,187.5,260.4z")]) blank
		elAttr "path" (Map.fromList [("fill","#0033AD"), ("d","M248.2,276.4c-6.2,3.2-8.7,10.8-5.5,17c3.2,6.2,10.8,8.7,17,5.5c6.2-3.1,8.7-10.7,5.6-16.9 C262.1,275.8,254.5,273.2,248.2,276.4C248.2,276.4,248.2,276.4,248.2,276.4z")]) blank

arrowSvg :: forall t m. (PostBuild t m, DomBuilder t m) => m ()
arrowSvg = elDynAttrNS (Just "xmlns=http://www.w3.org/2000/svg") "svg" (pure $ Map.fromList [("viewBox","0 0 22 22"), ("fill","currentColor"), ("width","1em"), ("height","1em"), ("aria-hidden","true"), ("focusable","false")]) $ do
  elAttr "path" (Map.fromList [("fill-rule","evenodd"), ("clip-rule","evenodd"), ("d","M0 11C0 17.0751 4.92487 22 11 22C17.0751 22 22 17.0751 22 11C22 4.92487 17.0751 0 11 0C4.92487 0 0 4.92487 0 11ZM5.01312 10.3273C4.69104 10.0051 4.68369 9.4751 4.99669 9.14354C5.29992 8.82235 5.79257 8.80521 6.11588 9.09739L6.14663 9.12663L10.1689 13.1507V6.7888C10.1689 6.31629 10.541 5.93324 11 5.93324C11.4451 5.93324 11.8085 6.29343 11.8301 6.7461L11.8311 6.7888V13.1507L15.8534 9.12663C16.1654 8.81449 16.6583 8.81184 16.9734 9.11341L17.0033 9.14354C17.3065 9.46474 17.3091 9.97215 17.0162 10.2965L16.9869 10.3273L11 16.3167L5.01312 10.3273Z"),("fill","black")]) blank

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

  elClass "div" "border rounded-lg p-4 mb-4" $ do
    elClass "div" "flex flex-row items-center justify-between mb-2" $ do
      elClass "div" "flex flex-row items-center" $ do
        elClass "div" "mr-2 font-bold" $ text $ outChain direction
        elClass "div" "select-none bg-gradient-to-r from-secondary to-secondary-end px-2 py-1 rounded-lg text-sm font-bold drop-shadow-md" $ do
          el "span" cardanoSvg
          text "Ada | ada"

      -- TODO(skylar): Show the fee in proper encoding
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

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Force Bridge"
      elAttr "link" ("href" =: $(static "favicon.ico") <> "rel" =: "icon") blank
      elAttr "link" ("href" =: $(static "css/output.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src" =: $(static "js/app.bundle.js")) blank
  , _frontend_body = elClass "div" "flex flex-col w-screen h-screen text-gray-600" $ do

      -- address <- Nami.currentAddress
      rec
        currentDirection <- foldDyn ($) BridgeIn changeEvent

        changeEvent <- elClass "div" "bg-white w-full p-4 drop-shadow-md items-center flex flex-row justify-between" $ do
          el "div" $ fbSvg

          (buttonEl, _) <- elClass' "button" "font-bold rounded-md bg-gradient-to-r from-secondary to-secondary-end text-black px-4 py-2 drop-shadow-md" $ do
            dynText $ pretty <$> currentDirection

          el "div" nervosSvg
          el "div" cardanoSvg
          el "div" arrowSvg
          elClass "div" "" $ el "div" $ text "Menu"

          pure $ changeBridgeDirection <$ domEvent Click buttonEl

      -- TODO(skylar): Do we care about the UI appearing properly in a prerender?
      elClass "div" "flex bg-gradient-to-br from-tertiary to-tertiary-end flex-grow justify-center items-center pt-36 pb-8" $ prerender_ blank $ do
        el "div" arrowSvg
        {-
        eApi <- liftJSM $ Nami.getApi
        case eApi of
          Right api -> do
            elClass "div" "rounded-2xl w-90 bg-white p-6 shadow-md" $ do
              addr <- liftJSM $ Nami.getUsedAddress api
              -- case addr of
              -- Just a -> Nami.pay api a Nami.deepakBech32 1
              -- Nothing -> pure ()
              balance <- liftJSM $ Nami.getBalance api
              elClass "div" "group relative select-none bg-gradient-to-r from-secondary to-secondary-end px-4 py-2 \
                            \ rounded-lg mb-4 drop-shadow-md text-black text-center font-bold" $ case addr of
                Nothing -> text "Loading wallet"
                Just result -> do
                  elClass "div" "absolute left-0 rounded-lg bg-black text-white bg-opacity-75 bottom-full px-4 py-4 select-all \
                                \ selection:bg-secondary selection:text-black break-all invisible group-hover:visible" $ text result
                  elClass "div" "" $ text $ truncateMiddleText result truncateLength

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
                                    , bool "cursor-not-allowed bg-white" "bg-primary drop-shadow-lg text-white border-transparent" b
                                    ]

              (submitButton, _) <- elDynClass' "button" (mkBridgeButtonClasses . checkAmount balance <$> amount) $ do
                dynText $ ffor (checkAmount balance <$> amount) $ \case
                  True -> "Bridge"
                  False -> "Enter an amount"

              let
                inTx = liftA2 BridgeInTx <$> amount <*> (pure <$> ckbAddress)

              performEvent_ $ maybe (pure ()) (\a -> Nami.pay api a Nami.deepakBech32 1) addr <$ (gate (current $ isJust <$> amount) $ domEvent Click submitButton)
              -- performEvent_ $ maybe (pure ()) (Nami.signTest api) addr <$ (gate (current $ isJust <$> amount) $ domEvent Click submitButton)
          _ -> do
            text "You require nami wallet"
-}
      pure ()
  }

fee :: Double
fee = 0.001

-- TODO(skylar): This isn't correct, where is the fee
checkAmount :: Double -> Maybe Double -> Bool
checkAmount balance (Just amount)
  | balance > amount && amount > 0  = True
checkAmount _ _ = False
