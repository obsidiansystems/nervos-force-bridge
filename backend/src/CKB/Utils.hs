-- |

module CKB.Utils where

import Data.Aeson
import Data.Aeson.TH

scrubPrefix :: String -> Options
scrubPrefix s =
  defaultOptions { fieldLabelModifier = drop (length s)
                 }
