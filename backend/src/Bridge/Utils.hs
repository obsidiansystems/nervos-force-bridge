{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 

module Bridge.Utils ( module E
                    , BridgeM
                    , runBridge
                    , scrubPrefix
                    ) where

import Control.Monad.Log as E
import Control.Monad.IO.Class as E
import qualified Data.Text as T

import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)

import Data.Aeson

scrubPrefix :: String -> Options
scrubPrefix s =
  defaultOptions { fieldLabelModifier = drop (length s)
                 }

type BridgeM m = (MonadLog (WithSeverity T.Text) m, MonadIO m)

runBridge :: LoggingT (WithSeverity T.Text) IO a -> IO a
runBridge = flip runLoggingT (putDoc . renderWithSeverity pretty)
