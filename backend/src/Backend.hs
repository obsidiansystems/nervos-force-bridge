{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Backend for force-bridge
-}

module Backend where

import Common.Route
import Obelisk.Backend

-- | Backend definition for force-bridge
backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
