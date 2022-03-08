{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Promise ( Promise
               , JSVal
               , unsafeToPromise
               , handlePromise
               , promiseMaybe
               ) where

import Control.Lens
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle ( JSVal
                                   , MakeObject
                                   , JSM
                                   , js1
                                   , fun
                                   )

newtype Promise =
  UnsafeToPromise JSVal
  deriving (MakeObject)

unsafeToPromise :: JSVal -> Promise
unsafeToPromise = UnsafeToPromise

handlePromise :: Promise -> (JSVal -> JSM b) -> (JSVal -> JSM a) -> JSM (Either a b)
handlePromise (UnsafeToPromise val) thenHandler catchHandler = do
  mvar <- liftIO $ newEmptyMVar
  nextVal <- val ^. js1 "then" (fun $ \_ _ [v] -> thenHandler v >>= liftIO . putMVar mvar . Right)
  _ <- nextVal ^. js1 "catch" (fun $ \_ _ [err] -> catchHandler err >>= liftIO . putMVar mvar . Left)
  liftIO $ takeMVar mvar

promiseMaybe :: Promise -> (JSVal -> JSM a) -> JSM (Maybe a)
promiseMaybe p thenHandler = do
  eitherToMaybe <$> handlePromise p thenHandler (\_ -> pure ())

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _ = Nothing
