{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Backend.Utils where

import System.Process
import Control.Monad.Log
import Control.Monad.IO.Class
import qualified Data.Text as T

inDirectory :: FilePath -> CreateProcess -> CreateProcess
inDirectory fp cp = cp { cwd = Just fp }

miliseconds :: Int -> Int
miliseconds = (1000*)

seconds :: Int -> Int
seconds = (1000000*)

tShow :: Show a => a -> T.Text
tShow = T.pack . show

-- TODO(skylar): Move this
type ForceM m = (MonadLog (WithSeverity T.Text) m, MonadIO m)
