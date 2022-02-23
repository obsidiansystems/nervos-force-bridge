-- | 

module CKB.Capsule where

import CKB.Types
import System.Process

import Backend.Utils

-- TODO(skylar): Can we statically do stuff related to capsule projects?
-- TODO(sklyar): Potentially
-- TODO(skylar): Isomorphic bool for releasev vs debug?
buildProject :: FilePath -> IO ()
buildProject cfp = do
  _ <- createProcess $ inDirectory cfp $ proc capsulePath ["build", "--release"]
  pure ()

deployProject :: FilePath -> IO ()
deployProject cfp = do
  _ <- createProcess $ inDirectory cfp $ proc capsulePath ["deploy", "--address", "ckt1qyq075y5ctzlgahu8pgsqxrqnglajgwa9zksmqdupd"]
  pure ()
