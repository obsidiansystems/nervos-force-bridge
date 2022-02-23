-- | 

module Backend.Utils where

import System.Process

inDirectory :: FilePath -> CreateProcess -> CreateProcess
inDirectory fp cp = cp { cwd = Just fp }
