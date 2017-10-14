module Audio where

import System.Process

playAudio :: FilePath -> IO ProcessHandle
playAudio path = spawnProcess "./audiolib" [path]
