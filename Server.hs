module Server where

import System.Process
import Database.MySQL.Simple
import Database
import Audio
import Control.Concurrent

runMusic :: Connection -> IO () -> IO ()
runMusic conn onReset = do
    songs <- selectSongs conn
    let bestOne = maximum songs
        fp = "music/" ++ path bestOne
    forkIO $ playAudio fp
    resetVotes conn
    clearUsers conn
    onReset
    getLine
    runMusic conn onReset
