module Server where

import System.Process
import Database.MySQL.Simple
import Database
import Audio

runMusic :: Connection -> IO () -> IO ()
runMusic conn onReset = do
    songs <- selectSongs conn
    let bestOne = maximum songs
        fp = "music/" ++ path bestOne
    procHandle <- playAudio fp
    resetVotes conn
    clearUsers conn
    onReset
    getLine
    terminateProcess procHandle
    runMusic conn onReset
