module Server where

import System.Process
import Database.MySQL.Simple
import Database
import Audio
import Control.Concurrent (Chan)

runMusic :: Connection -> Chan String -> (String -> IO ()) -> IO ()
runMusic conn chan onReset = do
    songs <- selectSongs conn
    let bestOne = maximum songs
        fp = "music/" ++ path bestOne
    procHandle <- playAudio fp
    resetVotes conn
    clearUsers conn
    resetVolume conn chan
    onReset (title bestOne)
    getLine
    terminateProcess procHandle
    runMusic conn chan onReset
