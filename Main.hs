import Socket
import Protocol
import Database
import System.IO
import Server
import Control.Concurrent

onReset conn chan newSong = do
    sendCurrentlyPlaying chan newSong
    sendSongs conn chan

main :: IO ()
main = do
    conn <- dbConnect
    comm <- initCommunicator 54345 (handle conn) (login conn)
    forkIO $ runCommunicator comm
    runMusic conn (commChan comm) (onReset conn $ commChan comm)
