import Socket
import Protocol
import Database
import System.IO
import Server
import Control.Concurrent

main :: IO ()
main = do
    conn <- dbConnect
    comm <- initCommunicator 54345 (handle conn) (login conn)
    forkIO $ runCommunicator comm
    runMusic conn (sendSongs conn $ commChan comm)
