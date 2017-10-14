module Socket where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forever)
import Shared (Handler, LoginHandler)

import Protocol (sendSongs)

type Msg = String

data Communicator = Communicator
    { commSocket :: Socket
    , onReceive :: Handler 
    , onLogin :: LoginHandler
    , commChan :: Chan Msg }

-- | Init a communicator instance
initCommunicator :: PortNumber
                 -> Handler
                 -> LoginHandler
                 -> IO Communicator
initCommunicator port handler loginHandler = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 5
    chan <- newChan
    forkIO $ forever $ do
        msg <- readChan chan
        return ()
    return (Communicator sock handler loginHandler chan)

-- | Looping over requests and establish connection
procRequests :: Communicator -> IO ()
procRequests comm = do
    (conn, clientaddr) <- accept (commSocket comm)
    forkIO $ procMessages conn clientaddr
        (commChan comm) (onReceive comm) (onLogin comm)
    procRequests comm

procMessages :: Socket
             -> SockAddr
             -> Chan Msg
             -> Handler
             -> LoginHandler
             -> IO ()
procMessages conn clientaddr chan handler loginHandler = do
    connhdl <- socketToHandle conn ReadWriteMode
    hSetBuffering connhdl NoBuffering
    commChan <- dupChan chan

    reader <- forkIO $ forever $ do
        msg <- readChan commChan
        hPutStrLn connhdl msg

    loginHandler connhdl clientaddr

    messages <- hGetContents connhdl
    mapM_ (handler connhdl chan clientaddr) (lines messages)
    killThread reader
    hClose connhdl
    
runServer :: PortNumber -> Handler -> LoginHandler -> IO ()
runServer port hdler loginHdler = initCommunicator port hdler loginHdler >>= procRequests

runCommunicator :: Communicator -> IO ()
runCommunicator = procRequests
