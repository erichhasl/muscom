module Shared where

import Network.Socket
import System.IO
import Control.Concurrent.Chan
import Database.MySQL.Simple

type Handler = Handle -> Chan String -> SockAddr -> String -> IO ()

type LoginHandler = Handle -> SockAddr -> IO ()
