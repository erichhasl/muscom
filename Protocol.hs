module Protocol where

import Prelude hiding (lookup)

import Shared (Handler, LoginHandler)
import Data.Aeson
import Database.MySQL.Simple
import Data.Map
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Concurrent.Chan
import Text.Read
import Database
import System.IO

-- | parse json message into action and value, fail on protocol
-- mismatch
handle :: Connection -> Handler
handle dbconn hdl chan addr msg = runMaybeT ( do
    parsed <- MaybeT (return (decode (BS.pack msg) :: Maybe (Map String String)))
    action <- MaybeT $ return $ lookup "action" parsed
    value <- MaybeT $ return $ (lookup "value" parsed >>= readMaybe :: Maybe Int)
    execute action value ) >> return ()
  where execute :: String -> Int -> MaybeT IO ()
        execute "upvote" value = do
            liftIO $ do votefor dbconn addr value
                        sendSongs dbconn chan
                        putStrLn $ "--> upvote for " ++ show value
        execute "volumeadjust" value = do
            liftIO $ putStrLn $ "adjusting volume to " ++ show value

sendSongs :: Connection -> Chan String -> IO ()
sendSongs dbconn chan = do
    songs <- selectSongs dbconn
    let msg = BS.unpack $ encode songs
    broadcast chan msg

broadcast :: Chan String -> String -> IO ()
broadcast = writeChan

login :: Connection -> LoginHandler
login dbconn hdl addr = do
    songs <- selectSongs dbconn
    let msg = BS.unpack $ encode songs
    hPutStrLn hdl msg
