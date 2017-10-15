{-# LANGUAGE OverloadedStrings #-}

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
import Text.Read (readMaybe)
import Data.Text
import Database
import System.IO
import qualified Data.Map as M

-- | parse json message into action and value, fail on protocol
-- mismatch
handle :: Connection -> Handler
handle dbconn hdl chan addr msg = runMaybeT ( do
    parsed <- MaybeT (return (decode (BS.pack msg) :: Maybe (Map String String)))
    action <- MaybeT $ return $ lookup "action" parsed
    value <- MaybeT $ return $ lookup "value" parsed
    execute action value ) >> return ()
  where execute :: String -> String -> MaybeT IO ()
        execute "upvote" value = do
            let songid = readMaybe value :: Maybe Int
            case songid of
                Just num -> liftIO $ do
                    votefor dbconn addr num
                    sendSongs dbconn chan
                    putStrLn $ "--> upvote for " ++ show value
                Nothing -> liftIO $ do
                    searched <- searchSong dbconn value
                    case searched of
                        Just found -> liftIO $ do
                            votefor dbconn addr found
                            sendSongs dbconn chan
                            putStrLn $ "--> upvote for " ++ show value
                        Nothing -> return ()
        execute "volume_adjust" value = do
            let volume = readMaybe value :: Maybe Int
            case volume of
                Just vol -> liftIO $ do addVolume dbconn addr (max 0 (min 100 vol))
                                        updateVolume dbconn chan
                Nothing -> return ()
        execute _ _ = return ()
                         

sendSongs :: Connection -> Chan String -> IO ()
sendSongs dbconn chan = do
    songs <- selectSongs dbconn
    let msg = BS.unpack $ encode (SongData songs)
    broadcast chan msg

sendCurrentlyPlaying :: Chan String -> String -> IO ()
sendCurrentlyPlaying chan title = do
    let dat = [ ("action", "currently_playing")
              , ("data", title) ] :: [(String, String)]
        msg = BS.unpack $ encode (M.fromList dat)
    broadcast chan msg

broadcast :: Chan String -> String -> IO ()
broadcast = writeChan

login :: Connection -> LoginHandler
login dbconn hdl addr = do
    songs <- selectSongs dbconn
    let msg = BS.unpack $ encode (SongData songs)
    hPutStrLn hdl msg
