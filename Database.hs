{-# LANGUAGE OverloadedStrings #-}

module Database where

import Data.Aeson
import Data.String
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Network.Socket (SockAddr(..))
import Data.Monoid ((<>))
import Control.Monad (when)
import System.Process
import Control.Concurrent (Chan, writeChan)

import Data.Int

type SqlQuery a = Connection -> IO a
type SqlCommand = Connection -> IO Int64

data Song = Song { songid :: Int
                 , title :: String
                 , path :: FilePath
                 , votes :: Int }
            deriving (Show, Eq)

newtype SongData = SongData [Song]

instance Ord Song where
    compare a b = compare (votes a) (votes b)

instance ToJSON SongData where
    toJSON (SongData songs) =
        object [ "action" .= String "receive_updates"
               , "data" .= toJSON songs ]

    toEncoding (SongData songs) =
        pairs ("action" .= String "receive_updates" <> "data" .= toJSON songs)

instance ToJSON Song where
    toJSON (Song songid title _ votes) =
        object [ "songid" .= show songid
               , "title" .= title
               , "votes" .= votes ]

    toEncoding (Song songid title _ votes) =
        pairs ("songid" .= show songid <> "title" .= title <> "votes" .= votes)

data User = User { uid :: Int
                 , addr :: String
                 , votedFor :: Int }
            deriving (Show, Eq)

instance QueryResults Song where
    convertResults [fa, fb, fc, fd] [va, vb, vc, vd] =
            Song songid title path votes
        where songid = convert fa va
              title = convert fb vb
              path = convert fc vc
              votes = convert fd vd
    convertResults fs vs = convertError fs vs 2

instance QueryResults User where
    convertResults [fa, fb, fc] [va, vb, vc] =
            User uid addr votedFor
        where uid = convert fa va
              addr = convert fb vb
              votedFor = convert fc vc
    convertResults fs vs = convertError fs vs 2

selectSongs :: Connection -> IO [Song]
selectSongs = (flip query_) "select songid, title, path, votes from songs order by votes desc"

selectUsers :: Connection -> SockAddr -> IO [User]
selectUsers dbconn addr = query dbconn "select uid, ip, songid from users where ip=?" [show addr]

votefor :: Connection -> SockAddr -> Int -> IO ()
votefor dbconn addr songid = do
    users <- selectUsers dbconn addr
    case users of
       [User { votedFor = a }] -> do
           if (getIP addr) `elem` ["10.42.23.66", "10.42.25.39"]
            then return ()
            else downvote dbconn a >> return ()
       _ -> return ()
    num <- upvote dbconn songid
    when (num > 0) $ updateUser dbconn addr songid >> return ()
    return ()

searchSong :: Connection -> String -> IO (Maybe Int)
searchSong dbconn text = do
    let q :: Query
        q = fromString ("select songid, title, path, votes from songs where title like \"%" ++ text ++ "%\"")
    songs <- query_ dbconn q
    case songs of
        [song] -> return $ Just $ songid song
        _ -> return Nothing

getIP :: SockAddr -> String
getIP (SockAddrInet port addr) = show addr
getIP (SockAddrInet6 _ _ addr _) = show addr
getIP (SockAddrUnix addr) = addr
getIP (SockAddrCan n) = show n

upvote :: Connection -> Int -> IO Int64
upvote conn songid = execute conn "update songs set votes = votes + 1 where songid=?" [songid]

downvote :: Connection -> Int -> IO Int64
downvote conn songid = execute conn "update songs set votes = votes - 1 where songid=?" [songid]

updateUser :: Connection -> SockAddr -> Int -> IO Int64
updateUser dbconn addr songid =
    execute dbconn "insert into users (ip, songid) values (?, ?) on duplicate key update songid = ?" (show addr, songid, songid)

resetVotes :: Connection -> IO Int64
resetVotes dbconn = execute_ dbconn "update songs set votes = 0"

clearUsers :: Connection -> IO Int64
clearUsers dbconn = execute_ dbconn "truncate table users"

addVolume :: Connection -> SockAddr -> Int -> IO Int64
addVolume dbconn addr volume = execute dbconn "insert into volumes (volume, ip) values (?, ?) on duplicate key update volume = ?" (volume, show addr, volume)

resetVolume :: Connection -> Chan String -> IO Int64
resetVolume dbconn chan = do
    updateVolume dbconn chan
    execute_ dbconn "truncate table volumes"

updateVolume :: Connection -> Chan String -> IO ()
updateVolume dbconn chan = do
    res <- query_ dbconn "select avg(volume) from volumes" :: IO [Only (Maybe Double)]
    new <- case res of
        [Only (Just x)] -> callCommand ("amixer set Master " ++ show x ++ "%") >> return x
        _               -> callCommand "amixer set Master 66%" >> return 66
    putStrLn $ "new volume " ++ show new
    sendCurrentVolume chan new

sendCurrentVolume :: Chan String -> Double -> IO ()
sendCurrentVolume chan vol = do
    let dat = [ ("action", "current_volume")
              , ("data", show vol) ] :: [(String, String)]
        msg = BS.unpack $ encode (M.fromList dat)
    writeChan chan msg

connectInfo :: ConnectInfo
connectInfo = ConnectInfo
    { connectHost = "localhost"
    , connectPort = 3306
    , connectUser = "muscom"
    , connectPassword = "muscom00muscom"
    , connectDatabase = "muscom"
    , connectOptions = []
    , connectPath = ""
    , connectSSL = Nothing }

dbConnect :: IO Connection
dbConnect = connect connectInfo

test :: IO ()
test = do
    conn <- dbConnect
    songs <- selectSongs conn
    putStrLn $ show songs
    upvote conn 2
    upvote conn 2
    downvote conn 1
    songs <- selectSongs conn
    putStrLn $ "Best song is " ++ show (maximum songs)
