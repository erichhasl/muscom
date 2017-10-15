{-# LANGUAGE OverloadedStrings #-}

module Database where

import Data.Aeson
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Network.Socket (SockAddr)
import Data.Monoid ((<>))
import Control.Monad (when)

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
    toJSON songs =
        object [ "action" .= String "receive_updates"
               , "data" .= toJSON songs ]

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
       [User { votedFor = a }] -> downvote dbconn a >> return ()
       _ -> return ()
    num <- upvote dbconn songid
    when (num > 0) $ updateUser dbconn addr songid >> return ()
    return ()

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
