{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data where

import Network.Wreq ( get, responseBody )
import Control.Lens
import Text.RawString.QQ
import Control.Concurrent ( threadDelay )
import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import qualified Types


refreshFeed :: Types.Feed -> IO [Types.FeedItem]
refreshFeed f = do
  resp <- get $ T.unpack f.url
  return $ Types.parseFeed f.url $ resp ^. responseBody


setUpDb :: DB.Connection -> IO ()
setUpDb conn = do
  DB.execute_ conn
    [r|CREATE TABLE IF NOT EXISTS entries (
      title text,
      link text,
      published text,
      seen int,
      feed text,
      CONSTRAINT uniqs UNIQUE (title, link, feed)
    );|]

  DB.execute_ conn
    [r|CREATE TABLE IF NOT EXISTS feeds (
      name text NOT NULL,
      url text PRIMARY KEY
    );|]


getItems :: DB.Connection -> Types.Feed -> IO [Types.FeedItem]
getItems conn feed = DB.query conn "SELECT * FROM entries WHERE feed = ?;" $ DB.Only feed.url

saveItem :: DB.Connection -> Types.FeedItem -> IO ()
saveItem conn = DB.execute conn "INSERT INTO entries VALUES (?, ?, ?, ?, ?);"

getFeeds :: DB.Connection -> IO [Types.Feed]
getFeeds conn = DB.query_ conn "SELECT * FROM feeds;"

addFeed :: DB.Connection -> Types.Feed -> IO ()
addFeed conn = DB.execute conn "INSERT INTO feeds VALUES (?, ?);"

deleteFeed :: DB.Connection -> Types.Feed -> IO ()
deleteFeed conn = DB.execute conn "DELETE FROM feeds WHERE name = ? AND url = ?;"

markSeen :: DB.Connection -> Types.FeedItem -> IO ()
markSeen conn = DB.execute conn "UPDATE entries SET seen = 1 WHERE link = ?"

refreshEveryFeed :: DB.Connection -> IO ()
refreshEveryFeed conn = do
  feeds <- getFeeds conn
  items <- liftM concat $ mapM refreshFeed $ feeds
  mapM (saveItem conn) items
  return ()

refreshLoop :: DB.Connection -> IO ()
refreshLoop conn = do
  refreshEveryFeed conn
  threadDelay $ 1000000 * 60 * 10
  refreshLoop conn
