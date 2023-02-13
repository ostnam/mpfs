{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data where

import Network.Wreq ( get, responseBody )
import Control.Lens ( (^.) )
import Text.RawString.QQ ( r )
import Control.Concurrent ( threadDelay )
import Network.HTTP.Client ( HttpException )
import Control.Exception.Base ( catch )
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import qualified Types

--- DB setup
setUpDb :: DB.Connection -> IO ()
setUpDb conn = do
  DB.execute_ conn
    [r|CREATE TABLE IF NOT EXISTS entries (
      title     text NOT NULL,
      link      text NOT NULL,
      published text NOT NULL,
      seen      int  NOT NULL,
      feed      text NOT NULL,
      CONSTRAINT uniqs UNIQUE (title, link, feed)
    );|]

  DB.execute_ conn
    [r|CREATE TABLE IF NOT EXISTS feeds (
      name text NOT NULL,
      url  text PRIMARY KEY
    );|]


--- DB queries
-- | Returns the unseen items from the specified feed.
getUnseenItems :: DB.Connection -> Types.Feed -> IO [Types.FeedItem]
getUnseenItems conn feed = DB.query conn "SELECT * FROM entries WHERE feed = ? and seen = 0;" $ DB.Only feed.url

-- | Saves the item to the database.
saveItem :: DB.Connection -> Types.FeedItem -> IO ()
saveItem conn = DB.execute conn "INSERT OR IGNORE INTO entries VALUES (?, ?, ?, ?, ?);"

-- | Lists every feed in the database.
getFeeds :: DB.Connection -> IO [Types.Feed]
getFeeds conn = DB.query_ conn "SELECT * FROM feeds;"

-- | Registers a feed into the database.
addFeed :: DB.Connection -> Types.Feed -> IO ()
addFeed conn = DB.execute conn "INSERT OR IGNORE INTO feeds VALUES (?, ?);"

-- | Deletes a feed from the database.
deleteFeed :: DB.Connection -> Types.Feed -> IO ()
deleteFeed conn = DB.execute conn "DELETE FROM feeds WHERE name = ? AND url = ?;"

-- | Marks an entry as seen.
markSeen :: DB.Connection -> Types.FeedItem -> IO ()
markSeen conn item = DB.execute conn "UPDATE entries SET seen = 1 WHERE link = ?;" $ DB.Only item.link

--- Updating feeds
-- | Fetch every item from the feed.
refreshFeed :: Types.Feed -> IO [Types.FeedItem]
refreshFeed f = do
  resp <- get $ T.unpack f.url
  mapM fillDefaults <$> Types.parseFeed $ resp ^. responseBody
    where fillDefaults = Types.toFeedItem "No title" "missing" f.url

-- | Handling exceptions
-- Currently ignores the exception, returning an empty list.
httpExceptionHandler :: HttpException -> IO [Types.FeedItem]
httpExceptionHandler _ = pure []

-- | refreshFeed, but handles HTTP exceptions
refreshFeedChecked :: Types.Feed -> IO [Types.FeedItem]
refreshFeedChecked f = catch (refreshFeed f) httpExceptionHandler

-- | Gets every subscribed feed from the database, then fetches their
-- | items.
refreshEveryFeed :: DB.Connection -> IO ()
refreshEveryFeed conn = do
  feeds <- getFeeds conn
  items <- concat <$> mapM refreshFeedChecked feeds
  mapM_ (saveItem conn) items

-- | Refreshes every feed from the database, every 10 minutes.
refreshLoop :: DB.Connection -> IO ()
refreshLoop conn = do
  refreshEveryFeed conn
  threadDelay $ 1000000 * 60 * 10
  refreshLoop conn
