{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NumericUnderscores #-}
module Data where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar ( MVar, takeMVar )
import Control.Exception.Base ( catch )
import Data.Foldable (for_, traverse_)
import System.Timeout ( timeout )

import Control.Lens ( (^.) )
import qualified Data.Text as T
import qualified Database.SQLite.Simple as DB
import Network.HTTP.Client ( HttpException )
import Network.Wreq ( get, responseBody )
import Text.RawString.QQ ( r )

import qualified Types

--- DB setup
setUpDb :: DB.Connection -> IO ()
setUpDb conn = do
  DB.execute_ conn
    [r|
      CREATE TABLE IF NOT EXISTS entries (
        title     text NOT NULL,
        link      text NOT NULL,
        published text NOT NULL,
        seen      int  NOT NULL,
        feed      text NOT NULL,
        CONSTRAINT uniqs UNIQUE (link, feed)
     );
    |]

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

-- | Only keep 1000 latest entries for every subscribed feed
cleanupDbEntries :: DB.Connection -> IO ()
cleanupDbEntries conn = do
  feeds <- getFeeds conn
  traverse_ (cleanUpFeed conn) feeds

-- | Only keep 1000 latest entries for the given feed
cleanUpFeed :: DB.Connection -> Types.Feed -> IO ()
cleanUpFeed conn feed = DB.execute conn
  [r|
    DELETE FROM entries
    WHERE feed = ? AND link NOT IN (
      SELECT link
      FROM entries
      WHERE feed = ?
      ORDER BY published DESC
      LIMIT 1000);
  |] (feed.url, feed.url)

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
  for_ feeds $ \feed -> do
    items <- refreshFeedChecked feed
    mapM_ (saveItem conn) items

-- | Refreshes every feed from the database, every 10 minutes.
refreshLoop :: DB.Connection -> MVar () -> IO ()
refreshLoop conn mvar = do
  _ <- timeout (1_000_000 * 60 * 10) (takeMVar mvar)
  refreshEveryFeed conn
  refreshLoop conn mvar

cleanupLoop :: DB.Connection -> IO ()
cleanupLoop conn = do
  cleanupDbEntries conn
  threadDelay (1_000_000 * 60 * 60 * 24) -- sleep 1 day
  cleanupLoop conn
