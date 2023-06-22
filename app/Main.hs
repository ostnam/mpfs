{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( newMVar, tryPutMVar )
import Control.Monad.IO.Class (liftIO)
import Data.Maybe ( fromMaybe )
import System.Environment (getEnv, lookupEnv)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import qualified Database.SQLite.Simple as DB
import Network.Wai ( Middleware )
import Network.Wai.Middleware.HttpAuth
import Network.Wai.Middleware.Static
import Web.Scotty

import qualified Data
import qualified Types

-- Every endpoint requires auth.
authSettings :: AuthSettings
authSettings = "MPFS" { authIsProtected = \_ -> return True }

checkCreds :: String -> String -> BS.ByteString -> BS.ByteString -> IO Bool
checkCreds shouldBeUser shouldBePw username password = return $
  username == u && password == p
    where u = BS.pack $ c2w <$> shouldBeUser
          p = BS.pack $ c2w <$> shouldBePw

auth :: String -> String -> Middleware
auth userId sessionId = basicAuth (checkCreds userId sessionId) authSettings

getDbPath :: IO String
getDbPath = fromMaybe "./feed.db" <$> lookupEnv "MPFS_DB_PATH"

main :: IO ()
main = do
  userId <- getEnv "MPFS_USERID"
  sessionId <- getEnv "MPFS_SESSION_KEY"
  dbPath <- getDbPath

  conn <- DB.open dbPath
  Data.setUpDb conn

  -- Initialize the thread that refreshes the feeds and the DB
  refreshSignal <- newMVar ()
  _ <- forkIO $ Data.refreshLoop conn refreshSignal
  _ <- forkIO $ Data.cleanupLoop conn

  scotty 8080 $ do
    middleware $ auth userId sessionId
    middleware $ staticPolicy $ hasPrefix "static"

    get "/" $ redirect "/feeds"

    get "/feeds" $ setHeader "Content-Type" "text/html" >> file "templates/feeds.html"

    -- Mark a feed item as seen.
    post "/seen" $ do
      reqBody <- body
      case Aeson.decode reqBody of
        Just item -> liftIO $ Data.markSeen conn item
        _         -> return ()
      text ""

    -- Subscribe to a feed.
    post "/subscriptions" $ do
      reqBody <- body
      case Aeson.decode reqBody of
        Just feed -> liftIO $ Data.addFeed conn feed
        _ ->  return ()
      text ""

    -- Unsubscribe to a feed.
    delete "/subscriptions" $ do
      reqBody <- body
      case Aeson.decode reqBody of
        Just feed -> liftIO $ Data.deleteFeed conn feed
        _ ->  return ()
      text ""

    -- Get the subscriptions.
    get "/subscriptions" $ do
      feeds <- liftIO $ Data.getFeeds conn
      json feeds

    -- Get every unseen entry from every subscribed feed.
    post "/entries_batch" $ do
      reqBody <- body
      items <- case Aeson.decode reqBody of
        Just (feeds :: [Types.Feed]) -> do
          _ <- liftIO $ tryPutMVar refreshSignal ()
          concat <$> mapM (liftIO . Data.getUnseenItems conn) feeds
        _ -> return  []
      json items

  DB.close conn
