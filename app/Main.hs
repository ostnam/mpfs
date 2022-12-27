{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Web.Scotty
import Network.Wai.Middleware.HttpAuth
import System.Environment (getEnv, lookupEnv)
import Network.Wai ( Request, pathInfo, Middleware )
import Data.ByteString.Internal (c2w)
import Network.Wai.Middleware.Static
import Control.Monad (mapM, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent ( forkIO )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Database.SQLite.Simple as DB
import qualified Data
import qualified Types

authSettings :: AuthSettings
authSettings = "MPFS" { authIsProtected = needsAuth }

needsAuth :: Request -> IO Bool
needsAuth req = return $ case pathInfo req of
  "login":_ -> False
  _         -> True

checkCreds :: String -> String -> BS.ByteString -> BS.ByteString -> IO Bool
checkCreds shouldBeUser shouldBePw username password = return $
  username == u && password == p
    where u = BS.pack $ c2w <$> shouldBeUser
          p = BS.pack $ c2w <$> shouldBePw

auth :: String -> String -> Middleware
auth userId sessionId = basicAuth (checkCreds userId sessionId) authSettings

getDbPath :: () -> IO String
getDbPath _ = do
  isEnv <- lookupEnv "MPFS_PRODUCTION"
  case isEnv of
    Just _ -> return "/data/feed.db"
    _      -> return "./feed.db"

main :: IO ()
main = do
  userId <- getEnv "MPFS_USERID"
  sessionId <- getEnv "MPFS_SESSION_KEY"
  dbPath <- getDbPath ()

  conn <- DB.open dbPath
  Data.setUpDb conn

  _ <- forkIO $ Data.refreshLoop conn

  scotty 8080 $ do
    middleware $ auth userId sessionId
    middleware $ staticPolicy $ hasPrefix "static"

    get "/" $ redirect "/feeds"
    get "/feeds" $ setHeader "Content-Type" "text/html" >> file "templates/feeds.html"
    post "/seen" $ do
      reqBody <- body
      case Aeson.decode reqBody of
        Just item -> liftIO $ Data.markSeen conn item
        _         -> return ()
      text ""

    post "/subscriptions" $ do
      reqBody <- body
      case Aeson.decode reqBody of
        Just feed -> liftIO $ Data.addFeed conn feed
        _ ->  return ()
      text ""

    delete "/subscriptions" $ do
      reqBody <- body
      case Aeson.decode reqBody of
        Just feed -> liftIO $ Data.deleteFeed conn feed
        _ ->  return ()
      text ""

    get "/subscriptions" $ do
      feeds <- liftIO $ Data.getFeeds conn
      json feeds

    post "/entries_batch" $ do
      reqBody <- body
      items <- case Aeson.decode reqBody of
        Just (feeds :: [Types.Feed]) -> concat <$> mapM (liftIO . Data.getItems conn) feeds
        _          -> return []
      json items

  DB.close conn
