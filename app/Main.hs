{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import Network.Wai.Middleware.HttpAuth
import System.Environment (getEnv)
import Network.Wai ( Request, pathInfo, Middleware )
import Data.ByteString.Internal (c2w)
import Network.Wai.Middleware.Static
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BL

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

main :: IO ()
main = do
  userId <- getEnv "MPFS_USERID"
  sessionId <- getEnv "MPFS_SESSION_KEY"
  scotty 8080 $ do
    middleware $ auth userId sessionId
    middleware $ staticPolicy $ hasPrefix "static"

    get "/" $ redirect "/feeds"
    get "/feeds" $ setHeader "Content-Type" "text/html" >> file "templates/feeds.html"
