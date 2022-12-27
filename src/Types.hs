{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where

import Text.Feed.Import ( parseFeedSource )
import Data.Maybe (mapMaybe)
import Data.Time.Format.ISO8601 ( iso8601ParseM )
import Data.Time.RFC822 ( parseTimeRFC822 )
import Control.Monad ( msum )
import Data.Aeson
import GHC.Generics
import Data.ByteString.Lazy ( ByteString )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TLazy
import qualified Data.Time as Time
import qualified Text.Feed.Types as FTypes
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as DB
import qualified Data.Vector

data Feed = Feed 
  { name :: T.Text
  , url :: T.Text
  } deriving (Generic, Show, FromJSON)

instance ToJSON Feed where
  toJSON feed = object [ "name" .= feed.name
                       , "url"  .= feed.url
                       ]
  toJSONList feeds = Array $ Data.Vector.fromList $ toJSON <$> feeds

instance DB.FromRow Feed where
  fromRow = Feed <$> DB.field <*> DB.field

instance DB.ToRow Feed where
  toRow f = [DB.SQLText f.name, DB.SQLText f.url]

data FeedItem = FeedItem
  { title :: T.Text
  , link :: T.Text
  , published :: Maybe Time.UTCTime
  , seen :: Bool
  , parentFeedUrl :: T.Text
  } deriving (Generic, Show, ToJSON, FromJSON)

instance DB.FromRow FeedItem where
  fromRow = FeedItem <$> DB.field <*> DB.field <*> DB.field <*> DB.field <*> DB.field

instance DB.ToRow FeedItem where
  toRow f = DB.toRow ( f.title
                     , f.link
                     , f.published
                     , f.seen
                     , f.parentFeedUrl
                     )


parseFeed :: T.Text -> ByteString -> [FeedItem]
parseFeed parentFeedUrl t = case parseFeedSource t of
  Just (FTypes.AtomFeed f) -> mapMaybe (atomEntryToItem parentFeedUrl) f.feedEntries
  Just (FTypes.RSSFeed  f) -> mapMaybe (rss2EntryToItem parentFeedUrl) f.rssChannel.rssItems
  Just (FTypes.RSS1Feed f) -> rss1EntryToItem parentFeedUrl <$> f.feedItems
  Just (FTypes.XMLFeed  _) -> []
  Nothing                  -> []


rss1EntryToItem :: T.Text -> RSS1.Item -> FeedItem
rss1EntryToItem parentFeedUrl item =  FeedItem 
  { title = item.itemTitle
  , link = item.itemLink
  , published = Nothing
  , seen = False
  , parentFeedUrl = parentFeedUrl
  }


rss2EntryToItem :: T.Text -> RSS.RSSItem -> Maybe FeedItem
rss2EntryToItem parentFeedUrl item = let
  title = msum [item.rssItemTitle, item.rssItemDescription]
  published = Time.zonedTimeToUTC <$> (item.rssItemPubDate >>= parseTimeRFC822)
  in case (title, item.rssItemLink) of
    (Just t, Just l) -> Just $ FeedItem
      { title = t
      , link = l
      , published = published
      , seen = False
      , parentFeedUrl = parentFeedUrl
      }
    _ -> Nothing


atomEntryToItem :: T.Text -> Atom.Entry -> Maybe FeedItem
atomEntryToItem parentFeedUrl e = let
  link = case e.entryLinks of
    (x:_) -> Just x.linkHref
    []    -> Nothing   
  title = case e.entryTitle of
    Atom.TextString t -> Just t
    Atom.HTMLString t -> Just t
    Atom.XHTMLString _ -> Nothing
  published = iso8601ParseM $ T.unpack e.entryUpdated

  in case (link, title) of
    (Just l, Just t) -> Just $ FeedItem 
      { title = t
      , link = l
      , published = published
      , seen = False
      , parentFeedUrl = parentFeedUrl
      }
    _ -> Nothing


fillMissingPublishedTime :: FeedItem -> IO FeedItem
fillMissingPublishedTime item =
  case item.published of
    Just _  -> return item
    Nothing -> do
      now <- Time.getCurrentTime
      return FeedItem
        { title = item.title
        , link = item.link
        , published = Just now
        , seen = item.seen
        , parentFeedUrl = item.parentFeedUrl
        }
