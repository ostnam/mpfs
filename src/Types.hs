{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where

import Data.Aeson
import Text.Feed.Import ( parseFeedSource )
import Data.Maybe ( fromMaybe )
import Data.Time.Format.ISO8601 ( iso8601ParseM )
import Data.Time.RFC822 ( parseTimeRFC822 )
import Control.Monad ( msum )
import GHC.Generics ( Generic )
import Data.ByteString.Lazy ( ByteString )
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Text.Feed.Types as FTypes
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1
import qualified Database.SQLite.Simple as DB
import qualified Data.Vector

-- | Represents a single feed.
data Feed = Feed 
  { name :: T.Text -- ^ Set by the user.
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

-- | Represents the values that can be obtained from parsing.
data ParsedFeedItem = ParsedFeedItem
  { parsedTitle :: Maybe T.Text
  , parsedLink :: Maybe T.Text
  , parsedPublished :: Maybe Time.UTCTime
  } deriving (Generic, Show, ToJSON, FromJSON)


{-| Converts a ParsedFeedItem to a regular FeedItem
 - The missing values are filled by default ones.
 -}
toFeedItem :: T.Text
           -> T.Text
           -> T.Text
           -> ParsedFeedItem
           -> IO FeedItem
toFeedItem defaultTitle defaultLink parentFeed entry = let
  finalTitle = fromMaybe defaultTitle entry.parsedTitle
  finalLink = fromMaybe defaultLink entry.parsedLink
  in case entry.parsedPublished of
    Just t -> return $ FeedItem
      { title = finalTitle
      , link = finalLink
      , published = t
      , seen = False
      , parentFeedUrl = parentFeed
      }
    Nothing -> do
      now <- Time.getCurrentTime
      return $ FeedItem
        { title = finalTitle
        , link = finalLink
        , published = now
        , seen = False
        , parentFeedUrl = parentFeed
        }


data FeedItem = FeedItem
  { title :: T.Text
  , link :: T.Text
  , published :: Time.UTCTime
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


parseFeed :: ByteString -> [ParsedFeedItem]
parseFeed t = case parseFeedSource t of
  Just (FTypes.AtomFeed f) -> atomEntryToItem <$> f.feedEntries
  Just (FTypes.RSSFeed  f) -> rss2EntryToItem <$> f.rssChannel.rssItems
  Just (FTypes.RSS1Feed f) -> rss1EntryToItem <$> f.feedItems
  Just (FTypes.XMLFeed  _) -> []
  Nothing                  -> []


rss1EntryToItem :: RSS1.Item -> ParsedFeedItem
rss1EntryToItem item = ParsedFeedItem
  { parsedTitle = Just item.itemTitle
  , parsedLink = Just item.itemLink
  , parsedPublished = Nothing
  }


rss2EntryToItem :: RSS.RSSItem -> ParsedFeedItem
rss2EntryToItem item = ParsedFeedItem
  { parsedTitle = msum [item.rssItemTitle, item.rssItemDescription]
  , parsedLink = item.rssItemLink
  , parsedPublished = Time.zonedTimeToUTC <$> (item.rssItemPubDate >>= parseTimeRFC822)
  }


atomEntryToItem :: Atom.Entry -> ParsedFeedItem
atomEntryToItem e = let
  title' = case e.entryTitle of
    Atom.TextString t  -> Just t
    Atom.HTMLString t  -> Just t
    Atom.XHTMLString _ -> Nothing
  published' = iso8601ParseM $ T.unpack e.entryUpdated
  link' = case e.entryLinks of
    (x:_) -> Just $ x.linkHref
    _     -> Nothing
  in ParsedFeedItem
      { parsedTitle = title'
      , parsedLink = link'
      , parsedPublished = published'
      }
