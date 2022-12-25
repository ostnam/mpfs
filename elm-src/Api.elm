module Api exposing (..)

import Http exposing (..)
import Json.Decode
import Json.Encode
import Types exposing (..)


type ApiMessage
    = Entries (Result Http.Error (List EntryData))
    | NoVal (Result Http.Error ())


registerFeed : FeedData -> Cmd ApiMessage
registerFeed feed =
    post
        { url = "/subscriptions"
        , body = jsonBody (feedDataEncoder feed)
        , expect = expectWhatever NoVal
        }


registerFeeds : List FeedData -> Cmd ApiMessage
registerFeeds feeds =
    post
        { url = "/subscriptions_batch"
        , body = jsonBody <| Json.Encode.list feedDataEncoder <| feeds
        , expect = expectWhatever NoVal
        }


unSubscribeFeed : FeedData -> Cmd ApiMessage
unSubscribeFeed feed =
    request
        { url = "/subscriptions"
        , method = "DELETE"
        , headers = []
        , body = jsonBody (feedDataEncoder feed)
        , expect = expectWhatever NoVal
        , timeout = Nothing
        , tracker = Nothing
        }


updateSeen : EntryData -> Cmd ApiMessage
updateSeen entry =
    post
        { url = "/seen"
        , body = jsonBody (entryDataEncoder entry)
        , expect = expectWhatever NoVal
        }


refreshFeeds : List FeedData -> Cmd ApiMessage
refreshFeeds feeds =
    post
        { url = "/entries_batch"
        , body = jsonBody (Json.Encode.list feedDataEncoder feeds)
        , expect = expectJson Entries (Json.Decode.list entryDataDecoder)
        }
