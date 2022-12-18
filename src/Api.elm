module Api exposing (..)

import Http exposing (..)
import Types exposing (..)

type ApiMessage =
  None (Result Http.Error ())

registerFeed : FeedData -> Cmd ApiMessage
registerFeed feed =
  post 
    { url  = "/subscriptions"
    , body = jsonBody (feedDataEncoder feed)
    , expect = expectWhatever None
    }

unSubscribeFeed : FeedData -> Cmd ApiMessage
unSubscribeFeed feed =
  request
    { url  = "/subscriptions"
    , method = "DELETE"
    , headers = []
    , body = jsonBody (feedDataEncoder feed)
    , expect = expectWhatever None
    , timeout = Nothing
    , tracker = Nothing
    }
