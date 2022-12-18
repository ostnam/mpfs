module Types exposing (..)

import Time
import Json.Encode as Encode

type alias FeedData = {
  url: String,
  name: String
  }


type alias Entry = {
  title: String,
  link: String,
  published: Time.Posix,
  seen: Bool,
  feed: String
  }

feedDataEncoder : FeedData -> Encode.Value
feedDataEncoder feed =
  Encode.object
    [ ( "name", Encode.string feed.name)
    , ( "url", Encode.string feed.url)
    ]
