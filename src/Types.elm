module Types exposing (..)

import Time
import Json.Encode as Encode
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Decode.Extra

type alias FeedData = {
  url: String,
  name: String
  }


type alias EntryData = {
  title: String,
  link: String,
  published: Time.Posix,
  seen: Bool,
  feed: String
  }


feedDataListDecoder : Decoder (List FeedData)
feedDataListDecoder = Json.Decode.list feedDataDecoder


feedDataDecoder : Decoder FeedData
feedDataDecoder = Json.Decode.map2 FeedData
  (Json.Decode.field "url" Json.Decode.string)
  (Json.Decode.field "name" Json.Decode.string)


feedDataEncoder : FeedData -> Encode.Value
feedDataEncoder feed =
  Encode.object
    [ ("name", Encode.string feed.name)
    , ("url", Encode.string feed.url)
    ]

entryDataEncoder : EntryData -> Encode.Value
entryDataEncoder entry =
  Encode.object
    [ ("url", Encode.string entry.title) ]

entryDataDecoder : Decoder EntryData
entryDataDecoder = 
  Json.Decode.succeed EntryData
   |> required "title" Json.Decode.string
   |> required "link" Json.Decode.string
   |> required "published" Json.Decode.Extra.datetime
   |> required "seen" Json.Decode.bool
   |> required "feed" Json.Decode.string
