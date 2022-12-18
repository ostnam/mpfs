module Decoders exposing (..)

import Json.Decode exposing (Decoder)

import Types exposing (..)

feedDataListDecoder : Decoder (List FeedData)
feedDataListDecoder = Json.Decode.list feedDataDecoder

feedDataDecoder : Decoder FeedData
feedDataDecoder = Json.Decode.map2 FeedData
  (Json.Decode.field "url" Json.Decode.string)
  (Json.Decode.field "name" Json.Decode.string)
