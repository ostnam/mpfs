module Types exposing (..)

import Iso8601
import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Time


type alias FeedData =
    { url : String
    , name : String
    }


type alias EntryData =
    { title : String
    , link : String
    , published : Time.Posix
    , seen : Bool
    , parentFeedUrl : String
    }


feedDataListDecoder : Decoder (List FeedData)
feedDataListDecoder =
    Json.Decode.list feedDataDecoder


feedDataDecoder : Decoder FeedData
feedDataDecoder =
    Json.Decode.map2 FeedData
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)


feedDataEncoder : FeedData -> Encode.Value
feedDataEncoder feed =
    Encode.object
        [ ( "name", Encode.string feed.name )
        , ( "url", Encode.string feed.url )
        ]


feedDataListToJson : List FeedData -> String
feedDataListToJson list =
    Encode.list feedDataEncoder list
        |> Encode.encode 0


entryDataEncoder : EntryData -> Encode.Value
entryDataEncoder entry =
    Encode.object
        [ ( "title", Encode.string entry.title )
        , ( "link", Encode.string entry.link )
        , ( "published", Encode.string <| Iso8601.fromTime entry.published )
        , ( "seen", Encode.bool entry.seen )
        , ( "parentFeedUrl", Encode.string entry.parentFeedUrl )
        ]


entryDataDecoder : Decoder EntryData
entryDataDecoder =
    Json.Decode.succeed EntryData
        |> required "title" Json.Decode.string
        |> required "link" Json.Decode.string
        |> required "published" Json.Decode.Extra.datetime
        |> required "seen" Json.Decode.bool
        |> required "parentFeedUrl" Json.Decode.string
