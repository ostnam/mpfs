module InterfaceTypes exposing (..)

import Types exposing (..)
import Time
import Http
import File exposing (File)
import Api exposing (..)

type alias Entry =
    { data : EntryData
    , seen : Bool
    }


type alias Feed =
    { data : FeedData
    , entries : List Entry
    }


type FeedSelected
    = All
    | Single FeedData


type PopupState
    = None
    | AddFeedPopup
    | SettingsPopup


type alias TimeZone =
    { name : String
    , tz : Time.Zone
    }

type alias Model =
    { feeds : List Feed
    , popup : PopupState
    , addFeedName : String
    , addFeedUrl : String
    , deleteFeedMode : Bool
    , selectedFeed : FeedSelected
    , tz : TimeZone
    , pendingUpdateResponse : Bool
    , nightMode : Bool
    }


defaultModel : Model
defaultModel =
    { feeds = []
    , popup = None
    , addFeedName = ""
    , addFeedUrl = ""
    , deleteFeedMode = False
    , selectedFeed = All
    , tz = TimeZone "UTC" Time.utc
    , pendingUpdateResponse = True
    , nightMode = True
    }


type Msg
    = EditedAddFeedName String
    | EditedAddFeedUrl String
    | SubmitAddFeed
    | SelectFeed FeedSelected
    | DeleteFeed FeedData
    | AddFeedButtonPressed
    | DeleteFeedButtonPressed
    | SettingsButtonPressed
    | RefreshButtonPressed
    | ReceivedSubscribedFeeds (Result Http.Error (List FeedData))
    | ClosePopupPressed
    | ApiMessage ApiMessage
    | SeenEntry EntryData FeedData
    | DeleteEntryButtonPressed EntryData FeedData
    | SubscriptionsJsonClicked
    | GotTimeZone TimeZone
    | ToggledNightMode
    | SaveSubscriptions
    | ImportedSubscriptions File
    | LoadedImportedSubscriptions String


addFeeds : List Feed -> List FeedData -> List Feed
addFeeds old new =
    let
        oldDatas =
            List.map .data old

        toAdd =
            List.filter (\e -> not <| List.member e oldDatas) new

        asFeeds =
            List.map (\d -> Feed d []) toAdd
    in
    asFeeds ++ old


submitAddFeed : Model -> ( Model, Cmd Msg )
submitAddFeed model =
    let
        newFeed =
            FeedData model.addFeedUrl model.addFeedName

        oldList =
            List.map .data model.feeds
    in
    if not (List.member newFeed oldList) then
        ( { model
            | feeds = Feed newFeed [] :: model.feeds
            , addFeedName = ""
            , addFeedUrl = ""
          }
        , Cmd.map ApiMessage (registerFeed newFeed)
        )

    else
        ( model, Cmd.none )


updateEntries : List EntryData -> List Feed -> List Feed
updateEntries entries feeds =
    let
        sameFeed feed entryData =
            entryData.parentFeedUrl == feed.data.url

        updateFn entries2 feed =
            { feed | entries =
                List.filter (sameFeed feed) entries2
                    |> List.map (\e -> Entry e False)
            }
    in
    List.map (updateFn entries) feeds
