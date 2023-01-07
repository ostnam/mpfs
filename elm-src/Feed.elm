module Feed exposing (..)

import Browser
import Http
import Task
import Time
import TimeZone
import File.Download
import Api exposing (..)
import Types exposing (..)
import InterfaceTypes exposing (..)
import View exposing (..)
import File
import File.Select
import Json.Decode

------------------------------------------------------------
---------------------- Main function -----------------------
------------------------------------------------------------
main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


------------------------------------------------------------
--------------------------- Init ---------------------------
------------------------------------------------------------
init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel
    , runInit )


runInit : Cmd Msg
runInit = Cmd.batch
    [ Task.attempt timeZoneHandler TimeZone.getZone
    , Http.get
        { url = "/subscriptions"
        , expect = Http.expectJson ReceivedSubscribedFeeds feedDataListDecoder
        }
    ]


------------------------------------------------------------
-------------------- Updating the model --------------------
------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddFeedButtonPressed ->
            ( { model | popup = AddFeedPopup }, Cmd.none )

        ClosePopupPressed ->
            ( { model | popup = None }, Cmd.none )

        EditedAddFeedName s ->
            ( { model | addFeedName = s }, Cmd.none )

        EditedAddFeedUrl s ->
            ( { model | addFeedUrl = s }, Cmd.none )

        SubmitAddFeed ->
            submitAddFeed model

        ReceivedSubscribedFeeds r ->
            case r of
                Ok l ->
                    let
                        newFeeds =
                            List.map (\f -> Feed f []) l
                    in
                    ( { model | feeds = newFeeds }, Cmd.map ApiMessage <| refreshFeeds <| List.map .data newFeeds )

                Err _ ->
                    ( model, Cmd.none )

        DeleteFeedButtonPressed ->
            ( { model | deleteFeedMode = not model.deleteFeedMode }, Cmd.none )

        SelectFeed All ->
            (   { model
                    | selectedFeed = All
                    , feeds = model.feeds
                        |> List.map (\f -> { f | entries = List.filter (\e -> not e.seen) f.entries})}
            , Cmd.none)

        SelectFeed (Single s) ->
            (   { model
                    | selectedFeed = Single s
                    , feeds = model.feeds
                        |> List.map (\f ->
                            if f.data == s
                            then
                                { f | entries = List.filter (\e -> not e.seen) f.entries }
                            else f)
                }
            , Cmd.none )

        DeleteFeed feed ->
            ( { model
                | feeds =
                    List.filter (\f -> f.data /= feed) model.feeds
              }
            , Cmd.map ApiMessage (unSubscribeFeed feed)
            )

        SeenEntry entryData parent ->
            ( { model
                | feeds =
                    List.map
                        (\f ->
                            if f.data == parent then
                                { f
                                    | entries =
                                        List.map
                                            (\e ->
                                                if e.data == entryData then
                                                    { e | seen = True }

                                                else
                                                    e
                                            )
                                            f.entries
                                }

                            else
                                f
                        )
                        model.feeds
              }
            , Cmd.map ApiMessage (updateSeen entryData)
            )

        DeleteEntryButtonPressed entryData parent ->
            ( { model
                | feeds =
                    List.map
                        (\f ->
                            if not (f.data == parent) then
                                f

                            else
                                { f | entries = List.filter (\e -> e.data /= entryData) f.entries }
                        )
                        model.feeds
              }
            , Cmd.map ApiMessage (updateSeen entryData)
            )

        ApiMessage m ->
            case m of
                Entries (Ok entries) ->
                    ( { model
                        | feeds = updateEntries entries model.feeds
                        , pendingUpdateResponse = False
                        }, Cmd.none )

                Entries (Err _) ->
                    ( { model | pendingUpdateResponse = False }  , Cmd.none )

                NoVal _ ->
                    ( model, Cmd.none )

        SettingsButtonPressed ->
            ( { model | popup = SettingsPopup }, Cmd.none )

        SubscriptionsJsonClicked ->
            ( model, File.Select.file ["application/json"] ImportedSubscriptions )

        RefreshButtonPressed ->
            ( { model | pendingUpdateResponse = True }
            , List.map .data model.feeds
                |> refreshFeeds
                |> Cmd.map ApiMessage
            )

        GotTimeZone t ->
            ( { model | tz = t }, Cmd.none )

        ToggledNightMode ->
            ( { model | nightMode = not model.nightMode }, Cmd.none )

        SaveSubscriptions ->
            let subs = model.feeds
                    |> List.map .data
                    |> feedDataListToJson
            in
                ( model, File.Download.string "subscriptions.json" "application/json" subs)

        ImportedSubscriptions file ->
            (model, Task.perform LoadedImportedSubscriptions (File.toString file))

        LoadedImportedSubscriptions str ->
            case Json.Decode.decodeString feedDataListDecoder str of
                Ok subs ->
                    ({ model | feeds = addFeeds model.feeds subs },
                     Cmd.map ApiMessage <| registerFeeds subs)
                Err _ ->
                    (model, Cmd.none)



timeZoneHandler : Result TimeZone.Error ( String, Time.Zone ) -> Msg
timeZoneHandler r =
    GotTimeZone <| case r of
        Err _         -> TimeZone "UTC" Time.utc
        Ok (name, tz) -> TimeZone name tz
