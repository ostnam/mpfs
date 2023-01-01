module View exposing (..)


import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import Browser exposing (Document)
import Css.Animations
import Css
import Time
import Types exposing (..)
import InterfaceTypes exposing (..)
import Themes exposing (..)

-- Top-level function that renders the entire document.
view : Model -> Document Msg
view model =
    let
        u =
            Html.Styled.toUnstyled

        mainBody =
            u (viewBody model)
    in
    case model.popup of
        None ->
            Document "MPFS" [ mainBody ]

        SettingsPopup ->
            Document "MPFS" [ mainBody, u (renderSettingsPopup model) ]

        AddFeedPopup ->
            Document "MPFS" [ mainBody, u (renderAddFeedPopup model) ]



-- Renders the main body: everything outside of popups.
viewBody : Model -> Html.Styled.Html Msg
viewBody model =
    div
        [ id "main"
        , css
            [ Css.fontFamily Css.sansSerif
            , Css.fontSize (Css.px 20)
            , Css.width (Css.pct 100)
            , Css.minHeight (Css.vh 100)
            , Css.backgroundColor <| getBgColor model
            , Css.displayFlex
            , Css.flexDirection Css.row
            , Css.flex (Css.int 1)
            ]
        ]
        [ renderLeftBar model
        , renderEntries model
        ]



-- Renders the left bar.
renderLeftBar : Model -> Html Msg
renderLeftBar model =
    div
        [ id "leftbar"
        , css
            [ Css.maxWidth (Css.px 400)
            , Css.minWidth (Css.px 170)
            , Css.width (Css.px 270)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "user-select" "none"
            ]
        ]
        ([ renderOptions model
         , renderTotal model
         ]
            ++ List.map (renderFeedInLeftBar model) model.feeds
        )


-- Renders the 'total' row on top of the feed list.
renderTotal : Model -> Html Msg
renderTotal model =
    let
        totCount =
            List.map .entries model.feeds
                |> List.map (List.filter (\e -> not e.seen))
                |> List.map List.length
                |> List.foldl (+) 0

        totalBgColor =
            case model.selectedFeed of
                All -> [Css.backgroundColor <| getHighlightColor model]
                _   -> []
        entryOrEntries =
            case totCount of
                1 -> " entry"
                _ -> " entries"

    in
    b
        [ css <|
            [ Css.padding4 (Css.px 0) (Css.px 0) (Css.px 0) (Css.px 10)
            , Css.borderStyle Css.hidden
            , Css.margin4 (Css.px 2) (Css.px 0) (Css.px 2) (Css.px 0)
            , Css.borderRadius (Css.px 5)
            , Css.property "user-select" "none"
            , Css.color <| getTextColor model
            ] ++ totalBgColor
        , onClick <| SelectFeed All
        ]
        [ text (String.fromInt totCount ++ entryOrEntries) ]


-- Produces the Html element for a single feed, to be inserted in the left bar.
renderFeedInLeftBar : Model -> Feed -> Html Msg
renderFeedInLeftBar model feed =
    let
        deleteModeOn = model.deleteFeedMode

        titleOnClick =
            onClick <|
                if deleteModeOn then
                    DeleteFeed feed.data
                else
                    SelectFeed (Single feed.data)

        numUnseenEntries = feed.entries
            |> List.filter (\e -> not e.seen)
            |> List.length

        bgColor =
            case model.selectedFeed of
                All      -> []
                Single f ->
                    if feed.data == f
                    then [Css.backgroundColor <| getHighlightColor model]
                    else []
    in
    div
        [ css <|
            [ Css.padding4 (Css.px 0) (Css.px 2) (Css.px 0) (Css.px 4)
            , Css.margin4 (Css.px 0) (Css.px 2) (Css.px 4) (Css.px 4)
            , Css.borderWidth (Css.px 1)
            , Css.borderColor <| getTextColor model
            , Css.borderRadius (Css.px 5)
            , Css.borderStyle (Css.solid)
            , Css.overflow Css.hidden
            , Css.displayFlex
            , Css.flexDirection Css.row
            , Css.property "user-select" "none"
            ] ++ bgColor
        , titleOnClick
        ]
        [ div
            [ css
                [ Css.minHeight Css.fitContent
                , Css.maxHeight Css.fitContent
                , Css.minWidth Css.fitContent
                , Css.maxWidth Css.fitContent
                , Css.margin4 (Css.px 0) (Css.px 8) (Css.px 0) (Css.px 0)
                , Css.color <| getFeedNameColor model
                ]
            ]
            [ text (String.fromInt numUnseenEntries)
            ]
        , div
            [ css
                [ Css.textOverflow Css.ellipsis
                , Css.whiteSpace Css.noWrap
                , Css.overflow Css.hidden
                , Css.color <| getFeedNameColor model
                ]
            ]
            [ text feed.data.name
            ]
        ]


optionButtonStyle : List Css.Style
optionButtonStyle =
    [ Css.height (Css.pct 100)
    ]


optionImgStyle : Model -> List Css.Style
optionImgStyle model =
    [ Css.pointerEvents Css.none
    , Css.height (Css.pct 100)
    , Css.width (Css.pct 100)
    , Css.property "object-fit" "scale-down"
    ] 
    ++ if model.nightMode 
        then [ Css.property "filter" "invert(1)" ]
        else []

refreshButtonAnimation : Model -> List Css.Style
refreshButtonAnimation model = if model.pendingUpdateResponse
    then
        [ Css.animationDuration (Css.sec 1)
        , Css.animationIterationCount Css.infinite
        , Css.animationName
            <| Css.Animations.keyframes
                [ (  0, [Css.Animations.property "rotate" "0deg"])
                , (100, [Css.Animations.property "rotate" "360deg"])
                ]
        ]
    else []



renderOptions : Model -> Html Msg
renderOptions model =
    div
        [ css
            [ Css.height (Css.px 30)
            , Css.displayFlex
            , Css.flexFlow1 Css.row
            , Css.margin (Css.px 4)
            , Css.property "user-select" "none"
            , Css.property "gap" "4px"
            ]
        ]
        [ div
            [ css optionButtonStyle
            , onClick RefreshButtonPressed
            ]
            [ img
                [ src "static/images/refresh.png"
                , css <| optionImgStyle model ++ refreshButtonAnimation model
                ]
                []
            ]
        , div
            [ css optionButtonStyle
            , onClick AddFeedButtonPressed
            ]
            [ img [ src "static/images/add_feed.png", css <| optionImgStyle model ] []
            ]
        , div
            [ css optionButtonStyle
            , onClick DeleteFeedButtonPressed
            ]
            [ img [ src "static/images/remove_feed.png", css <| optionImgStyle model ] [] ]
        , div
            [ css optionButtonStyle
            , onClick SettingsButtonPressed
            ]
            [ img [ src "static/images/settings.png", css <| optionImgStyle model ] [] ]
        , div
            [ css optionButtonStyle
            , onClick ToggledNightMode
            ]
            [ img [ src "static/images/toggle_night_mode.png", css <| optionImgStyle model ] [] ]
        ]


addFeedPopupRootStyle : List Css.Style
addFeedPopupRootStyle =
    [ Css.display Css.block
    , Css.position Css.fixed
    , Css.zIndex (Css.int 1)
    , Css.left (Css.px 0)
    , Css.top (Css.px 0)
    , Css.width (Css.pct 100)
    , Css.height (Css.pct 100)
    , Css.overflow Css.auto
    , Css.backgroundColor (Css.rgba 0 0 0 0.4)
    , Css.fontFamily Css.sansSerif
    , Css.fontSize (Css.px 20)
    ]


addFeedPopupContentStyle : Model -> List Css.Style
addFeedPopupContentStyle model =
    [ Css.backgroundColor <| getBgColor model
    , Css.margin2 (Css.pct 15) Css.auto
    , Css.padding (Css.px 20)
    , Css.border3 (Css.px 1) Css.solid (Css.rgb 136 136 136)
    , Css.width (Css.pct 80)
    , Css.maxWidth (Css.px 800)
    ]


popupTopBar : String -> Model  -> Html Msg
popupTopBar title model =
    div
        [ css
            [ Css.height (Css.px 30)
            , Css.displayFlex
            , Css.flexFlow1 Css.row
            , Css.margin4 (Css.px 0) (Css.px 0) (Css.px 20) (Css.px 0)
            ]
        ]
        [ b
            [ css
                [ Css.textAlign Css.center
                , Css.flex Css.auto
                , Css.fontSize (Css.px 25)
                , Css.color <| getTextColor model
                ]
            ]
            [ text title ]
        , div
            [ onClick ClosePopupPressed
            , css optionButtonStyle
            ]
            [ img
                [ css <|
                    optionImgStyle model 
                    ++ if model.nightMode 
                    then [ Css.property "filter" "invert(1)" ]
                    else []
                , src "static/images/close.png"
                ]
                []
            ]
        ]


addFeedName : String -> Html Msg
addFeedName s =
    input
        [ type_ "text"
        , placeholder "Feed name"
        , value s
        , onInput EditedAddFeedName
        ]
        []


addFeedUrl : String -> Html Msg
addFeedUrl s =
    input
        [ type_ "text"
        , placeholder "Feed url"
        , value s
        , onInput EditedAddFeedUrl
        ]
        []


addFeedSubmitButton : Html Msg
addFeedSubmitButton =
    button
        [ onClick SubmitAddFeed
        ]
        [ text "Submit"
        ]


renderAddFeedPopup : Model -> Html Msg
renderAddFeedPopup model =
    div [ css addFeedPopupRootStyle ]
        [ div [ css <| addFeedPopupContentStyle model ]
            [ popupTopBar "Add feed" model
            , addFeedName model.addFeedName
            , br [] []
            , addFeedUrl model.addFeedUrl
            , br [] []
            , addFeedSubmitButton
            ]
        ]


renderSettingsPopup : Model -> Html Msg
renderSettingsPopup model =
    div [ css addFeedPopupRootStyle ]
        [ div [ css <| addFeedPopupContentStyle model ]
            [ popupTopBar "Settings" model
            , button
                [ onClick SubscriptionsJsonClicked
                ]
                [ text "Load subscriptions from JSON."
                ]
            ]
        ]


renderEntries : Model -> Html Msg
renderEntries model =
    let
        unpackEntries : Feed -> List { feed : FeedData, entry : Entry }
        unpackEntries f =
            List.map (\e -> { feed = f.data, entry = e }) f.entries

        displayedEntries =
            case model.selectedFeed of
                All ->
                    model.feeds
                        |> List.map unpackEntries
                        |> List.concat
                        |> List.sortBy (\e -> e.entry.data.published |> Time.posixToMillis)
                        |> List.reverse

                Single s ->
                    model.feeds
                        |> List.filter (\f -> f.data == s)
                        |> List.map unpackEntries
                        |> List.concat
                        |> List.sortBy (\e -> e.entry.data.published |> Time.posixToMillis)
                        |> List.reverse
    in
    div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.width <| Css.px 1200
            ]
        ]
    <| case displayedEntries of
        [] -> [renderNoEntryMsg model]
        ls -> List.map (\x -> renderEntry x.entry x.feed model) ls


renderNoEntryMsg : Model -> Html Msg
renderNoEntryMsg model =
    div
        [ css
            [ Css.maxWidth (Css.px 1000)
            , Css.textAlign Css.center
            , Css.padding (Css.px 50)
            , Css.fontStyle (Css.italic)
            , Css.color <| getTextColor model
            ]
        ]
        [ text "No entry available in current feed"
        ]

renderEntry : Entry -> FeedData -> Model -> Html Msg
renderEntry entry parentFeed model =
    let
        bgColor = Css.backgroundColor <| getEntryBgColor entry.seen model
    in
    div
        [ css
            [ Css.maxWidth (Css.px 1000)
            , Css.padding4 (Css.px 0) (Css.px 2) (Css.px 0) (Css.px 6)
            , Css.margin4 (Css.px 6) (Css.px 2) (Css.px 0) (Css.px 0)
            , Css.overflow Css.hidden
            , Css.outline Css.none
            , bgColor
            ]
        ]
        [ div
            [ css
                [ Css.border (Css.px 0)
                , Css.width (Css.pct 100)
                , Css.borderRadius (Css.px 2)
                ]
            ]
            [ div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.row
                    ]
                ]
                [ a
                    [ href entry.data.link
                    , onClick (SeenEntry entry.data parentFeed)
                    , on "auxclick" <| Json.Decode.succeed <| SeenEntry entry.data parentFeed
                    , css
                        [ Css.margin4 (Css.px 0) Css.auto (Css.px 0) (Css.px 0)
                        , Css.textDecoration Css.none
                        , Css.color <| getTextColor model
                        ]
                    ]
                    [ b [] [ text entry.data.title ]
                    ]
                , div
                    [ css
                        [ Css.height (Css.px 25)
                        , Css.fontSize (Css.px 30)
                        , Css.textAlign Css.right
                        , Css.displayFlex
                        , Css.margin (Css.px 0)
                        , Css.property "user-select" "none"
                        ]
                    , onClick <| DeleteEntryButtonPressed entry.data parentFeed
                    ]
                    [ text "🞫"
                    ]
                ]
            ]
        , div 
            [ css [ Css.color <| getAltTextColor model ]
            ]
            [ text <| parentFeed.name ++ ", " ++ displayTime entry.data.published model.tz ]
        ]


displayTime : Time.Posix -> Time.Zone -> String
displayTime t tz =
    let
        day = case Time.toWeekday tz t of
            Time.Mon -> "Mon"
            Time.Tue -> "Tue"
            Time.Wed -> "Wed"
            Time.Thu -> "Thu"
            Time.Fri -> "Fri"
            Time.Sat -> "Sat"
            Time.Sun -> "Sun"

        dayNum = String.fromInt <| Time.toDay tz t

        month = case Time.toMonth tz t of
            Time.Jan -> "Jan"
            Time.Feb -> "Feb"
            Time.Mar -> "Mar"
            Time.Apr -> "Apr"
            Time.May -> "May"
            Time.Jun -> "Jun"
            Time.Jul -> "Jul"
            Time.Aug -> "Aug"
            Time.Sep -> "Sep"
            Time.Oct -> "Oct"
            Time.Nov -> "Nov"
            Time.Dec -> "Dec"

        pad str = if String.length str < 2 then
            "0" ++ str
            else str

        year = Time.toYear tz t |> String.fromInt |> pad
        h = Time.toHour tz t |> String.fromInt |> pad
        m = Time.toMinute tz t |> String.fromInt |> pad
        s = Time.toSecond tz t |> String.fromInt |> pad
    in
    day ++ " " ++ dayNum ++ " " ++ month ++ " " ++ year ++ " " ++ h ++ ":" ++ m ++ ":" ++ s

