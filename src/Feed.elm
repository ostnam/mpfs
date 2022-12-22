module Feed exposing (..)

import Time
import Json.Decode
import Browser exposing (Document)
import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Types exposing (..)
import Api exposing (..)


------------------------------------------------------------
---------------------- Defining types ----------------------
------------------------------------------------------------
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
  | Single String


type PopupState
  = None
  | AddFeedPopup
  | SettingsPopup


type alias Model =
    { feeds : List Feed
    , popup : PopupState
    , addFeedName : String
    , addFeedUrl : String
    , deleteFeedMode : Bool
    , selectedFeed : FeedSelected
    }


default_model : Model
default_model =
    Model [] None "" "" False All


type Msg
    = EditedAddFeedName String
    | EditedAddFeedUrl String
    | SubmitAddFeed
    | SelectFeed FeedSelected
    | DeleteFeed FeedData
    | NewEntries (List Entry)
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
---------------------- View functions ----------------------
------------------------------------------------------------
-- Top-level function that renders the entire document.
view : Model -> Document Msg
view model =
  let 
      u = 
        Html.Styled.toUnstyled

      mainBody = 
        u (view_body model)
  in 
  case model.popup of
    None          -> Document "MPFS" [ mainBody ]
    SettingsPopup -> Document "MPFS" [ mainBody, u (renderSettingsPopup model) ]
    AddFeedPopup  -> Document "MPFS" [ mainBody, u (renderAddFeedPopup model) ]

-- Renders the main body: everything outside of popups.
view_body : Model -> Html.Styled.Html Msg
view_body model =
    div
        [ id "main"
        , css
            [ Css.fontFamily Css.sansSerif
            , Css.fontSize (Css.px 20)
            , Css.width (Css.pct 100)
            , Css.displayFlex
            , Css.flexDirection Css.row
            , Css.flex (Css.int 1)
            , Css.minHeight (Css.px 0)
            , Css.backgroundColor (Css.rgb 248 248 248)
            ]
        ]
        [ render_left_bar model
        , render_entries model
        ]


-- Renders the left bar.
render_left_bar : Model -> Html Msg
render_left_bar model =
  div
    [ id "leftbar" 
    , css 
      [ Css.maxWidth (Css.px 400)
      , Css.minWidth (Css.px 170)
      , Css.width (Css.px 270)
      , Css.displayFlex
      , Css.flexDirection (Css.column)
      , Css.backgroundColor (Css.rgb 248 248 248)
      ]
    ]
    ([ render_options,
      renderTotal model.feeds
     ] ++ List.map (renderFeedInLeftBar model.deleteFeedMode) model.feeds)


-- Renders the 'total' row on top of the feed list.
renderTotal : List Feed -> Html Msg
renderTotal l =
  let totCount = List.map .entries l
              |> List.map List.length
              |> List.foldl (+) 0
  in 
    b [css [Css.margin4 (Css.px 0) (Css.px 0) (Css.px 0) (Css.px 10)]]
      [text (String.fromInt totCount ++ " entries")]


-- Produces the Html element for a single feed, to be inserted in the left bar.
renderFeedInLeftBar : Bool -> Feed ->  Html Msg
renderFeedInLeftBar deleteModeOn feed = 
  let 
      nameColor = 
        if deleteModeOn
        then Css.color (Css.rgb 255 0 0)
        else Css.color (Css.rgb 0 0 0)

      titleOnClick = onClick  <|
        if deleteModeOn
        then DeleteFeed feed.data
        else SelectFeed (Single feed.data.url)
  in
  div 
    [css
      [ Css.padding4 (Css.px 0) (Css.px 20) (Css.px 0) (Css.px 20)
      , Css.margin4 (Css.px 0) (Css.px 2) (Css.px 1) (Css.px 0)
      , Css.overflow Css.hidden
      , Css.displayFlex
      , Css.flexDirection Css.row
      ]
    ]
    [ div
        [ css
          [ Css.minHeight Css.fitContent
          , Css.maxHeight Css.fitContent
          , Css.minWidth Css.fitContent
          , Css.maxWidth Css.fitContent
          , Css.margin4 (Css.px 0) (Css.px 10) (Css.px 0) (Css.px 0)
          ]
        ]
        [
          text (String.fromInt (List.length feed.entries))
        ]
      , div
        [ css
          [ Css.minHeight Css.fitContent
          , Css.maxHeight Css.fitContent
          , Css.minWidth Css.fitContent
          , Css.maxWidth Css.fitContent
          , nameColor
          ]
        , titleOnClick
        ]
        [
          text feed.data.name
        ]
    ]


option_button_style : List Css.Style
option_button_style =
    [ Css.margin (Css.px 4)
    , Css.height (Css.pct 100)
    ]


option_img_style : List Css.Style
option_img_style =
    [ Css.pointerEvents Css.none
    , Css.height (Css.pct 100)
    , Css.width (Css.pct 100)
    ]


render_options : Html Msg
render_options =
    div
        [ css
            [ Css.height (Css.px 30)
            , Css.displayFlex
            , Css.flexFlow1 Css.row
            , Css.margin (Css.px 8)
            ]
        ]
        [ div
            [ css option_button_style
            , onClick RefreshButtonPressed
            ]
            [ img [
              src "static/images/refresh.png", css option_img_style ] [] ]
        , div
          [
          css option_button_style
          , onClick AddFeedButtonPressed
          ]
          [ img [ src "static/images/add_feed.png", css option_img_style ] [] 
          ]
        , div
          [ css option_button_style
          , onClick DeleteFeedButtonPressed
          ]
          [ img [ src "static/images/remove_feed.png", css option_img_style ] [] ]
        , div
          [ css option_button_style
          , onClick SettingsButtonPressed
          ]
          [ img [ src "static/images/settings.png", css option_img_style ] [] ]
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
  , Css.overflow (Css.auto)
  , Css.backgroundColor (Css.rgba 0 0 0 0.4)
  , Css.fontFamily Css.sansSerif
  , Css.fontSize (Css.px 20)
  ]


addFeedPopupContentStyle : List Css.Style 
addFeedPopupContentStyle =
  [ Css.backgroundColor (Css.rgb 254 254 254)
  , Css.margin2 (Css.pct 15) (Css.auto)
  , Css.padding (Css.px 20)
  , Css.border3 (Css.px 1) Css.solid (Css.rgb 136 136 136)
  , Css.width (Css.pct 80)
  , Css.maxWidth (Css.px 800)
  ]


popupTopBar : String -> Html Msg
popupTopBar title =
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
        ]
      ]
      [text title]
    , div
      [ onClick ClosePopupPressed
      , css option_button_style
      ]
      [ img
        [ css option_img_style
        , src "static/images/close.png"
        ] []
      ]
    ]


addFeedName : String -> Html Msg
addFeedName s =
  input 
    [ type_ "text"
    , placeholder "Feed name"
    , value s
    , onInput EditedAddFeedName
    ] []


addFeedUrl : String ->  Html Msg
addFeedUrl s =
  input 
    [ type_ "text"
    , placeholder "Feed url"
    , value s
    , onInput EditedAddFeedUrl
    ] []

addFeedSubmitButton : Html Msg
addFeedSubmitButton = 
  button
    [ onClick SubmitAddFeed
    ]
    [ text "Submit"
    ]

renderAddFeedPopup : Model -> Html Msg
renderAddFeedPopup model =
  div [css addFeedPopupRootStyle]
    [
      div [css addFeedPopupContentStyle]
        [ popupTopBar "Add feed"
        , addFeedName model.addFeedName
        , br [] []
        , addFeedUrl model.addFeedUrl
        , br [] []
        , addFeedSubmitButton
        ]
    ]

renderSettingsPopup : Model -> Html Msg
renderSettingsPopup _ =
  div [css addFeedPopupRootStyle]
    [div [css addFeedPopupContentStyle]
      [ popupTopBar "Settings"
      , button
        [ onClick SubscriptionsJsonClicked
        ]
        [
          text "Load subscriptions from JSON."
        ]
      ]
    ]

render_entries : Model -> Html Msg
render_entries model =
  let
    unpackEntries : Feed -> List { feed : FeedData, entry : Entry }
    unpackEntries f = List.map (\e -> { feed = f.data, entry = e} ) f.entries

    displayedEntries = case model.selectedFeed of
      All -> model.feeds
        |> List.map unpackEntries
        |> List.concat
        |> List.sortBy (\e -> e.entry.data.published |> Time.posixToMillis)
        |> List.reverse
      Single s -> model.feeds
        |> List.filter (\f -> f.data.url == s)
        |> List.map unpackEntries
        |> List.concat
        |> List.sortBy (\e -> e.entry.data.published |> Time.posixToMillis)
        |> List.reverse
  in
  div
    [ css
      [ Css.displayFlex
      , Css.flexDirection Css.column
      , Css.backgroundColor <| Css.rgb 248 248 248
      , Css.width <| Css.px 1200
      ]
    ]
    <| List.map (\x -> renderEntry x.entry x.feed) displayedEntries


renderEntry : Entry -> FeedData -> Html Msg
renderEntry entry f =
  let
      bgColor = Css.backgroundColor <|
        if entry.seen
        then Css.rgb 217 216 216
        else Css.rgb 242 240 240
  in
  div
    [ css
      [ Css.maxWidth (Css.px 1000)
      , Css.padding4 (Css.px 0) (Css.px 2) (Css.px 0) (Css.px 6)
      , Css.margin4  (Css.px 0) (Css.px 2) (Css.px 6) (Css.px 0)
      , Css.overflow Css.hidden
      , Css.outline Css.none
      , bgColor
      ]
    ]
    [ div
      [ css
        [ Css.border (Css.px 0)
        , Css.width  (Css.pct 100)
        , Css.borderRadius (Css.px 2)
        ]
      ]
      [ div
        [ css
          [ Css.displayFlex
          , Css.flexDirection Css.row
          ]
        ]
        [a
          [ href entry.data.link
          , onClick (SeenEntry entry.data f)
          , on "auxclick" <| Json.Decode.succeed <| SeenEntry entry.data f
          , css
            [ Css.margin4 (Css.px 0) (Css.auto) (Css.px 0) (Css.px 0)
            ]
          ]
          [b [] [text entry.data.title]
          ]
        , div
          [ css
            [ Css.height (Css.px 25)
            , Css.fontSize (Css.px 30)
            , Css.textAlign (Css.right)
            , Css.displayFlex
            , Css.margin (Css.px 0)
            ]
          , onClick <| DeleteEntryButtonPressed entry.data f
          ]
          [ text "🞫"
          ]
        ]
      ]
    , div [] [text <| f.name ++ ", " ++ (displayTime entry.data.published)]
    ]

displayTime : Time.Posix -> String
displayTime t =
  let
    tz = Time.utc

    day = case Time.toWeekday tz t of
      Time.Mon -> "Mon"
      Time.Tue -> "Tue"
      Time.Wed -> "Wed"
      Time.Thu -> "Thu"
      Time.Fri -> "Fri"
      Time.Sat -> "Sat"
      Time.Sun -> "Sun"

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

    pad str = if String.length str < 2 then "0" ++ str else str
    year = Time.toYear tz t |> String.fromInt |> pad
    h = Time.toHour tz t    |> String.fromInt |> pad
    m = Time.toMinute tz t  |> String.fromInt |> pad
    s = Time.toSecond tz t  |> String.fromInt |> pad
  in
  day ++ " " ++ month ++ " " ++ year ++ " " ++ h ++ ":" ++ m ++ ":" ++ s

------------------------------------------------------------
-------------------- Updating the model --------------------
------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AddFeedButtonPressed ->
      ({ model | popup = AddFeedPopup }, Cmd.none)

    ClosePopupPressed ->
      ({ model | popup = None }, Cmd.none)

    EditedAddFeedName s ->
      ({ model | addFeedName = s}, Cmd.none)

    EditedAddFeedUrl s ->
      ({ model | addFeedUrl = s}, Cmd.none)

    SubmitAddFeed -> submitAddFeed model

    ReceivedSubscribedFeeds r ->
      case r of
        Ok l  -> 
          let newFeeds = List.map (\f -> Feed f []) l
          in
              ({ model | feeds = newFeeds }, Cmd.map ApiMessage <| refreshFeeds <| List.map .data newFeeds)
        Err _ -> (model, Cmd.none)

    DeleteFeedButtonPressed ->
      ({ model | deleteFeedMode = not model.deleteFeedMode}, Cmd.none)

    SelectFeed s ->
      ({ model | selectedFeed = s }, Cmd.none)

    DeleteFeed feed -> 
      ({ model | feeds = 
          List.filter (\f -> f.data /= feed) model.feeds}
      , Cmd.map ApiMessage (unSubscribeFeed feed))

    SeenEntry entryData parent ->
      ({model | feeds = List.map (\f -> if f.data == parent
        then {f | entries = 
          List.map (\e -> if e.data == entryData
          then {e | seen = True }
          else e) f.entries
        }
        else f) model.feeds
      }, Cmd.map ApiMessage (updateSeen entryData))

    DeleteEntryButtonPressed entryData parent -> 
      ({ model | feeds = List.map (\f -> if not (f.data == parent)
      then f
      else { f | entries = List.filter (\e -> e.data /= entryData) f.entries }
      ) model.feeds }, Cmd.map ApiMessage (updateSeen entryData))

    ApiMessage m ->
      case m of
        Entries (Ok entries) -> ({model | feeds = updateEntries entries model.feeds}, Cmd.none)
        Entries (Err _)      -> (model, Cmd.none)
        NoVal _              -> (model, Cmd.none)

    SettingsButtonPressed -> 
      ({ model | popup = SettingsPopup }, Cmd.none)

    SubscriptionsJsonClicked ->
      (model, Cmd.none)

    RefreshButtonPressed  ->
      (model,
        List.map .data model.feeds
        |> refreshFeeds
        |> Cmd.map ApiMessage)

    NewEntries _          -> (model, Cmd.none)


addFeeds : List Feed -> List FeedData -> List Feed
addFeeds old new =
  let oldDatas = List.map .data old
      toAdd = List.filter (\e -> not <| List.member e oldDatas) new
      asFeeds = List.map (\d -> Feed d []) toAdd
  in
  asFeeds ++ old


submitAddFeed : Model -> (Model, Cmd Msg)
submitAddFeed model =
  let newFeed = FeedData model.addFeedUrl model.addFeedName
      oldList = List.map .data model.feeds
  in 
    if not (List.member newFeed oldList)
    then 
      ({ model | feeds = (Feed newFeed []) :: model.feeds
               , addFeedName = ""
               , addFeedUrl  = ""
      }, Cmd.map ApiMessage (registerFeed newFeed))
    else ( model, Cmd.none )


updateEntries : List EntryData -> List Feed -> List Feed
updateEntries entries feeds =
  let sameFeed feed entryData = entryData.feed == feed.data.url
      updateFn entries2 feed =
        { feed | entries = List.filter (sameFeed feed) entries2
          |> List.map (\e -> Entry e False)}
  in
  List.map (updateFn entries) feeds

------------------------------------------------------------
--------------------------- Init ---------------------------
------------------------------------------------------------
init : () -> ( Model, Cmd Msg )
init _ =
    ( default_model
    , Http.get
        { url = "/subscriptions"
        , expect = Http.expectJson ReceivedSubscribedFeeds feedDataListDecoder
        }
    )
