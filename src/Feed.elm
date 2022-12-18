module Feed exposing (..)

import Browser exposing (Document)
import Css
import Decoders exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Types exposing (..)
import Api exposing (..)


------------------------------------------------------------
---------------------- Defining types ----------------------
------------------------------------------------------------
type alias Feed =
    { data : FeedData
    , entries : List Entry
    }


type Popup
  = None
  | AddFeedPopup
  | SettingsPopup

type alias Model =
    { feeds : List Feed
    , popup : Popup
    , addFeedName : String
    , addFeedUrl : String
    }


default_model : Model
default_model =
    Model [] None "" ""


type Msg
    = AddFeed String String
    | EditedAddFeedName String
    | EditedAddFeedUrl String
    | SubmitAddFeed
    | DeleteFeed
    | NewEntries (List Entry)
    | AddFeedButtonPressed
    | DeleteFeedButtonPressed
    | SettingsButtonPressed
    | RefreshButtonPressed
    | ReceivedSubscribedFeeds (Result Http.Error (List FeedData))
    | ClosePopupPressed
    | ApiMessage ApiMessage


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
    SettingsPopup -> Document "MPFS" [ mainBody ]
    AddFeedPopup  -> Document "MPFS" [ mainBody, u (renderAddFeedPopup model) ]


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
        [ render_left_bar model.feeds
        , render_entries model
        ]


render_left_bar : List Feed -> Html Msg
render_left_bar feeds =
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
      renderTotal feeds
     ] ++ List.map renderFeedInLeftBar feeds)


renderTotal : List Feed -> Html Msg
renderTotal l =
  let totCount = List.map .entries l
              |> List.map List.length
              |> List.foldl (+) 0
  in 
    div [] [text (String.fromInt totCount ++ " entries")]


renderFeedInLeftBar : Feed -> Html Msg
renderFeedInLeftBar feed = 
  div [] [text feed.data.name]


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
        , div [ css option_button_style ] [ img [ src "static/images/remove_feed.png", css option_img_style ] [] ]
        , div [ css option_button_style ] [ img [ src "static/images/settings.png", css option_img_style ] [] ]
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


render_entries : Model -> Html Msg
render_entries _ =
    div [] [ text "yo" ]

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
        Ok l  -> ({ model | feeds = List.map (\f -> Feed f []) l}, Cmd.none)
        Err _ -> (model, Cmd.none)

    _ -> ( model, Cmd.none )


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
