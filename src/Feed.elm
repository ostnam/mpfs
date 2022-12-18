module Feed exposing (..)

import Browser exposing (Document)
import Css
import Decoders exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import Types exposing (..)


type alias Feed =
    { data : FeedData
    , entries : List Entry
    }


type alias Model =
    { feeds : List Feed
    }


type Msg
    = AddFeed String String
    | DeleteFeed
    | NewEntries (List Entry)
    | AddFeedButtonPressed
    | DeleteFeedButtonPressed
    | SettingsButtonPressed
    | RefreshButtonPressed
    | ReceivedSubscribedFeeds (Result Http.Error (List FeedData))


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


default_model : Model
default_model =
    Model []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Document Msg
view msg =
    Document "MPFS" [ Html.Styled.toUnstyled (view_body msg) ]


view_body : Model -> Html.Styled.Html Msg
view_body model =
    div
        [ id "main"
        , css
            [ Css.fontFamily Css.sansSerif
            , Css.fontSize (Css.px 20)
            ]
        ]
        [ render_left_bar model.feeds
        , render_entries model
        ]


render_left_bar : List Feed -> Html Msg
render_left_bar l =
    div [ id "leftbar" ]
        [ render_options
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
            , onClick AddFeedButtonPressed
            ]
            [ img [ src "static/images/refresh.png", css option_img_style ] [] ]
        , div [ css option_button_style ] [ img [ src "static/images/add_feed.png", css option_img_style ] [] ]
        , div [ css option_button_style ] [ img [ src "static/images/remove_feed.png", css option_img_style ] [] ]
        , div [ css option_button_style ] [ img [ src "static/images/settings.png", css option_img_style ] [] ]
        ]


render_entries : Model -> Html Msg
render_entries x =
    div [] [ text "yo" ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( default_model
    , Http.get
        { url = "/subscriptions"
        , expect = Http.expectJson ReceivedSubscribedFeeds feedDataListDecoder
        }
    )
