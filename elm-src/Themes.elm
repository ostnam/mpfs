module Themes exposing (..)

import Css
import Css.Media
import InterfaceTypes exposing (..)


mainTextColorDayMode : Css.Color
mainTextColorDayMode =
    Css.rgb 0 0 0


mainTextColorNightMode : Css.Color
mainTextColorNightMode =
    Css.rgb 237 227 187


altTextColorDayMode : Css.Color
altTextColorDayMode =
    Css.rgb 20 20 20


altTextColorNightMode : Css.Color
altTextColorNightMode =
    Css.rgb 219 202 129


getBgColor : Model -> Css.Color
getBgColor model =
    if model.nightMode then
        Css.rgb 32 32 32

    else
        Css.rgb 248 248 248


getTextColor : Model -> Css.Color
getTextColor model =
    if model.nightMode then
        mainTextColorNightMode

    else
        mainTextColorDayMode


getAltTextColor : Model -> Css.Color
getAltTextColor model =
    if model.nightMode then
        altTextColorNightMode

    else
        altTextColorDayMode


getHighlightColor : Model -> Css.Color
getHighlightColor model =
    if model.nightMode then
        Css.rgb 24 65 78

    else
        Css.rgb 173 216 230


getFeedNameColor : Model -> Css.Color
getFeedNameColor model =
    case ( model.nightMode, model.deleteFeedMode ) of
        ( True, True ) ->
            Css.rgb 204 16 66

        ( True, False ) ->
            mainTextColorNightMode

        ( False, True ) ->
            Css.rgb 255 0 0

        ( False, False ) ->
            mainTextColorDayMode


getEntryBgColor : Bool -> Model -> Css.Color
getEntryBgColor isSeen model =
    case ( model.nightMode, isSeen ) of
        ( True, True ) ->
            Css.rgb 72 72 72

        ( True, False ) ->
            Css.rgb 52 52 52

        ( False, True ) ->
            Css.rgb 217 216 216

        ( False, False ) ->
            Css.rgb 242 240 240


isDesktopMode : List Css.Media.MediaQuery
isDesktopMode =
    [ Css.Media.only Css.Media.screen
        [ Css.Media.minWidth (Css.px 1200)
        ]
    ]


isPhoneMode : List Css.Media.MediaQuery
isPhoneMode =
    [ Css.Media.only Css.Media.screen
        [ Css.Media.maxWidth (Css.px 1200)
        ]
    ]


mainBodyStyle : Model -> List Css.Style
mainBodyStyle model =
    [ Css.Media.withMedia isDesktopMode
        [ Css.fontFamily Css.sansSerif
        , Css.fontSize (Css.px 20)
        , Css.width (Css.pct 100)
        , Css.minHeight (Css.vh 100)
        , Css.backgroundColor <| getBgColor model
        , Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flex (Css.int 1)
        ]
    , Css.Media.withMedia isPhoneMode
        [ Css.fontFamily Css.sansSerif
        , Css.fontSize (Css.px 20)
        , Css.width (Css.pct 100)
        , Css.minHeight (Css.vh 100)
        , Css.backgroundColor <| getBgColor model
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.flex (Css.int 1)
        ]
    ]


leftBarStyle : Model -> List Css.Style
leftBarStyle _ =
    [ Css.Media.withMedia isDesktopMode
        [ Css.maxWidth (Css.px 400)
        , Css.minWidth (Css.px 170)
        , Css.width (Css.px 270)
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.property "user-select" "none"
        ]
    , Css.Media.withMedia isPhoneMode
        [ Css.width (Css.pct 100)
        , Css.displayFlex
        , Css.flexDirection Css.row
        , Css.property "user-select" "none"
        ]
    ]


totalEntriesStyle : Model -> List Css.Style
totalEntriesStyle model =
    let
        totalBgColor =
            case model.selectedFeed of
                All ->
                    [ Css.backgroundColor <| getHighlightColor model ]

                _ ->
                    []
    in
    [ Css.Media.withMedia isDesktopMode <|
        [ Css.padding4 (Css.px 0) (Css.px 0) (Css.px 0) (Css.px 10)
        , Css.borderStyle Css.hidden
        , Css.margin4 (Css.px 2) (Css.px 0) (Css.px 2) (Css.px 0)
        , Css.borderRadius (Css.px 5)
        , Css.property "user-select" "none"
        , Css.color <| getTextColor model
        ]
            ++ totalBgColor
    , Css.Media.withMedia isPhoneMode <|
        [ Css.padding4 (Css.px 0) (Css.px 0) (Css.px 0) (Css.px 10)
        , Css.borderStyle Css.hidden
        , Css.margin4 (Css.px 2) (Css.px 0) (Css.px 2) (Css.px 0)
        , Css.borderRadius (Css.px 5)
        , Css.property "user-select" "none"
        , Css.color <| getTextColor model
        , Css.textAlign Css.center
        , Css.flexGrow (Css.int 1)
        , Css.displayFlex
        , Css.alignItems Css.center
        , Css.justifyContent Css.center
        ]
            ++ totalBgColor
    ]


feedLeftBarStyle : Model -> Feed -> List Css.Style
feedLeftBarStyle model feed =
    let
        bgColor =
            case model.selectedFeed of
                All ->
                    []

                Single f ->
                    if feed.data == f then
                        [ Css.backgroundColor <| getHighlightColor model ]

                    else
                        []
    in
    [ Css.Media.withMedia isDesktopMode <|
        [ Css.padding4 (Css.px 0) (Css.px 2) (Css.px 0) (Css.px 4)
        , Css.margin4 (Css.px 0) (Css.px 2) (Css.px 4) (Css.px 4)
        , Css.borderWidth (Css.px 1)
        , Css.borderColor <| getTextColor model
        , Css.borderRadius (Css.px 5)
        , Css.borderStyle Css.solid
        , Css.overflow Css.hidden
        , Css.displayFlex
        , Css.flexDirection Css.row
        , Css.property "user-select" "none"
        ]
            ++ bgColor
    , Css.Media.withMedia isPhoneMode <|
        [ Css.display Css.none
        ]
    ]


entryStyle : Model -> Entry -> List Css.Style
entryStyle model entry =
    let
        bgColor =
            Css.backgroundColor <| getEntryBgColor entry.seen model
    in
    [ Css.Media.withMedia isDesktopMode <|
        [ Css.width (Css.px 1000)
        , Css.padding4 (Css.px 0) (Css.px 2) (Css.px 0) (Css.px 6)
        , Css.margin4 (Css.px 6) (Css.px 2) (Css.px 0) (Css.px 0)
        , Css.overflow Css.hidden
        , Css.outline Css.none
        , bgColor
        ]
    , Css.Media.withMedia isPhoneMode <|
        [ Css.maxWidth (Css.px 1000)
        , Css.padding4 (Css.px 0) (Css.px 2) (Css.px 0) (Css.px 6)
        , Css.margin4 (Css.px 6) (Css.px 2) (Css.px 0) (Css.px 0)
        , Css.overflow Css.hidden
        , Css.outline Css.none
        , bgColor
        ]
    ]
