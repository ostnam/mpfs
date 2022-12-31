module Themes exposing (..)

import InterfaceTypes exposing (..)
import Css

mainTextColorDayMode : Css.Color
mainTextColorDayMode = Css.rgb 0 0 0

mainTextColorNightMode : Css.Color
mainTextColorNightMode = Css.rgb 237 227 187

altTextColorDayMode : Css.Color
altTextColorDayMode = Css.rgb 20 20 20

altTextColorNightMode : Css.Color
altTextColorNightMode = Css.rgb 219 202 129


getBgColor : Model -> Css.Color
getBgColor model =
    if model.nightMode
    then Css.rgb 32 32 32
    else Css.rgb 248 248 248


getTextColor : Model -> Css.Color
getTextColor model =
    if model.nightMode
    then mainTextColorNightMode
    else mainTextColorDayMode


getAltTextColor : Model -> Css.Color
getAltTextColor model =
    if model.nightMode
    then altTextColorNightMode
    else altTextColorDayMode

getHighlightColor : Model -> Css.Color
getHighlightColor model =
    if model.nightMode
    then Css.rgb 24 65 78
    else Css.rgb 173 216 230


getFeedNameColor : Model -> Css.Color
getFeedNameColor model =
    case (model.nightMode, model.deleteFeedMode) of
        (True, True)   -> Css.rgb 204 16 66
        (True, False)  -> mainTextColorNightMode
        (False, True)  -> Css.rgb 255 0 0
        (False, False) -> mainTextColorDayMode


getEntryBgColor : Bool -> Model -> Css.Color
getEntryBgColor isSeen model =
    case (model.nightMode, isSeen) of
        (True, True)   -> Css.rgb 72 72 72
        (True, False)  -> Css.rgb 52 52 52
        (False, True)  -> Css.rgb 217 216 216
        (False, False) -> Css.rgb 242 240 240
