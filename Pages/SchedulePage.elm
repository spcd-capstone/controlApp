module Pages.SchedulePage where

import Html exposing (..)
import Signal exposing (Address)

type alias Model =
    { cat : String
    }


type Action
    = NoOp


initialModel = Model ""


constructModel : String -> Model
constructModel _ = initialModel


update : Action -> Model -> Model
update a m = m


view : Address Action -> Model -> Html
view address model = text ("Schedule")

