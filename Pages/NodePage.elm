module Pages.NodePage where

import Html exposing (..)
import Signal exposing (Address)

type alias HANode =
    { name : String
    , ipAddress : String
    }

type alias Model =
    { nodes : List HANode
    }


type Action
    = NoOp
    | RefreshData


initialModel = Model []


constructModel : String -> Model
constructModel _ = initialModel


update : Action -> Model -> Model
update a m = m


view : Address Action -> Model -> Html
view address model = text ("Nodes")
