module Pages.SchedulePage where

import Html exposing (..)
import Signal exposing (Address)
import Task exposing (Task, andThen)
import Http


type alias Model =
    { cat : String
    }


type Action
    = NoOp
    | ReqData (Task String ())
    | RespData String


initialModel = Model ""

initTask : Address Action -> Task String ()
initTask address =
    Task.mapError (always "Not Found") (Http.getString "/json/Scheduler.json")
        |> Task.map (RespData)
        |> flip andThen ( Signal.send address )

update : Action -> Model -> Model
update a m =
    case a of
        RespData s ->
            { m | cat = s }
        _ -> m


view : Address Action -> Model -> Html
view address model = text "Not implemented yet"

