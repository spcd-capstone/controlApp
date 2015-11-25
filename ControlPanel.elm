module ControlPanel where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String
import Task
import Window

import Pages.NodePage as NodePage
import Pages.ScriptPage as ScriptPage
import Pages.SchedulePage as SchedulePage
import Pages.LogPage as LogPage


---- MODEL ----

type Page
    = Nodes
    | Scripts
    | Schedule
    | Logs


type alias Model =
    { activePage : Page
    , resultStr : String
    , nodeModel : NodePage.Model
    , scriptModel : ScriptPage.Model
    , scheduleModel : SchedulePage.Model
    , logModel : LogPage.Model
    }


---- UPDATE ----

type Action
    = NoOp
    | ChangePage Page
    | CreatePageModel Page String
    | NodePageAction NodePage.Action
    | ScriptPageAction ScriptPage.Action
    | SchedulePageAction SchedulePage.Action
    | LogPageAction LogPage.Action


update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model

        ChangePage p ->
            { model |
                activePage = p
            }

        CreatePageModel p data -> case p of
            Nodes ->
                { model | nodeModel = NodePage.constructModel data }

            Scripts ->
                { model | scriptModel = ScriptPage.constructModel data }

            Schedule ->
                { model | scheduleModel = SchedulePage.constructModel data }

            Logs ->
                { model | logModel = LogPage.constructModel data }


        NodePageAction a ->
            { model |
                nodeModel = NodePage.update a model.nodeModel
            }

        ScriptPageAction a ->
            { model |
                scriptModel = ScriptPage.update a model.scriptModel
            }

        SchedulePageAction a ->
            { model |
                scheduleModel = SchedulePage.update a model.scheduleModel
            }

        LogPageAction a ->
            { model |
                logModel = LogPage.update a model.logModel
            }


---- VIEW ----

view : Address Action -> Model -> Html
view address model =
    let ssTag = "link"
        ssAttrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "/style.css"
            ]
        ssChildren = []
        stylesheet = node ssTag ssAttrs ssChildren
    in
        div [ id "page" ]
            [ stylesheet
            , div [ class "sidebar" ] <| getSidebarContents address model
            , div [ class "page-panel" ]
                [ div [ class "content" ]
                    [ getPageContent address model ]
                ]
            ]


getSidebarContents : Address Action -> Model -> List Html
getSidebarContents address model =
    let classes p = classList
            [ ("sidebar-item", True)
            , ("sidebar-item-selected", model.activePage == p)
            ]
        divFor p = div
            [ classes p
            , onClick address (ChangePage p)
            ]
            [ text <| pageTitles p ]
    in
        [ div [ class "sidebar-title" ] [ text "Control Panel" ]
        , divFor Nodes
        , divFor Scripts
        , divFor Schedule
        , divFor Logs
        ]


getPageContent : Address Action -> Model -> Html
getPageContent address model =
    case model.activePage of
        Nodes ->
            NodePage.view
                (Signal.forwardTo address NodePageAction)
                model.nodeModel

        Scripts ->
            ScriptPage.view
                (Signal.forwardTo address ScriptPageAction)
                model.scriptModel

        Schedule ->
            SchedulePage.view
                (Signal.forwardTo address SchedulePageAction)
                model.scheduleModel

        Logs ->
            LogPage.view
                (Signal.forwardTo address LogPageAction)
                model.logModel


pageTitles : Page -> String
pageTitles page =
    case page of
        Nodes -> "Nodes"
        Scripts -> "Run Script"
        Schedule -> "Scheduler"
        Logs -> "Logs"


---- WIRING ----

main : Signal Html
main = Signal.map (view actionsMailbox.address) model


model = Signal.foldp update initialModel actionsMailbox.signal


initialModel =
    { activePage = Nodes
    , resultStr = ""
    , nodeModel = NodePage.initialModel
    , scriptModel = ScriptPage.initialModel
    , scheduleModel = SchedulePage.initialModel
    , logModel = LogPage.initialModel
    }


actionsMailbox : Signal.Mailbox Action
actionsMailbox = Signal.mailbox (ChangePage Nodes)


port requests : Signal (Task.Task String ())
port requests =
    let filt act =
            case act of
                ChangePage p -> Just p
                _ -> Nothing
    in
        Signal.filterMap filt Nodes actionsMailbox.signal
            |> Signal.map
                (\ page -> Task.andThen
                    (getQueryTask page)
                    (Signal.send actionsMailbox.address)
                )


getQueryTask : Page -> Task.Task String Action
getQueryTask page =
    let toUrl = "/json/" ++ pageTitles page ++ ".json"
    in
        Task.mapError (always "Not Found") (Http.getString toUrl)
            |> Task.map (CreatePageModel page)

