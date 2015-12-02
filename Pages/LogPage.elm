module Pages.LogPage where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import String
import Task exposing (Task, andThen)
import Http
import Json.Decode as JD
import Json.Decode exposing ((:=))

import Pages.Helper exposing (getHttpGetTask, getHttpPostTask)

---- MODEL ----

type alias ScriptLogEntry =
    { id : Int
    , timestamp : String
    , threadId : Int
    , name : String
    , params : List String
    , entryType : String
    , data : String
    }


type alias Model =
    { entries : List ScriptLogEntry
    , threadIdFilter : Maybe Int
    }


initialModel = Model [] Nothing


---- UPDATE ----

type Action
    = NoOp
    | ReqData (Task String ())
    | RespScriptEntryList String
    | SetThreadIdFilter (Maybe Int)


update : Action -> Model -> Model
update a m = case a of
    NoOp -> m
    ReqData _ -> m

    RespScriptEntryList data ->
        { m | entries = decodeLogEntryList data }

    SetThreadIdFilter i ->
        { m | threadIdFilter = i }


---- VIEW ----

view : Address Action -> Model -> Html
view address model =
    let tid = case model.threadIdFilter of
            Nothing -> ""
            Just i -> toString i
    in
        div [ class "log-page" ]
            [ h1 [] [ text "Logs" ]
            , select [] [ option [] [ text "Script Logs" ] ]
            , input
                [ id "thread-id-filter"
                , placeholder "Thread ID"
                , value tid
                , on "input" targetValue (Signal.message address << SetThreadIdFilter << Result.toMaybe << String.toInt)
                ] []
            , text "["
            , a [ onClick address (SetThreadIdFilter Nothing) ] [ text "Clear" ]
            , text "]"
            , text " | "
            , text "["
            , a [ onClick address (ReqData <| initTask address) ] [ text "Refresh" ]
            , text "]"
            , renderLogTable address model
            , br [] []
            ]


renderLogTable : Address Action -> Model -> Html
renderLogTable address model =
    let entryList = case model.threadIdFilter of
            Nothing -> model.entries
            Just i -> List.filter (\ x -> x.threadId == i) model.entries
    in
        table []
            [ thead []
                [ th [] [ text "Thread ID" ]
                , th [] [ text "Time" ]
                , th [] [ text "Type" ]
                , th [] [ text "Script Name" ]
                , th [] [ text "Params" ]
                , th [] [ text "Data" ]
                ]
            , tbody []
                <| List.map (renderLogEntry address) entryList
            ]


renderLogEntry : Address Action -> ScriptLogEntry -> Html
renderLogEntry address model = tr []
    [ td [] [ a [ onClick address <| SetThreadIdFilter (Just model.threadId) ] [ text <| toString model.threadId ] ]
    , td [] [ text model.timestamp ]
    , td [] [ text model.entryType ]
    , td [] [ text model.name ]
    , td [] [ text <| toString model.params ]
    , td [] [ text model.data ]
    ]


---- API REQUESTS ----

initTask : Address Action -> Task String ()
initTask address =
    getHttpGetTask address RespScriptEntryList "/api/logs/script"


---- JSON DECODERS ----

decodeLogEntryList : String -> List ScriptLogEntry
decodeLogEntryList =
    let decodeId = ("id" := JD.int)
        decodeTimestamp =  ("timestamp" := JD.string)
        decodeThreadId = ("thread_id" := JD.int)
        decodeName = ("script_name" := JD.string)
        decodeParams = ("params" := JD.list JD.string)
        decodeType = ("entry_type" := JD.string)
        decodeData = ("data" := JD.string)
        decodeEntry = JD.object7 ScriptLogEntry
            decodeId decodeTimestamp decodeThreadId decodeName decodeParams
                decodeType decodeData
        decodeEntryList = ("entry_list" := JD.list decodeEntry)
    in
        Result.withDefault [] << JD.decodeString decodeEntryList
