module Pages.ScriptPage where

import Dict exposing (Dict)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode exposing ((:=))
import Signal exposing (Address)
import String
import Task exposing (Task, andThen)
import Http

import Pages.Helper exposing (getHttpGetTask, getHttpPostTask)


---- MODEL ----

type alias ScriptLine =
    { id : Int
    , name : String
    , selectedTarget : String
    , params : String
    }

type alias Model =
    { nodeList : List String
    , scriptDict : Dict Int ScriptLine
    }


initialModel = Model [] Dict.empty


---- UPDATE ----

type Action
    = NoOp
    | ReqData (Task String ())
    | RespNodeList String
    | RespScriptList String
    | RespScriptExec String

    | TargetNodeChanged Int String
    | ParamTextEntered Int String


update : Action -> Model -> Model
update a m =
    case a of
        NoOp -> m
        ReqData _ -> m

        RespNodeList resp ->
            {m | nodeList = decodeNodeList resp }

        RespScriptList resp ->
            let makeDict = decodeScriptList (Maybe.withDefault "" (List.head m.nodeList)) resp
                    |> List.map (\ nd -> (nd.id, nd))
                    |> Dict.fromList
            in
                { m | scriptDict = makeDict }

        RespScriptExec resp ->
            m

        TargetNodeChanged i str ->
            let update mval = case mval of
                    Just val -> Just {val | selectedTarget = str}
                    Nothing -> Nothing
                newDict = Dict.update i update m.scriptDict
            in
                { m | scriptDict = newDict }

        ParamTextEntered i str ->
            let update mval = case mval of
                    Just val -> Just { val | params = str }
                    Nothing -> Nothing
                newDict = Dict.update i update m.scriptDict
            in
                { m | scriptDict = newDict }



---- VIEW ----

view : Address Action -> Model -> Html
view address model = div [ class "scripts-page" ]
    [ h1 [] [ text "Scripts" ]
    , renderScriptTable address model
    ]

renderScriptTable : Address Action -> Model -> Html
renderScriptTable address model =
    let rows = Dict.toList model.scriptDict
            |> List.map snd
            |> List.map (renderScriptTableRow address model.nodeList)
    in
        table []
            [ thead []
                [ th [] [ text "Script Name" ]
                , th [] [ text "Target Node" ]
                , th [] [ text "Params" ]
                , th [] []
                ]
            , tbody [] rows
            ]


renderScriptTableRow : Address Action -> List String -> ScriptLine -> Html
renderScriptTableRow address nodes scriptLine =
    let clickAction str = onClick address (TargetNodeChanged scriptLine.id str)
        paramInputAction = on "input" targetValue (Signal.message address << ParamTextEntered scriptLine.id)
        runClicked = onClick address (ReqData (runScript address scriptLine.id scriptLine.selectedTarget scriptLine.params))
    in
        tr []
            [ td [] [ text scriptLine.name ]
            , td []
                [ select []
                    <| List.map (\ n -> option [ clickAction n ] [ text n ] ) nodes
                ]
            , td []
                [ input [ paramInputAction ] []
                ]
            , td []
                [ text "["
                , a [ runClicked ] [ text "run" ]
                , text "]"
                ]
            ]

---- API REQUESTS ----

initTask : Address Action -> Task String ()
initTask address =
    Task.andThen
        (getHttpGetTask address RespNodeList "/api/nodes")
        (always <| getHttpGetTask address RespScriptList "/api/scripts/list")


runScript : Address Action -> Int -> String -> String -> Task String ()
runScript address id node params =
    let decoder = (JD.succeed <| RespScriptExec node)
        url = "/api/scripts/exec/id/" ++ toString id
        paramList = node :: (String.split " " params)
            |> List.map JE.string
            |> JE.list
        body = JE.object [ ("params", paramList) ]
            |> JE.encode 0
    in
        getHttpPostTask address decoder url body


---- JSON DECODERS ----

decodeNodeList : String -> List String
decodeNodeList =
    let decodeName = ("name" := JD.string)
        decodeNodeList = ("node_list" := JD.list decodeName)
    in
        Result.withDefault [] << JD.decodeString decodeNodeList


decodeScriptList : String -> String -> List ScriptLine
decodeScriptList fstNode =
    let decodeId = ("id" := JD.int)
        decodeName = ("name" := JD.string)
        decodeScript = JD.object4 ScriptLine decodeId decodeName (JD.succeed fstNode) (JD.succeed "")
        decodeScriptList = ("script_list" := JD.list decodeScript)
    in
        Result.withDefault [] << JD.decodeString decodeScriptList


---- HELPER FUNCTIONS ----





