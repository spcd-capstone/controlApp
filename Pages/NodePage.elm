module Pages.NodePage where

import Date
import Date exposing (Date)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD
import Json.Decode exposing ((:=))
import Signal exposing (Address, message)
import String
import Task exposing (Task, andThen)

import Pages.Helper exposing (getHttpGetTask, getHttpPostTask)


---- MODEL ----

type alias HANode =
    { id : Int
    , name : String
    , ipAddress : String
    , nodeType : HANodeType
    , lastUpdated : Date
    }


type HANodeType
    = OtherNode
    | ToggleNode
    | RGBLightNode
    | TempSensorNode
    | ThermostatNode


type alias Model =
    { nodes : List HANode
    , extra : String
    , renaming : Maybe (Int, String)
    }


initialModel = Model [] "" Nothing


---- UPDATE ----

type Action
    = NoOp
    | ReqData (Task String ())
    | RespNodeList String

{- Actions for renaming nodes. One to begin renaming, one for key press each
keypress, and finally one for sending POST request -}
    | OpenRename Int
    | UpdateName Int String
    | CloseRename
    | RenameNodeSent

    | RespExtraData String


update : Action -> Model -> Model
update a m =
    case a of
        NoOp -> m
        ReqData _ -> m

        RespNodeList s ->
            { m |
                nodes = decodeNodeList s,
                renaming = Nothing
            }

        OpenRename id ->
            { m | renaming = Just (id, "") } -- TODO: change this to begin with old name

        UpdateName id str ->
            { m | renaming = Just (id, str) }

        CloseRename ->
            { m | renaming = Nothing }

        RenameNodeSent ->
            { m | renaming = Nothing }

        RespExtraData s ->
            { m | extra = s }


---- VIEW ----

view : Address Action -> Model -> Html
view address model = div [ class "nodes-page" ]
    [ h1 [] [ text "Nodes" ]
    , renderNodeTable address model
    , br [] []
    , button [ onClick address <| ReqData (getExtraData address) ] [ text "more" ]
    , br [] []
    , text model.extra
    ]


renderNodeTable : Address Action -> Model -> Html
renderNodeTable address model = table []
    [ thead []
        [ th [] [ text "Node Name" ]
        , th [] [ text "Type" ]
        , th [] [ text "IP Address" ]
        , th [] [ text "Last Updated" ]
        , th [] []
        ]
    , tbody [] <| List.map (renderNodeTableRow address model.renaming) model.nodes
    ]


renderNodeTableRow : Address Action -> Maybe (Int, String) -> HANode -> Html
renderNodeTableRow address renaming node =
    let default = text node.name
        nameField = case renaming of
            Nothing -> default
            Just (i, str) -> if i /= node.id
                then default
                else input
                    [ placeholder node.name
                    , autofocus True
                    , value str
                    , name "newName"
                    , on "input" targetValue (Signal.message address << UpdateName  node.id)
                    , onBlur address (CloseRename)
                    , onEnter address (ReqData (renameNode address i str))
                    ] []
    in
        tr []
            [ td [] [ nameField ]
            , td [] [ text <| nodeTypeToString node.nodeType ]
            , td [] [ text node.ipAddress ]
            , td [] [ text <| renderDate node.lastUpdated ]
            , td []
                [ text "["
                , a [ onClick address (OpenRename node.id) ] [ text "rename" ]
                , text "]"
                ]
            ]


---- API REQUESTS ----

initTask : Address Action -> Task String ()
initTask address =
    getHttpGetTask address RespNodeList "/json/Nodes.json"


getExtraData : Address Action -> Task String ()
getExtraData address =
    getHttpGetTask address RespExtraData "/json/Nodes.extra.json"


renameNode : Address Action -> Int -> String -> Task String ()
renameNode address id newName =
    let decoder = (JD.succeed RenameNodeSent)
        url = "/json/nodes/" ++ ( toString id )
        body = "{ \"name\": \"" ++ newName ++ "\" }"
        sendRenameReq = getHttpPostTask address decoder url body
    in
        Task.andThen sendRenameReq (\ _ -> initTask address)


---- JSON DECODERS ----

decodeNodeList : String -> List HANode
decodeNodeList =
    let dateParser = Date.fromString
            >> Result.withDefault (Date.fromTime 0)
        decodeId = ("id" := JD.int)
        decodeName = ("name" := JD.string)
        decodeIP = ("ip" := JD.string)
        decodeLastUpdated = JD.object1 dateParser ("last_updated" := JD.string)
        decodeType = JD.object1 nodeTypeParse ("type" := JD.string)
        decodeNode = (JD.object5 HANode decodeId decodeName decodeIP decodeType decodeLastUpdated)
        decodeNodeList = ("node_list" := JD.list decodeNode)
    in
        Result.withDefault [] << JD.decodeString decodeNodeList


---- HELPER FUNCTIONS ----

onEnter : Address a -> a -> Attribute
onEnter address value =
    let is13 c = if c == 13 then Ok () else Err "Not enter"
    in
        on "keydown"
            (JD.customDecoder keyCode is13)
            (\_ -> Signal.message address value)


renderDate : Date -> String
renderDate date =
    let y = toString <| Date.year date
        m = toString <| Date.month date
        d = toString <| Date.day date
        h = toString <| Date.hour date
        min = toString <| Date.minute date
        s = toString <| Date.second date
    in
        String.join "-" [y, m, d]
            ++ " "
            ++ String.join ":" [h, min, s]


nodeTypeLUT : List (HANodeType, String)
nodeTypeLUT =
    [ (OtherNode, "other")
    , (ToggleNode, "toggle")
    , (RGBLightNode, "light_rgb")
    , (TempSensorNode, "sensor_temp")
    , (ThermostatNode, "thermostat")
    ]


nodeTypeToString : HANodeType -> String
nodeTypeToString nt =
    let fn tup def = if (fst tup) == nt then snd tup else def
    in
        List.foldr fn "other" nodeTypeLUT


nodeTypeParse : String -> HANodeType
nodeTypeParse nts =
    let nodeDict = Dict.fromList <| List.map tupleSwap nodeTypeLUT
    in
        Dict.get nts nodeDict
            |> Maybe.withDefault OtherNode


tupleSwap : (a, b) -> (b, a)
tupleSwap (a, b) = (b, a)

