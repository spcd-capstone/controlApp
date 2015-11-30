module Pages.Helper where

import Json.Decode exposing (Decoder)
import Task exposing (Task, andThen)
import Signal exposing (Address)
import Http exposing (getString, post, string)


getHttpGetTask : Address a -> (String -> a) -> String -> Task String ()
getHttpGetTask address fn url =
    Task.mapError (always "Not Found") (getString url)
        |> Task.map fn
        |> flip andThen ( Signal.send address )


getHttpPostTask : Address a -> Decoder a -> String -> String -> Task String ()
getHttpPostTask address decoder url body =
    Task.mapError (always "Error Posting") (post decoder url (string body))
        |> flip andThen ( Signal.send address )
