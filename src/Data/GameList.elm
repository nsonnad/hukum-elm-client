module Data.GameList exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)


type alias GameList =
    List GameListGame


type alias GameListGame =
    { name : String
    , startedBy : String
    }


gameListDecoder : JD.Decoder (List GameListGame)
gameListDecoder =
    JD.list gameListGameDecoder


gameListGameDecoder : JD.Decoder GameListGame
gameListGameDecoder =
    JD.succeed GameListGame
        |> required "name" JD.string
        |> required "started_by" JD.string
