module Data.GameState exposing (..)

import Data.Cards exposing (..)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (custom, required)


type Stage
    = WaitingForPlayers
    | ChoosingTeams
    | CallOrPass


type alias Player =
    { name : String
    , team : Maybe Int
    , hand : List Card
    }


type alias Score =
    Dict String Int


type alias Trick =
    List ( String, Card )


type alias GameState =
    { id : String
    , stage : Stage
    , score : Score
    , players : List Player

    --, current_trick : Maybe Trick
    --, calling_team : Maybe Int
    --, suit_led : Maybe Suit
    --, suit_trump : Maybe Suit
    --, turn : Maybe String
    }


scoreDecoder : JD.Decoder Score
scoreDecoder =
    JD.dict JD.int


playersDecoder : JD.Decoder (List Player)
playersDecoder =
    JD.list playerDecoder


playerDecoder : JD.Decoder Player
playerDecoder =
    JD.succeed Player
        |> required "name" JD.string
        |> required "team" (JD.maybe JD.int)
        |> required "hand" (JD.list cardDecoder)


stageDecoder : String -> JD.Decoder Stage
stageDecoder stage =
    case stage of
        "waiting_for_players" ->
            JD.succeed WaitingForPlayers

        "choosing_teams" ->
            JD.succeed ChoosingTeams

        "call_or_pass" ->
            JD.succeed CallOrPass

        _ ->
            JD.fail ("invalid stage: " ++ stage)


gameStateDecoder : JD.Decoder GameState
gameStateDecoder =
    JD.succeed GameState
        |> required "id" JD.string
        |> custom (JD.field "stage" JD.string |> JD.andThen stageDecoder)
        |> required "score" scoreDecoder
        |> required "players" playersDecoder
