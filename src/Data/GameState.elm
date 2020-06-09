module Data.GameState exposing (..)

import Data.Cards exposing (..)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as JE


type Stage
    = WaitingForPlayers
    | ChoosingTeams
    | CallOrPass


type Choice
    = Call
    | Pass
    | Loner


type PlayerAction
    = ChooseTeam Int
    | ChooseCallOrPass Choice


type Msg
    = GotGameState JE.Value
    | Action PlayerAction


type alias Player =
    { name : String
    , team : Maybe Int
    , hand : List Card
    }


type alias Score =
    Dict String Int


type alias Trick =
    List PlayedCard


type alias GameState =
    { id : String
    , stage : Stage
    , score : Score
    , players : List Player
    , dealer : String
    , turn : String
    , current_trick : List PlayedCard
    , calling_team : Maybe Int
    , suit_led : Suit
    , suit_trump : Suit
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
        |> required "turn" JD.string
        |> required "dealer" JD.string
        |> custom (JD.field "current_trick" (JD.list playedCardDecoder))
        |> required "calling_team" (JD.nullable JD.int)
        |> custom (JD.field "suit_trump" JD.string |> JD.andThen suitDecoder)
        |> custom (JD.field "suit_led" JD.string |> JD.andThen suitDecoder)


playedCardDecoder : JD.Decoder PlayedCard
playedCardDecoder =
    arrayAsTuple2 JD.string cardDecoder



-- helpers


choiceToString : Choice -> String
choiceToString choice =
    case choice of
        Call ->
            "call"

        Pass ->
            "pass"

        Loner ->
            "loner"
