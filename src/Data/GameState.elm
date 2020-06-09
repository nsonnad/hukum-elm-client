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
    | WaitingForFirstCard
    | WaitingForTrump
    | PlayingHand
    | GameOver


type Choice
    = Call
    | Pass
    | Loner


type PlayerAction
    = ChooseTeam Int
    | ChooseCallOrPass Choice
    | CallTrump Suit
    | PlayCard Card


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


type alias GameState =
    { id : String
    , stage : Stage
    , players : List Player
    , score : Score
    , turn : String
    , dealer : String
    , currentTrick : List Card
    , handTrickWinners : List Int
    , callingTeam : Maybe Int
    , suitLed : Suit
    , suitTrump : Suit
    }


gameStateDecoder : JD.Decoder GameState
gameStateDecoder =
    JD.succeed GameState
        |> required "id" JD.string
        |> custom (JD.field "stage" JD.string |> JD.andThen stageDecoder)
        |> required "players" playersDecoder
        |> required "score" scoreDecoder
        |> required "turn" JD.string
        |> required "dealer" JD.string
        |> custom (JD.field "current_trick" (JD.list cardDecoder))
        |> required "hand_trick_winners" (JD.list JD.int)
        |> required "calling_team" (JD.nullable JD.int)
        |> custom (JD.field "suit_led" JD.string |> JD.andThen suitDecoder)
        |> custom (JD.field "suit_trump" JD.string |> JD.andThen suitDecoder)


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

        "waiting_for_first_card" ->
            JD.succeed WaitingForFirstCard

        "waiting_for_trump" ->
            JD.succeed WaitingForTrump

        "playing_hand" ->
            JD.succeed PlayingHand

        "game_over" ->
            JD.succeed GameOver

        _ ->
            JD.fail ("invalid stage: " ++ stage)



-- helpers


choiceToString : Choice -> String
choiceToString choice =
    case choice of
        Call ->
            "calling"

        Pass ->
            "pass"

        Loner ->
            "loner"
