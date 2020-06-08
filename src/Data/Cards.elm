module Data.Cards exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as JE


type alias Card =
    { rank : Rank
    , suit : Suit
    }


type Rank
    = Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


rankDecoder : String -> JD.Decoder Rank
rankDecoder rank =
    case rank of
        "seven" ->
            JD.succeed Seven

        "eight" ->
            JD.succeed Eight

        "nine" ->
            JD.succeed Nine

        "ten" ->
            JD.succeed Ten

        "jack" ->
            JD.succeed Jack

        "queen" ->
            JD.succeed Queen

        "king" ->
            JD.succeed King

        "ace" ->
            JD.succeed Ace

        _ ->
            JD.fail ("invalid rank: " ++ rank)


suitDecoder : String -> JD.Decoder Suit
suitDecoder suit =
    case suit of
        "clubs" ->
            JD.succeed Clubs

        "diamonds" ->
            JD.succeed Diamonds

        "hearts" ->
            JD.succeed Hearts

        "spades" ->
            JD.succeed Spades

        _ ->
            JD.fail ("invalid suit: " ++ suit)


cardDecoder : JD.Decoder Card
cardDecoder =
    JD.succeed Card
        |> custom (JD.field "rank" JD.string |> JD.andThen rankDecoder)
        |> custom (JD.field "suit" JD.string |> JD.andThen suitDecoder)
