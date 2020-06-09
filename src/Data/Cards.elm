module Data.Cards exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as JE


type alias Card =
    { rank : Rank
    , suit : Suit
    }


type alias PlayedCard =
    { rank : Rank
    , suit : Suit
    , player : String
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
    | Undecided


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

        "undecided" ->
            JD.succeed Undecided

        _ ->
            JD.fail ("invalid suit: " ++ suit)


cardDecoder : JD.Decoder Card
cardDecoder =
    JD.succeed Card
        |> custom (JD.field "rank" JD.string |> JD.andThen rankDecoder)
        |> custom (JD.field "suit" JD.string |> JD.andThen suitDecoder)


playedCardDecoder : JD.Decoder PlayedCard
playedCardDecoder =
    JD.succeed PlayedCard
        |> custom (JD.field "rank" JD.string |> JD.andThen rankDecoder)
        |> custom (JD.field "suit" JD.string |> JD.andThen suitDecoder)
        |> required "player" JD.string


encodeCard : Card -> JE.Value
encodeCard { rank, suit } =
    let
        rankVal =
            String.toLower (rankToString rank)

        suitVal =
            String.toLower (suitToString suit)
    in
    JE.object
        [ ( "rank", JE.string rankVal )
        , ( "suit", JE.string suitVal )
        ]


cardToString : Card -> String
cardToString { rank, suit } =
    rankToString rank ++ "_of_" ++ suitToString suit


rankToString : Rank -> String
rankToString rank =
    case rank of
        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Jack ->
            "Jack"

        Queen ->
            "Queen"

        King ->
            "King"

        Ace ->
            "Ace"


suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs ->
            "Clubs"

        Diamonds ->
            "Diamonds"

        Hearts ->
            "Hearts"

        Spades ->
            "Spades"

        Undecided ->
            "undecided"
