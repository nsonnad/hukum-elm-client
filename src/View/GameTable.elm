module View.GameTable exposing (..)

import Data.Cards exposing (..)
import Data.GameState exposing (..)
import Data.SharedTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import View.Cards exposing (..)


playerView : Player -> String -> Bool -> Html Msg
playerView player turn currentPlayer =
    let
        isTurn =
            turn == player.name
    in
    case ( currentPlayer, isTurn ) of
        ( True, True ) ->
            div [ class "player-view" ]
                [ span [ class "dot turn-dot" ] []
                , activeHandView (\c -> Action (PlayCard c)) player.hand
                , div [ class "player-name" ] [ text player.name ]
                ]

        ( True, False ) ->
            div [ class "player-view" ]
                [ visibleHandView player.hand
                , div [ class "player-name" ] [ text player.name ]
                ]

        ( False, True ) ->
            div [ class "player-view" ]
                [ span [ class "dot turn-dot" ] []
                , concealedHandView player.hand
                , div [ class "player-name" ] [ text player.name ]
                ]

        ( False, False ) ->
            div [ class "player-view" ]
                [ concealedHandView player.hand
                , div [ class "player-name" ] [ text player.name ]
                ]


currentPlayerFirst : List Player -> String -> List Player
currentPlayerFirst players userName =
    case players of
        p :: rest ->
            if p.name == userName then
                [ p ] ++ rest

            else
                currentPlayerFirst (rest ++ [ p ]) userName

        [] ->
            []


isUserPlayer : List Player -> String -> Bool
isUserPlayer players userName =
    case players of
        p :: rest ->
            if p.name == userName then
                True

            else
                isUserPlayer rest userName

        [] ->
            False


playedCardsView : List Card -> Stage -> Html Msg
playedCardsView cards stage =
    case ( cards, stage ) of
        ( [ a ], WaitingForTrump ) ->
            div [ class "current-trick" ]
                (List.map (cardBackView []) [ a ])

        _ ->
            div [ class "current-trick" ]
                (List.map (cardFrontView []) (List.reverse cards))



-- TODO: refactor this hack to allow observers


cardTableView : Session -> GameState -> Html Msg
cardTableView session gs =
    case isUserPlayer gs.players session.userName of
        True ->
            let
                arrangedPlayers =
                    currentPlayerFirst gs.players session.userName
            in
            case arrangedPlayers of
                [ p1, p2, p3, p4 ] ->
                    div [ class "card-table" ]
                        [ div [ class "player-area" ]
                            [ div [ class "card-table-top" ]
                                [ playerView p3 gs.turn False ]
                            ]
                        , div [ class "card-table-center" ]
                            [ div [ class "card-table-left" ]
                                [ playerView p2 gs.turn False ]
                            , div [ class "card-table-play-area" ]
                                [ playedCardsView gs.currentTrick gs.stage ]
                            , div [ class "card-table-right" ]
                                [ playerView p4 gs.turn False ]
                            ]
                        , div [ class "player-area" ]
                            [ div [ class "card-table-bottom" ]
                                [ playerView p1 gs.turn True ]
                            ]
                        ]

                _ ->
                    div [ class "card-table" ] [ text "Table" ]

        False ->
            case gs.players of
                [ p1, p2, p3, p4 ] ->
                    div [ class "card-table" ]
                        [ div [ class "player-area" ]
                            [ div [ class "card-table-top" ]
                                [ playerView p3 gs.turn False ]
                            ]
                        , div [ class "card-table-center" ]
                            [ div [ class "card-table-left" ]
                                [ playerView p2 gs.turn False ]
                            , div [ class "card-table-play-area" ]
                                [ playedCardsView gs.currentTrick gs.stage ]
                            , div [ class "card-table-right" ]
                                [ playerView p4 gs.turn False ]
                            ]
                        , div [ class "player-area" ]
                            [ div [ class "card-table-bottom" ]
                                [ playerView p1 gs.turn False ]
                            ]
                        ]

                _ ->
                    div [ class "card-table" ] [ text "Table" ]


countedTricksToStr : List Int -> Int -> String
countedTricksToStr tricks team =
    tricks
        |> List.filter (\t -> t == team)
        |> List.length
        |> String.fromInt


maybeIntToStr : Maybe Int -> String
maybeIntToStr i =
    case i of
        Just x ->
            String.fromInt x

        Nothing ->
            "N/A"
