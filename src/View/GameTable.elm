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
    -- todo: truth table of (currentPlayer, isMyTurn)
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


playedCardsView : List Card -> Stage -> Html Msg
playedCardsView cards stage =
    case ( cards, stage ) of
        ( [ a ], WaitingForTrump ) ->
            div [ class "current-trick" ]
                (List.map (cardBackView []) [ a ])

        _ ->
            div [ class "current-trick" ]
                (List.map (cardFrontView []) (List.reverse cards))


cardTableView : Session -> GameState -> Html Msg
cardTableView session gs =
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
