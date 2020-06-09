module View.GameTable exposing (..)

import Data.Cards exposing (Card)
import Data.GameState exposing (..)
import Data.SharedTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import View.Cards exposing (..)


playerWrapper : Player -> Html Msg -> Html Msg
playerWrapper player pView =
    div [ class "player-view" ]
        [ pView
        , div [ class "player-name" ] [ text player.name ]
        ]


playerView : Player -> Bool -> Html Msg
playerView player currentPlayer =
    case currentPlayer of
        True ->
            playerWrapper player (visibleHandView player.hand)

        False ->
            playerWrapper player (concealedHandView player.hand)


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


cardTableView : Session -> List Player -> Html Msg
cardTableView session players =
    let
        arrangedPlayers =
            currentPlayerFirst players session.userName
    in
    case arrangedPlayers of
        [ p1, p2, p3, p4 ] ->
            div [ class "card-table" ]
                [ div [ class "player-area" ]
                    [ div [ class "card-table-top" ] [ playerView p3 False ] ]
                , div [ class "card-table-center" ]
                    [ div [ class "card-table-left" ] [ playerView p2 False ]
                    , div [ class "card-table-play-area" ] [ text "Center" ]
                    , div [ class "card-table-right" ] [ playerView p4 False ]
                    ]
                , div [ class "player-area" ]
                    [ div [ class "card-table-bottom" ] [ playerView p1 True ] ]
                ]

        _ ->
            div [ class "card-table" ] [ text "Table" ]
