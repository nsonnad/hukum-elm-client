port module Page.Game exposing (..)

import Data.GameState exposing (..)
import Data.SharedTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import View.GameTable exposing (..)


type alias Model =
    { gameState : Maybe GameState
    }


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        GotGameState gs ->
            case JD.decodeValue gameStateDecoder gs of
                Ok gameState ->
                    ( { model | gameState = Just gameState }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        Action action ->
            ( model, pushPlayerAction (encodeAction action) )


initModel : Model
initModel =
    { gameState = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


view : Session -> Model -> Html Msg
view session model =
    case model.gameState of
        Just gameState ->
            case gameState.stage of
                WaitingForPlayers ->
                    viewWrapper (viewWaitingForPlayers gameState)

                ChoosingTeams ->
                    viewWrapper (viewChooseTeams gameState)

                CallOrPass ->
                    viewWrapper (viewGameTable session gameState)

        Nothing ->
            h1 [] [ text "Game." ]


viewWrapper : Html Msg -> Html Msg
viewWrapper children =
    div [ class "page-wrapper" ] [ children ]


viewWaitingForPlayers : GameState -> Html Msg
viewWaitingForPlayers gameState =
    div []
        [ p []
            [ text "You're in the game "
            , strong [ class "game-name-inline" ] [ text gameState.id ]
            , text ". "
            , text "Waiting for other players to join, hang tight."
            ]
        , h4 [] [ text "Who's here so far:" ]
        , div [ class "row player-game-list" ]
            [ div [ class "column column-50 column-offset-25" ]
                [ table []
                    (List.concat
                        [ [ thead [] [ th [] [ text "Player" ] ] ]
                        , List.map (\p -> tr [] [ td [] [ text p.name ] ]) gameState.players
                        ]
                    )
                ]
            ]
        ]


viewUserList : Player -> Html Msg
viewUserList player =
    li [] [ text player.name ]


viewChooseTeams : GameState -> Html Msg
viewChooseTeams gameState =
    div [ class "choose-teams" ]
        [ h4 [] [ text "Everyone's here. It's time to pick teams." ]
        , div [ class "select-team-buttons" ]
            [ button [ onClick (Action (ChooseTeam 1)) ] [ text "Team One" ]
            , button [ onClick (Action (ChooseTeam 2)) ] [ text "Team Two" ]
            ]
        , div [ class "row" ]
            [ viewGenerateTeamList gameState.players ]
        ]


viewGenerateTeamList : List Player -> Html Msg
viewGenerateTeamList players =
    div [ class "team-display column column-50 column-offset-25" ]
        [ table []
            (List.concat
                [ [ thead []
                        [ th [] [ text "Player " ]
                        , th [] [ text "Team" ]
                        ]
                  ]
                , List.map viewGenerateTeamPlayer players
                ]
            )
        ]


viewGenerateTeamPlayer : Player -> Html Msg
viewGenerateTeamPlayer player =
    case player.team of
        Just team ->
            tr []
                [ td [] [ text player.name ]
                , td [] [ text (String.fromInt team) ]
                ]

        Nothing ->
            tr []
                [ td [] [ text player.name ]
                , td [] [ text "Undecided..." ]
                ]


viewGameTable : Session -> GameState -> Html Msg
viewGameTable session gameState =
    cardTableView session gameState.players



-- PORTS/SUBSCRIPTIONS


port gotGameState : (JE.Value -> msg) -> Sub msg


port pushPlayerAction : JE.Value -> Cmd msg


encodeAction : PlayerAction -> JE.Value
encodeAction action =
    case action of
        ChooseTeam team ->
            JE.object
                [ ( "action", JE.string "choose_team" )
                , ( "payload", JE.object [ ( "team", JE.int team ) ] )
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotGameState GotGameState
        ]
