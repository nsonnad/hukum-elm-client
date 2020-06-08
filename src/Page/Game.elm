port module Page.Game exposing (..)

import Data.GameState exposing (..)
import Data.SharedTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Json.Decode as JD
import Json.Encode as JE


type alias Model =
    { gameState : Maybe GameState
    }


type Msg
    = GotGameState JE.Value


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        GotGameState gs ->
            case JD.decodeValue gameStateDecoder gs of
                Ok gameState ->
                    ( { model | gameState = Just gameState }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


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
                    viewWrapper (viewWaitingForPlayers gameState)

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
            , text "We're waiting for other players to join, hang tight."
            ]
        , h4 [] [ text "Who's here now:" ]
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
    h2 [] [ text "choosing teams" ]



-- PORTS/SUBSCRIPTIONS


port gotGameState : (JE.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotGameState GotGameState
        ]
