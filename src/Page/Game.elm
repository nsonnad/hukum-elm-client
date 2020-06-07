port module Page.Game exposing (..)

import Data.GameState exposing (..)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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


view : Model -> Html Msg
view model =
    case model.gameState of
        Just gameState ->
            case gameState.stage of
                WaitingForPlayers ->
                    viewWrapper (viewWaitingForPlayers gameState)

                ChoosingTeams ->
                    viewWrapper (viewWaitingForPlayers gameState)

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
        [ p [] [ text ("You're in the game" ++ gameState.id) ]
        , div [ class "players-list" ]
            [ p [] [ text "Current players:" ]
            , ul [] (List.map viewUserList gameState.players)
            ]
        ]


viewUserList : Player -> Html Msg
viewUserList player =
    li [] [ text player.name ]



-- PORTS/SUBSCRIPTIONS


port gotGameState : (JE.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotGameState GotGameState
        ]
