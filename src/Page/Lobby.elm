port module Page.Lobby exposing (..)

import Data.GameList exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Json.Decode as JD
import Json.Encode as JE


type Status
    = Unregistered
    | InLobby
    | JoiningGame
    | StartingGame


type alias Model =
    { status : Status
    , userName : String
    , gameList : List GameListGame
    , lobbyUsers : List String
    , gameName : String
    }


type Msg
    = UpdateUsername String
    | UpdateGameName String
    | Registered JE.Value
    | GotUserList JE.Value
    | GotGameList JE.Value
    | ManualJoinGame
    | ToLobby
    | JoinFromGameList String
    | StartNewGame JE.Value
    | AddNewUser JE.Value
    | JoinGame JE.Value
    | JoinedGameChannel JE.Value


initModel : Model
initModel =
    { status = Unregistered
    , userName = ""
    , gameList = []
    , lobbyUsers = []
    , gameName = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername userName ->
            ( { model | userName = userName }, Cmd.none )

        UpdateGameName gameName ->
            ( { model | gameName = gameName }, Cmd.none )

        Registered status ->
            case JD.decodeValue JD.bool status of
                Ok True ->
                    ( { model | status = InLobby }
                    , Cmd.none
                    )

                Ok False ->
                    ( model, Cmd.none )

                Err message ->
                    ( model, Cmd.none )

        GotUserList usersRaw ->
            case JD.decodeValue decodeUserList usersRaw of
                Ok users ->
                    ( { model | lobbyUsers = users }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        GotGameList gamesRaw ->
            case JD.decodeValue gameListDecoder gamesRaw of
                Ok gameList ->
                    ( { model | gameList = gameList }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        StartNewGame value ->
            ( { model | status = StartingGame }, startNewGame value )

        ManualJoinGame ->
            ( { model | status = JoiningGame }, Cmd.none )

        ToLobby ->
            ( { model | status = InLobby }, Cmd.none )

        JoinFromGameList gameName ->
            ( { model | status = JoiningGame, gameName = gameName }, Cmd.none )

        AddNewUser userName ->
            ( model, addNewUser userName )

        JoinGame gameName ->
            ( model
            , joinGame
                { userName = JE.string model.userName
                , gameName = gameName
                }
            )

        _ ->
            ( model, Cmd.none )


decodeUserList : JD.Decoder (List String)
decodeUserList =
    JD.list JD.string



-- VIEW


view : Model -> Html Msg
view model =
    case model.status of
        Unregistered ->
            viewWrapper (viewUnregistered model.userName)

        InLobby ->
            viewWrapper (viewInLobby model)

        JoiningGame ->
            viewWrapper (viewJoiningGame model)

        StartingGame ->
            div []
                [ h1 [] [ text "Starting game!" ] ]


viewWrapper : Html Msg -> Html Msg
viewWrapper children =
    div [ class "page-wrapper row" ]
        [ div [ class "column column-80 column-offset-10" ] [ children ] ]


viewUnregistered : String -> Html Msg
viewUnregistered userName =
    div [ class "unregistered" ]
        [ p [] [ text "Welcome. Please enter the name we should call you by:" ]
        , div [ class "new-player-form row" ]
            [ div [ class "column column-40 column-offset-25" ]
                [ input
                    [ type_ "text"
                    , placeholder "username"
                    , value userName
                    , onInput UpdateUsername
                    ]
                    []
                ]
            , div [ class "column column-10 column-offset-65" ]
                [ viewBroadcastButton userName AddNewUser ]
            ]
        ]


viewBroadcastButton : String -> (JE.Value -> Msg) -> Html Msg
viewBroadcastButton value action =
    let
        broadcastAction =
            value
                |> JE.string
                |> action
                |> onClick
    in
    button
        [ type_ "submit", broadcastAction ]
        [ text "Submit" ]


viewInLobby : Model -> Html Msg
viewInLobby model =
    div []
        [ p [] [ text "This is the lobby. You can join an existing game, or start your own." ]
        , viewJoinOrCreateBtns model.userName
        , div [ class "row lobby-info" ]
            [ div [ class "column column-30 players-list" ]
                [ h4 [] [ text "Players online" ]
                , viewUserList model.lobbyUsers
                ]
            , div [ class "column column-60 game-list" ]
                [ h4 [] [ text "Open games" ]
                , viewGameList model.gameList
                ]
            ]
        ]


viewJoiningGame : Model -> Html Msg
viewJoiningGame model =
    div [ class "join-game" ]
        [ p [] [ text "Enter the name of the game you'd like to join." ]
        , div [ class "row" ]
            [ div [ class "column column-40 column-offset-25" ]
                [ input
                    [ type_ "text"
                    , placeholder "game name"
                    , value model.gameName
                    , onInput UpdateGameName
                    ]
                    []
                ]
            , div [ class "column column-10 column-offset-65" ]
                [ viewBroadcastButton model.gameName JoinGame ]
            ]
        , button [ class "back-button button-outline", onClick ToLobby ] [ text "← Back" ]
        ]


viewJoinOrCreateBtns : String -> Html Msg
viewJoinOrCreateBtns userName =
    div [ class "join-or-create" ]
        [ button [ onClick (StartNewGame (JE.string userName)) ] [ text "New Game" ]
        , button [ onClick ManualJoinGame ] [ text "Join Game" ]
        ]


viewGameList : GameList -> Html Msg
viewGameList gl =
    table []
        (List.concat
            [ [ thead []
                    [ th [] [ text "Game" ]
                    , th [] [ text "Started by" ]
                    , th [] [ text "" ]
                    ]
              ]
            , List.map viewGameListGame gl
            ]
        )


viewGameListGame : GameListGame -> Html Msg
viewGameListGame glg =
    let
        joinHandler =
            glg.name
                |> JoinFromGameList
                |> onClick
    in
    tr [ class "game-list-row" ]
        [ td [] [ text glg.name ]
        , td [] [ text glg.startedBy ]
        , td [ class "join-btn" ] [ button [ class "button-outline game-list-join", joinHandler ] [ text "Join" ] ]
        ]


viewUserList : List String -> Html Msg
viewUserList users =
    table []
        (List.concat
            [ [ thead [] [ th [] [ text "Player" ] ] ]
            , List.map (\user -> tr [] [ td [] [ text user ] ]) users
            ]
        )



--viewUserRow : String -> Html Msg
--viewUserRow user =
-- PORTS/SUBSCRIPTIONS


port addNewUser : JE.Value -> Cmd msg


port registered : (JE.Value -> msg) -> Sub msg


port gotUserList : (JE.Value -> msg) -> Sub msg


port gotGameList : (JE.Value -> msg) -> Sub msg


port joinedGameChannel : (JE.Value -> msg) -> Sub msg


port startNewGame : JE.Value -> Cmd msg


port joinGame : { userName : JE.Value, gameName : JE.Value } -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ registered Registered
        , gotUserList GotUserList
        , gotGameList GotGameList
        , joinedGameChannel JoinedGameChannel
        ]
