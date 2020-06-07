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
    div [ class "page-wrapper" ] [ children ]


viewUnregistered : String -> Html Msg
viewUnregistered userName =
    div [ class "new-player-form" ]
        [ p [] [ text "Welcome to Hukum, a card game." ]
        , p [] [ text "Please enter a name we can call you by:" ]
        , input
            [ type_ "text"
            , placeholder "username"
            , value userName
            , onInput UpdateUsername
            ]
            []
        , viewBroadcastButton userName AddNewUser
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
        [ p [] [ text "This is the lobby. You can join an existing game\n                or start your own." ]
        , viewJoinOrCreateBtns model.userName
        , div [ class "players-list" ]
            [ p [] [ text "Current players:" ]
            , ul [] (List.map viewUserList model.lobbyUsers)
            ]
        , div [ class "game-list" ]
            [ p [] [ text "Open games:" ]
            , viewGameList model.gameList
            ]
        ]


viewJoiningGame : Model -> Html Msg
viewJoiningGame model =
    div [ class "join-game" ]
        [ p [] [ text "Game name:" ]
        , input
            [ type_ "text"
            , placeholder "game name"
            , value model.gameName
            , onInput UpdateGameName
            ]
            []
        , viewBroadcastButton model.gameName JoinGame
        ]


viewJoinOrCreateBtns : String -> Html Msg
viewJoinOrCreateBtns userName =
    div []
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
                |> onDoubleClick
    in
    tr [ joinHandler ]
        [ td [] [ text glg.name ]
        , td [] [ text glg.startedBy ]
        ]


viewUserList : String -> Html Msg
viewUserList user =
    li [] [ text user ]



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
