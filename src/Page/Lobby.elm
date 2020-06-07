port module Page.Lobby exposing (Model, Msg, init, initModel, subscriptions, update, view)

import Data.GameListGame exposing (..)
import Html exposing (Html, button, div, h1, h2, input, li, p, text, ul)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
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
    | AddNewUser JE.Value
    | Registered JE.Value
    | GotUserList JE.Value
    | JoinGame
    | StartNewGame JE.Value


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

        AddNewUser value ->
            ( model, addNewUser value )

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

        StartNewGame value ->
            ( { model | status = StartingGame }, startNewGame value )

        JoinGame ->
            ( { model | status = JoiningGame }, Cmd.none )


decodeUserList : JD.Decoder (List String)
decodeUserList =
    JD.list JD.string



-- VIEW


view : Model -> Html Msg
view model =
    case model.status of
        Unregistered ->
            div [ class "new-player-form" ]
                [ p [] [ text "Username:" ]
                , input
                    [ type_ "text"
                    , placeholder "username"
                    , value model.userName
                    , onInput UpdateUsername
                    ]
                    []
                , viewAddNewUserButton model
                ]

        InLobby ->
            div []
                [ h1 [] [ text "In tha Lobby!" ]
                , viewJoinOrCreateBtns model.userName
                , h2 [] [ text "Current players:" ]
                , ul [] (List.map viewUserList model.lobbyUsers)
                ]

        JoiningGame ->
            div [ class "join-game" ]
                [ p [] [ text "Game name:" ]
                , input
                    [ type_ "text"
                    , placeholder "game name"
                    , value model.gameName
                    , onInput UpdateGameName
                    ]
                    []
                ]

        StartingGame ->
            div []
                [ h1 [] [ text "Starting game!" ] ]


viewJoinOrCreateBtns : String -> Html Msg
viewJoinOrCreateBtns user_name =
    div []
        [ button [ onClick (StartNewGame (JE.string user_name)) ] [ text "New Game" ]
        , button [ onClick JoinGame ] [ text "Join Game" ]
        ]


viewUserList : String -> Html Msg
viewUserList user =
    li [] [ text user ]


viewAddNewUserButton : Model -> Html Msg
viewAddNewUserButton model =
    let
        broadcastNewUser =
            model.userName
                |> JE.string
                |> AddNewUser
                |> onClick
    in
    button
        [ type_ "submit", broadcastNewUser ]
        [ text "Submit" ]



-- PORTS/SUBSCRIPTIONS


port addNewUser : JE.Value -> Cmd msg


port registered : (JE.Value -> msg) -> Sub msg


port gotUserList : (JE.Value -> msg) -> Sub msg


port joinedGameChannel : (JE.Value -> msg) -> Sub msg


port startNewGame : JE.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ registered Registered
        , gotUserList GotUserList
        ]
