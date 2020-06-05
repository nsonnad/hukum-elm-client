port module Main exposing (..)

import Browser
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
    | InGame


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
        , joinedGameChannel JoinedGameChannel
        ]



---- MODEL ----


type alias Model =
    { status : Status
    , user_name : String
    , game_name : String
    , all_users : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { status = Unregistered
      , user_name = ""
      , game_name = ""
      , all_users = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateUsername String
    | UpdateGameName String
    | AddNewUser JE.Value
    | Registered JE.Value
    | GotUserList JE.Value
    | JoinGame
    | StartNewGame JE.Value
    | JoinedGameChannel JE.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername user_name ->
            ( { model | user_name = user_name }, Cmd.none )

        UpdateGameName game_name ->
            ( { model | game_name = game_name }, Cmd.none )

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
                    ( { model | all_users = users }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        StartNewGame value ->
            ( { model | status = StartingGame }, startNewGame value )

        JoinGame ->
            ( { model | status = JoiningGame }, Cmd.none )

        JoinedGameChannel game ->
            case JD.decodeValue JD.string game of
                Ok game_name ->
                    ( { model | status = InGame, game_name = game_name }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


decodeUserList : JD.Decoder (List String)
decodeUserList =
    JD.list JD.string



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.status of
        Unregistered ->
            div [ class "new-player-form" ]
                [ p [] [ text "Username:" ]
                , input
                    [ type_ "text"
                    , placeholder "username"
                    , value model.user_name
                    , onInput UpdateUsername
                    ]
                    []
                , viewAddNewUserButton model
                ]

        InLobby ->
            div []
                [ h1 [] [ text "In tha Lobby!" ]
                , viewJoinOrCreateBtns model.user_name
                , h2 [] [ text "Current players:" ]
                , ul [] (List.map viewUserList model.all_users)
                ]

        JoiningGame ->
            div [ class "join-game" ]
                [ p [] [ text "Game name:" ]
                , input
                    [ type_ "text"
                    , placeholder "game name"
                    , value model.game_name
                    , onInput UpdateGameName
                    ]
                    []
                ]

        StartingGame ->
            div []
                [ h1 [] [ text "Starting game!" ] ]

        InGame ->
            div []
                [ h1 [] [ text ("Welcome to game" ++ model.game_name) ] ]


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
            model.user_name
                |> JE.string
                |> AddNewUser
                |> onClick
    in
    button
        [ type_ "submit", broadcastNewUser ]
        [ text "Submit" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
