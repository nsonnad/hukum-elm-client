port module Main exposing (..)

import Browser exposing (Document)
import Data.GameState exposing (..)
import Data.SharedTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import Page.Game as Game
import Page.Lobby as Lobby



---- MODEL ----


type alias Model =
    { page : Page
    , session : Session
    }


type Msg
    = UpdateUsername String
    | AddNewUser JE.Value
    | Registered JE.Value
    | GotLobbyMsg Lobby.Msg
    | GotGameMsg Data.GameState.Msg


type Page
    = Lobby Lobby.Model
    | Game Game.Model
    | Registration
    | NotFound



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateUsername userName ->
            ( { model | session = setUserName userName model.session }, Cmd.none )

        AddNewUser userName ->
            ( model, addNewUser userName )

        Registered status ->
            case JD.decodeValue JD.bool status of
                Ok True ->
                    ( { model | page = Lobby Lobby.initModel }
                    , Cmd.none
                    )

                Ok False ->
                    ( model, Cmd.none )

                Err msg ->
                    ( model, Cmd.none )

        GotLobbyMsg (Lobby.JoinedGameChannel joinedGame) ->
            case JD.decodeValue JD.bool joinedGame of
                Ok True ->
                    ( { model | page = Game Game.initModel }, Cmd.none )

                Ok False ->
                    ( model, Cmd.none )

                Err msg ->
                    ( model, Cmd.none )

        GotLobbyMsg lobbyMsg ->
            case model.page of
                Lobby lobby ->
                    toLobby model (Lobby.update model.session lobbyMsg lobby)

                _ ->
                    ( model, Cmd.none )

        GotGameMsg gameMsg ->
            case model.page of
                Game game ->
                    toGame model (Game.update model.session gameMsg game)

                _ ->
                    ( model, Cmd.none )


toLobby : Model -> ( Lobby.Model, Cmd Lobby.Msg ) -> ( Model, Cmd Msg )
toLobby model ( lobby, cmd ) =
    ( { model | page = Lobby lobby }, Cmd.map GotLobbyMsg cmd )


toGame : Model -> ( Game.Model, Cmd Data.GameState.Msg ) -> ( Model, Cmd Msg )
toGame model ( game, cmd ) =
    ( { model | page = Game game }, Cmd.map GotGameMsg cmd )


setUserName : String -> Session -> Session
setUserName userName session =
    { session | userName = userName }



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                Registration ->
                    viewWrapper (viewUnregistered model.session.userName)

                Lobby lobby ->
                    Lobby.view model.session lobby
                        |> Html.map GotLobbyMsg

                Game game ->
                    Game.view model.session game
                        |> Html.map GotGameMsg

                NotFound ->
                    text "Not Found"
    in
    { title = "Hukum"
    , body =
        [ viewHeader model.page
        , content
        ]
    }


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


viewHeader : Page -> Html Msg
viewHeader page =
    case page of
        Game _ ->
            header [ class "header-game" ] [ h4 [] [ text "Hukum" ] ]

        _ ->
            header [] [ h2 [] [ text "Hukum" ] ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Registration ->
            Sub.batch
                [ registered Registered
                ]

        Lobby lobby ->
            Lobby.subscriptions lobby
                |> Sub.map GotLobbyMsg

        Game game ->
            Game.subscriptions game
                |> Sub.map GotGameMsg

        _ ->
            Sub.none



-- PORTS/SUBSCRIPTIONS


port addNewUser : JE.Value -> Cmd msg


port registered : (JE.Value -> msg) -> Sub msg



---- PROGRAM ----


initModel : Model
initModel =
    { page = Registration
    , session = { userName = "" }
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
