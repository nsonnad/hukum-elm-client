module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, button, div, h1, h2, input, li, p, text, ul)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import Page.Lobby as Lobby



---- MODEL ----


type alias Model =
    { page : Page }


type Msg
    = GotLobbyMsg Lobby.Msg


type Page
    = Lobby Lobby.Model
    | NotFound



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotLobbyMsg lobbyMsg ->
            case model.page of
                Lobby lobby ->
                    toLobby model (Lobby.update lobbyMsg lobby)

                _ ->
                    ( model, Cmd.none )


toLobby : Model -> ( Lobby.Model, Cmd Lobby.Msg ) -> ( Model, Cmd Msg )
toLobby model ( lobby, cmd ) =
    ( { model | page = Lobby lobby }, Cmd.map GotLobbyMsg cmd )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                Lobby lobby ->
                    Lobby.view lobby
                        |> Html.map GotLobbyMsg

                NotFound ->
                    text "Not Found"
    in
    { title = "Hukum"
    , body = [ content ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Lobby lobby ->
            Lobby.subscriptions lobby
                |> Sub.map GotLobbyMsg

        _ ->
            Sub.none



--Game game ->
--Game.view game
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> ( { page = Lobby Lobby.initModel }, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
