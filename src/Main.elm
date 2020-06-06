module Main exposing (..)

import Browser
import Commands exposing (..)
import Html exposing (Html, button, div, h1, h2, input, li, p, text, ul)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import Page.Lobby as Lobby


type Page
    = Lobby Lobby.Model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ registered Registered
        , gotUserList GotUserList
        , joinedGameChannel JoinedGameChannel
        ]



--| Game Game.Model
---- MODEL ----


type alias Model =
    { page : Page
    }


type Msg
    = LobbyMsg Lobby.Msg
    | JoinedGameChannel JE.Value


init : ( Model, Cmd Msg )
init =
    ( { page = Lobby Lobby.initModel
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LobbyMsg msg ->
            Lobby.update msg model.page



--JoinedGameChannel game ->
--case JD.decodeValue JD.string game of
--Ok game_name ->
--( { model | page = Game }, Cmd.none )
--Err error ->
--( model, Cmd.none )
---- VIEW ----


view : Model -> Html Msg
view model =
    case model.page of
        Lobby lobby ->
            Lobby.view lobby



--Game game ->
--Game.view game
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
