module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, button, div, h1, h2, header, input, li, p, text, ul)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD
import Json.Encode as JE
import Page.Game as Game
import Page.Lobby as Lobby



---- MODEL ----


type alias Model =
    { page : Page }


type Msg
    = GotLobbyMsg Lobby.Msg
    | GotGameMsg Game.Msg


type Page
    = Lobby Lobby.Model
    | Game Game.Model
    | NotFound



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
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
                    toLobby model (Lobby.update lobbyMsg lobby)

                _ ->
                    ( model, Cmd.none )

        GotGameMsg gameMsg ->
            case model.page of
                Game game ->
                    toGame model (Game.update gameMsg game)

                _ ->
                    ( model, Cmd.none )


toLobby : Model -> ( Lobby.Model, Cmd Lobby.Msg ) -> ( Model, Cmd Msg )
toLobby model ( lobby, cmd ) =
    ( { model | page = Lobby lobby }, Cmd.map GotLobbyMsg cmd )


toGame : Model -> ( Game.Model, Cmd Game.Msg ) -> ( Model, Cmd Msg )
toGame model ( game, cmd ) =
    ( { model | page = Game game }, Cmd.map GotGameMsg cmd )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                Lobby lobby ->
                    Lobby.view lobby
                        |> Html.map GotLobbyMsg

                Game game ->
                    Game.view game
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


viewHeader : Page -> Html Msg
viewHeader page =
    header [] [ h2 [] [ text "Hukum" ] ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Lobby lobby ->
            Lobby.subscriptions lobby
                |> Sub.map GotLobbyMsg

        Game game ->
            Game.subscriptions game
                |> Sub.map GotGameMsg

        _ ->
            Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { page = Lobby Lobby.initModel }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
