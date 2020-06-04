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


port addNewUser : JE.Value -> Cmd msg


port registered : (JE.Value -> msg) -> Sub msg


port gotUserList : (JE.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ registered Registered
        , gotUserList GotUserList
        ]



---- MODEL ----


type alias Model =
    { status : Status
    , user_name : String
    , all_users : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { status = Unregistered
      , user_name = ""
      , all_users = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateUsername String
    | AddNewUser JE.Value
    | Registered JE.Value
    | GotUserList JE.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUsername user_name ->
            ( { model | user_name = user_name }, Cmd.none )

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
                , h2 [] [ text "Current players:" ]
                , ul [] (List.map viewUserList model.all_users)
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
