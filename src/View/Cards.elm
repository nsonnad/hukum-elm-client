module View.Cards exposing (activeHandView, concealedHandView, visibleHandView)

import Data.Cards exposing (..)
import Html exposing (Attribute, Html, div, img)
import Html.Attributes exposing (class, src, value)
import Html.Events exposing (onClick)


handContainer : List (Html msg) -> Html msg
handContainer =
    div [ class "hand" ]


concealedHandView : List Card -> Html msg
concealedHandView hand =
    handContainer (List.map (cardBackView []) hand)


visibleHandView : List Card -> Html msg
visibleHandView hand =
    handContainer (List.map (cardFrontView []) hand)


activeHandView : (Card -> msg) -> List Card -> Html msg
activeHandView onSelect hand =
    handContainer
        (List.map
            (\c ->
                cardFrontView [ onClick (onSelect c), class "selectable" ] c
            )
            hand
        )


cardBaseView : List (Attribute msg) -> List (Attribute msg) -> Card -> Html msg
cardBaseView wrapperAttrs imgAttrs card =
    div ([ class "card" ] ++ wrapperAttrs)
        [ img imgAttrs []
        ]


cardFrontView : List (Attribute msg) -> Card -> Html msg
cardFrontView attrs card =
    let
        imgSrc =
            "/cards/" ++ cardToString card ++ ".png"
    in
    cardBaseView attrs [ src imgSrc ] card


cardBackView : List (Attribute msg) -> Card -> Html msg
cardBackView attrs =
    cardBaseView attrs [ src "/cards/back.png" ]
