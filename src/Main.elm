module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h2, img, p, text)
import Http
import Json.Decode exposing (Decoder, field, string, int, map)
import Html.Attributes exposing (src)


---- MODEL ----
type alias Model =
    {
        nodeInfo: Maybe NodeInfo
    }

type alias NodeInfo =
    {
        blockHeight: Int
    }


init : ( Model, Cmd Msg )
init =
    ( { nodeInfo = Nothing }, getInfo )

---- UPDATE ----
type Msg
    = GotInfo (Result Http.Error NodeInfo)

getInfo : Cmd Msg
getInfo =
    Http.get
    {
        url = "http://localhost:8080/status",
        expect = Http.expectJson GotInfo getInfoDecoder
    }

getInfoDecoder : Decoder NodeInfo
getInfoDecoder =
    map NodeInfo
        (field "block_height" int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInfo result ->
            case result of
                Ok ni -> ({ nodeInfo = Just ni}, Cmd.none)
                Err _ -> (model, Cmd.none)

---- VIEW ----


view : Model -> Html Msg
view model =
    div [] [
        nodeInfoView(model.nodeInfo)
        ]

nodeInfoView : Maybe NodeInfo -> Html Msg
nodeInfoView nodeInfo =
    case nodeInfo of
        Just ns ->
           (div []
            [ h2 [] [ text "nodeinfo" ]
            , p [] [ text ("BlockHeight: " ++ String.fromInt ns.blockHeight) ]
            ])
        Nothing -> div [] []

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
