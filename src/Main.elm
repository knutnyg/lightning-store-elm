module Main exposing (..)

import Browser
import Html exposing (Html, div, h2, p, text)
import Http
import Json.Decode exposing (Decoder, field, int, map)


---- MODEL ----


type alias Model =
    { nodeInfo : Maybe NodeInfo
    , flags : Flags
    }


type alias NodeInfo =
    { blockHeight : Int
    }


type alias Flags =
    { backendApiUrl : String }


init : Flags -> ( Model, Cmd Msg )
init flag =
    ( { nodeInfo = Nothing, flags = flag }, getInfo flag.backendApiUrl )



---- UPDATE ----


type Msg
    = GotInfo (Result Http.Error NodeInfo)


getInfo : String -> Cmd Msg
getInfo baseUrl =
    Http.get
        { url = baseUrl ++ "/status"
        , expect = Http.expectJson GotInfo getInfoDecoder
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
                Ok ni ->
                    ( { model | nodeInfo = Just ni }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ nodeInfoView (model.nodeInfo)
        ]


nodeInfoView : Maybe NodeInfo -> Html Msg
nodeInfoView nodeInfo =
    case nodeInfo of
        Just ns ->
            (div []
                [ h2 [] [ text "nodeinfo" ]
                , p [] [ text ("BlockHeight: " ++ String.fromInt ns.blockHeight) ]
                ]
            )

        Nothing ->
            div [] []



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
