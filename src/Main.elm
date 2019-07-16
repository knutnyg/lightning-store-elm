module Main exposing (..)

import Browser
import Html exposing (Html, div, h2, li, p, text, ul)
import Http
import Json.Decode exposing (Decoder, field, int, list, map, map2, map3, string)


---- MODEL ----


type alias Model =
    { nodeInfo : Maybe NodeInfo
    , flags : Flags
    }


type alias NodeInfo =
    { blockHeight : Int
    , alias: String
    , uri: String
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
        { url = baseUrl ++ "/nodeInfo"
        , expect = Http.expectJson GotInfo getInfoDecoder
        }


getInfoDecoder : Decoder NodeInfo
getInfoDecoder =
    map3 NodeInfo
        (field "blockHeight" int)
        (field "alias" string)
        (field "uri"  string)


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
                [ h2 [] [ text ("Connect to my node: " ++ ns.alias) ]
                , p [] [ text ("BlockHeight: " ++ String.fromInt ns.blockHeight) ]
                , p [] [ text ("URI: " ++ ns.uri)]
                ]
            )

        Nothing ->
            text "waiting for data"



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
