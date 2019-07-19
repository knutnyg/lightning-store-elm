module Main exposing (Flags, Invoice, Model, Msg(..), NodeInfo, getInfo, getInfoDecoder, init, main, nodeInfoView, update, view)

import Base64
import Browser
import Html exposing (Html, a, article, button, div, h1, h2, header, img, input, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (class, href, id, src, type_)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, map5, maybe, nullable, string)
import QRCode
import Utility exposing (textHtml)



---- MODEL ----


type alias Model =
    { loginStatus : LoginState
    , nodeInfo : Maybe NodeInfo
    , flags : Flags
    , invoice : Maybe Invoice
    , articleTeasers : List Article
    }


type alias Invoice =
    { rhash : String
    , paymentRequest : String
    }


type LoginState
    = LoggedIn
    | Anonymous


type alias CheckLoginResult =
    { status : LoginState
    }


type alias NodeInfo =
    { blockHeight : Int
    , alias : String
    , uri : String
    }


type alias Article =
    { uuid : String
    , title : String
    , teaser : String
    }


type alias Flags =
    { backendApiUrl : String }


init : Flags -> ( Model, Cmd Msg )
init flag =
    ( { loginStatus = Anonymous, nodeInfo = Nothing, flags = flag, invoice = Nothing, articleTeasers = [] }, Cmd.batch [ checkLogin flag.backendApiUrl, getInfo flag.backendApiUrl, getArticles flag.backendApiUrl ] )



---- UPDATE ----


type Msg
    = GotInfo (Result Http.Error NodeInfo)
    | GetInvoice
    | CheckLogin
    | GotCheckLogin (Result Http.Error CheckLoginResult)
    | GotInvoice (Result Http.Error Invoice)
    | GotArticles (Result Http.Error (List Article))


checkLogin : String -> Cmd Msg
checkLogin baseUrl =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , body = Http.emptyBody
        , url = baseUrl ++ "/login"
        , expect = Http.expectJson GotCheckLogin checkLoginResultDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



--checkLogin baseUrl =
--    Http.get
--        { url = baseUrl ++ "/login"
--        , expect = Http.expectJson GotCheckLogin checkLoginResultDecoder
--        }


getInfo : String -> Cmd Msg
getInfo baseUrl =
    Http.get
        { url = baseUrl ++ "/nodeInfo"
        , expect = Http.expectJson GotInfo getInfoDecoder
        }


getInvoice : String -> Cmd Msg
getInvoice baseUrl =
    Http.post
        { url = baseUrl ++ "/invoices"
        , body = Http.emptyBody
        , expect = Http.expectJson GotInvoice invoiceDecoder
        }


getArticles : String -> Cmd Msg
getArticles baseUrl =
    Http.get
        { url = baseUrl ++ "/articles"
        , expect = Http.expectJson GotArticles articleDecoder
        }


articleDecoder : Decoder (List Article)
articleDecoder =
    list
        (map3 Article
            (field "uuid" string)
            (field "title" string)
            (field "teaser" string)
        )


invoiceDecoder : Decoder Invoice
invoiceDecoder =
    map2 Invoice
        (field "rhash" string)
        (field "paymentRequest" string)


getInfoDecoder : Decoder NodeInfo
getInfoDecoder =
    map3 NodeInfo
        (field "blockHeight" int)
        (field "alias" string)
        (field "uri" string)


loginStateDecoder : Decoder LoginState
loginStateDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "LoggedIn" ->
                        Json.Decode.succeed LoggedIn

                    _ ->
                        Json.Decode.succeed Anonymous
            )


checkLoginResultDecoder : Decoder CheckLoginResult
checkLoginResultDecoder =
    Json.Decode.map CheckLoginResult
        (field "status" loginStateDecoder)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInfo result ->
            case result of
                Ok ni ->
                    ( { model | nodeInfo = Just ni }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetInvoice ->
            ( model, getInvoice model.flags.backendApiUrl )

        GotInvoice result ->
            case result of
                Ok i ->
                    ( { model | invoice = Just i }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotArticles result ->
            case result of
                Ok i ->
                    ( { model | articleTeasers = i }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CheckLogin ->
            ( model, checkLogin model.flags.backendApiUrl )

        GotCheckLogin result ->
            case result of
                Ok val ->
                    ( { model | loginStatus = val.status }, Cmd.none )

                Err err ->
                    let
                        error =
                            Debug.log "GotCheckLogin error=" err
                    in
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ header [ class "header" ]
            [ nav []
                [ div [ id "menuToggle" ]
                    [ input [ type_ "checkbox" ] []
                    , span [] [ text "" ]
                    , span [] [ text "" ]
                    , span [] [ text "" ]
                    , ul [ id "menu" ]
                        [ li [] [ a [ href "#" ] [ text "Home" ] ]
                        , li [] [ a [ href "#" ] [ text "About" ] ]
                        , li [] [ a [ href "#" ] [ text "Contact" ] ]
                        ]
                    ]
                ]
            , h1 [] [ text "Concept Lightning Store" ]
            ]
        , articleTeaserViews model.articleTeasers
        , section [] [ nodeInfoView model.nodeInfo ]
        ]


articleTeaserViews : List Article -> Html Msg
articleTeaserViews lst =
    div []
        (List.map
            (\l ->
                article []
                    (textHtml
                        (case Base64.decode l.teaser of
                            Ok res ->
                                res

                            Err _ ->
                                "<p>Failed to decode article</p>"
                        )
                    )
            )
            lst
        )


invoiceView : Maybe Invoice -> Html Msg
invoiceView invoice =
    case invoice of
        Just i ->
            div []
                [ p [] [ text ("paymenthash: " ++ i.paymentRequest) ]
                , qrCodeView i.paymentRequest
                ]

        Nothing ->
            text ""


nodeInfoView : Maybe NodeInfo -> Html Msg
nodeInfoView nodeInfo =
    case nodeInfo of
        Just ns ->
            div []
                [ h2 [] [ text ("Connect to my node: " ++ ns.alias) ]
                , p [] [ text ("BlockHeight: " ++ String.fromInt ns.blockHeight) ]
                , p [] [ text ("URI: " ++ ns.uri) ]
                ]

        Nothing ->
            text "waiting for data"


qrCodeView : String -> Html msg
qrCodeView message =
    QRCode.encode message
        |> Result.map QRCode.toSvg
        |> Result.withDefault
            (Html.text "Error while encoding to QRCode.")



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
