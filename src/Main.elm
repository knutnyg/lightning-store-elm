module Main exposing (Flags, Invoice, Model, Msg(..), NodeInfo, getInfo, getInfoDecoder, init, main, nodeInfoView, update, view)

import Browser
import Html exposing (Html, a, article, button, div, h1, h2, header, input, li, nav, p, section, span, text, ul)
import Html.Attributes exposing (class, href, id, type_)
import Http
import Json.Decode exposing (Decoder, field, int, list, map, map2, map3, string)
import QRCode



---- MODEL ----


type alias Model =
    { nodeInfo : Maybe NodeInfo
    , flags : Flags
    , invoice : Maybe Invoice
    }


type alias Invoice =
    { rhash : String
    , paymentRequest : String
    }


type alias NodeInfo =
    { blockHeight : Int
    , alias : String
    , uri : String
    }


type alias Flags =
    { backendApiUrl : String }


init : Flags -> ( Model, Cmd Msg )
init flag =
    ( { nodeInfo = Nothing, flags = flag, invoice = Nothing }, getInfo flag.backendApiUrl )



---- UPDATE ----


type Msg
    = GotInfo (Result Http.Error NodeInfo)
    | GetInvoice
    | GotInvoice (Result Http.Error Invoice)


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
        , section []
            [ article []
                [ header [] [ text "title" ]
                , p [] [ text "lorem ipsum" ]
                ]
            , nodeInfoView model.nodeInfo
            ]
        ]



--	<article>
--			<header>
--				<h2>Article title</h2>
--				<p>Posted on <time datetime="2009-09-04T16:31:24+02:00">September 4th 2009</time> by <a href="#">Writer</a> - <a href="#comments">6 comments</a></p>
--			</header>
--			<p>Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas.</p>
--		</article>
--    div []
--        [ nodeInfoView model.nodeInfo
--        , button [ onClick GetInvoice ] [ text "Pay me all your money" ]
--        , invoiceView model.invoice
--        ]


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
