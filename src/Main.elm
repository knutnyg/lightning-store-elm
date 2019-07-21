module Main exposing (Article, Flags, Invoice, Model, Msg(..), NodeInfo, getInfo, getInfoDecoder, init, main, nodeInfoView, update, view)

import Array exposing (Array)
import Base64
import Browser
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (Html, a, article, button, div, form, h1, h2, header, img, input, li, nav, p, section, span, text, ul)
import Html.Attributes as Attributes exposing (action, class, href, id, src, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, field, maybe, string)
import Json.Encode as Encode
import QRCode
import Url exposing (Url)
import Utility exposing (textHtml)



---- MODEL ----


type alias Model =
    { loginStatus : LoginState
    , formKey : String
    , nodeInfo : Maybe NodeInfo
    , flags : Flags
    , invoice : Maybe Invoice
    , articleTeasers : List Article
    , url : Url
    , key : Key
    }


type alias Invoice =
    { rhash : String
    , paymentRequest : String
    }


type LoginState
    = LoggedIn String
    | Anonymous


type alias CheckLoginResult =
    { status : LoginStateTemp
    , key : Maybe String
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
    , content : Maybe String
    }


type alias Flags =
    { backendApiUrl : String }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flag url key =
    ( { loginStatus = Anonymous
      , formKey = ""
      , nodeInfo = Nothing
      , flags = flag
      , invoice = Nothing
      , articleTeasers = []
      , key = key
      , url = url
      }
    , Cmd.batch [ checkLogin flag.backendApiUrl, getInfo flag.backendApiUrl, getArticles flag.backendApiUrl ]
    )



---- UPDATE ----


type Msg
    = GotInfo (Result Http.Error NodeInfo)
    | GetInvoice
    | CheckLogin
    | SetFormKey String
    | DoLogin
    | GotCheckLogin (Result Http.Error LoginState)
    | GotInvoice (Result Http.Error Invoice)
    | AddedInvoice (Result Http.Error CreateInvoiceResult)
    | GotArticles (Result Http.Error (List Article))
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | StartPayment
    | GetArticle String
    | GotArticle (Result Http.Error Article)


newLoginCheckPost : String -> Encode.Value
newLoginCheckPost key =
    Encode.object
        [ ( "key", Encode.string key ) ]


checkLoginKey : String -> String -> Cmd Msg
checkLoginKey baseUrl key =
    Http.riskyRequest
        { method = "POST"
        , headers = []
        , body = Http.jsonBody (newLoginCheckPost key)
        , url = baseUrl ++ "/login"
        , expect = Http.expectJson GotCheckLogin checkLoginResultDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


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


type alias CreateInvoiceResult =
    { id : String
    , rhash : String
    , paymentRequest : String
    , memo : Maybe String
    }


invoiceAddedDecoder : Decoder CreateInvoiceResult
invoiceAddedDecoder =
    Decode.map4 CreateInvoiceResult
        (field "id" string)
        (field "rhash" string)
        (field "paymentRequest" string)
        (Decode.maybe (field "memo" string))


createInvoice : String -> CreateInvoiceResult -> Cmd Msg
createInvoice baseUrl invoice =
    Http.post
        { url = baseUrl
        , body = Http.emptyBody
        , expect = Http.expectJson AddedInvoice invoiceAddedDecoder
        }


getArticles : String -> Cmd Msg
getArticles baseUrl =
    let
        _ =
            Debug.log "msg" "Fetching articles"
    in
    Http.get
        { url = baseUrl ++ "/articles"
        , expect = Http.expectJson GotArticles articlesDecoder
        }


getArticle : String -> String -> Cmd Msg
getArticle baseUrl uuid =
    let
        _ =
            Debug.log "msg" "Fetching article"
    in
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , body = Http.emptyBody
        , url = baseUrl ++ "/articles/" ++ uuid
        , expect = Http.expectJson GotArticle articleDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


articleDecoder : Decoder Article
articleDecoder =
    Decode.map4 Article
        (field "uuid" string)
        (field "title" string)
        (field "teaser" string)
        (maybe (field "content" string))


articlesDecoder : Decoder (List Article)
articlesDecoder =
    Decode.list articleDecoder


invoiceDecoder : Decoder Invoice
invoiceDecoder =
    Decode.map2 Invoice
        (field "rhash" string)
        (field "paymentRequest" string)


getInfoDecoder : Decoder NodeInfo
getInfoDecoder =
    Decode.map3 NodeInfo
        (field "blockHeight" Decode.int)
        (field "alias" string)
        (field "uri" string)


updateArticle : Article -> List Article -> List Article
updateArticle up articles =
    List.map
        (\l ->
            if l.uuid == up.uuid then
                up

            else
                l
        )
        articles


type LoginStateTemp
    = LoggedInTemp
    | AnonymousTemp


loginStateDecoder : Decoder LoginStateTemp
loginStateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "LOGGED_IN" ->
                        Decode.succeed LoggedInTemp

                    _ ->
                        Decode.succeed AnonymousTemp
            )


checkLoginResultDecoder : Decoder LoginState
checkLoginResultDecoder =
    Decode.map2 CheckLoginResult
        (field "status" loginStateDecoder)
        (Decode.maybe (field "key" string))
        |> Decode.andThen
            (\val ->
                case ( val.status, val.key ) of
                    ( LoggedInTemp, Just key ) ->
                        Decode.succeed (LoggedIn key)

                    _ ->
                        Decode.succeed Anonymous
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetFormKey val ->
            ( { model | formKey = val }, Cmd.none )

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
                Ok status ->
                    ( { model | loginStatus = status }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        DoLogin ->
            ( model, checkLoginKey model.flags.backendApiUrl model.formKey )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        StartPayment ->
            let
                _ =
                    Debug.log "message" "starting payment"
            in
            ( model, Cmd.none )

        GotArticle result ->
            case result of
                Ok i ->
                    ( { model | articleTeasers = updateArticle i model.articleTeasers }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetArticle uuid ->
            ( model, getArticle model.flags.backendApiUrl uuid )

        AddedInvoice result ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Lightning Concept Store"
        [ div [ class "app" ]
            [ header [ class "header" ]
                [ nav []
                    [ div [ id "menuToggle" ]
                        [ input [ type_ "checkbox" ] []
                        , span [] [ text "" ]
                        , span [] [ text "" ]
                        , span [] [ text "" ]
                        , ul [ id "menu" ]
                            [ li [] [ a [ href "/" ] [ text "Home" ] ]
                            , li [] [ a [ href "/articles" ] [ text "Articles" ] ]
                            , li [] [ a [ href "#" ] [ text "Contact" ] ]
                            ]
                        ]
                    ]
                , h1 [] [ text "Concept Lightning Store" ]
                ]
            , let
                segments =
                    String.split "/" model.url.path
                        |> List.filter (\s -> s /= "")
              in
              case segments of
                [ "articles" ] ->
                    articleTeaserViews model.articleTeasers

                [ "articles", uuid ] ->
                    let
                        maybeArticle =
                            List.head (List.filter (\a -> a.uuid == uuid) model.articleTeasers)
                    in
                    case maybeArticle of
                        Nothing ->
                            p [] [ text "could not find article" ]

                        Just article ->
                            articleView article

                _ ->
                    div []
                        [ loginView model
                        , section [] [ nodeInfoView model.nodeInfo ]
                        ]
            ]
        ]


loginView : Model -> Html Msg
loginView model =
    section []
        [ case model.loginStatus of
            LoggedIn key ->
                div [] [ text key ]

            Anonymous ->
                div []
                    [ section [] [ text "Enter your key to restore the session: " ]
                    , section []
                        [ form [ onSubmit DoLogin ]
                            [ input
                                [ Attributes.style "width" "100%"
                                , Attributes.placeholder ""
                                , type_ "text"
                                , onInput SetFormKey
                                ]
                                []
                            , button [ onSubmit DoLogin ] [ text "Login" ]
                            ]
                        ]
                    ]
        ]


articleTeaserViews : List Article -> Html Msg
articleTeaserViews lst =
    div []
        (List.map
            (\l ->
                div []
                    [ article []
                        (textHtml
                            (case Base64.decode l.teaser of
                                Ok res ->
                                    res

                                Err _ ->
                                    "<p>Failed to decode article</p>"
                            )
                        )
                    , a [ href ("/articles/" ++ l.uuid) ] [ text "To full article >" ]
                    ]
            )
            lst
        )


articleView : Article -> Html Msg
articleView art =
    case art.content of
        Nothing ->
            div []
                [ article []
                    (textHtml
                        (case Base64.decode art.teaser of
                            Ok res ->
                                res

                            Err err ->
                                err
                        )
                    )
                , button [ onClick (GetArticle art.uuid) ] [ text "buy me please" ]
                ]

        Just a ->
            article []
                (textHtml
                    (case Base64.decode a of
                        Ok res ->
                            res

                        Err err ->
                            err
                    )
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
                [ h2 [] [ text "Connect to my node: " ]
                , p [] [ text ("Name: " ++ ns.alias) ]
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
