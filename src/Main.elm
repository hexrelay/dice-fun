module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s, top)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Route
    = Home
    | Roller
    | Probability
    | NotFound


type alias Model =
    { key : Nav.Key
    , route : Route
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , route = parseRoute url
      }
    , Cmd.none
    )


parseRoute : Url -> Route
parseRoute url =
    -- Hash routing: parse the fragment as if it were the path
    let
        fragmentPath =
            url.fragment |> Maybe.withDefault ""

        fakePath =
            { url | path = "/" ++ fragmentPath, fragment = Nothing }
    in
    Parser.parse routeParser fakePath
        |> Maybe.withDefault Home


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Home top
        , Parser.map Roller (s "roller")
        , Parser.map Probability (s "probability")
        ]



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            ( { model | route = parseRoute url }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Dice Fun"
    , body =
        [ layout
            [ Background.color (rgb255 30 30 35)
            , Font.color (rgb255 230 230 230)
            , Font.family [ Font.typeface "system-ui", Font.sansSerif ]
            , padding 40
            ]
            (column [ centerX, width (fill |> maximum 800), spacing 30 ]
                [ header
                , nav
                , content model.route
                ]
            )
        ]
    }


header : Element Msg
header =
    el
        [ Font.size 36
        , Font.bold
        , Font.color (rgb255 120 180 255)
        ]
        (text "Dice Fun")


nav : Element Msg
nav =
    row [ spacing 20 ]
        [ navLink "Home" "#/"
        , navLink "Roller" "#/roller"
        , navLink "Probability" "#/probability"
        ]


navLink : String -> String -> Element Msg
navLink label url =
    link
        [ padding 10
        , Background.color (rgb255 50 50 60)
        , Border.rounded 4
        , mouseOver [ Background.color (rgb255 70 70 85) ]
        ]
        { url = url
        , label = text label
        }


content : Route -> Element Msg
content route =
    case route of
        Home ->
            homeView

        Roller ->
            rollerView

        Probability ->
            probabilityView

        NotFound ->
            el [] (text "Page not found")


homeView : Element Msg
homeView =
    column [ spacing 20 ]
        [ paragraph []
            [ text "A playground for dice mechanics and probability experiments." ]
        , paragraph [ Font.color (rgb255 150 150 150) ]
            [ text "Use the navigation above to explore." ]
        ]


rollerView : Element Msg
rollerView =
    column [ spacing 20 ]
        [ el [ Font.size 24, Font.bold ] (text "Dice Roller")
        , paragraph [] [ text "Coming soon: roll some dice!" ]
        ]


probabilityView : Element Msg
probabilityView =
    column [ spacing 20 ]
        [ el [ Font.size 24, Font.bold ] (text "Probability Visualizer")
        , paragraph [] [ text "Coming soon: explore probability distributions." ]
        ]
