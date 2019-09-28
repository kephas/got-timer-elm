module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (Element, layout, el, text, row, column, image, height, width, px, inFront) --alignRight, fill, rgb255, spacing, centerY, padding)
import Element.Input exposing (button)
import String.Interpolate exposing(interpolate)
import Url


---- MODEL ----


type alias Family =
    { name : String
    , picture : String
    , remainingTime : Int
    , buttonText : String
    }

type alias Model =
    { families : List Family
    }


defaultTime = 3600
families = ["Greyjoy", "Baratheon", "Lanister", "Stark", "Tyrell", "Martell"]

initFamily : Int -> String -> Family
initFamily time name =
    Family name (interpolate "/public/img/family/{0}.jpg" [name]) time "WHAT?"

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model <| List.map (initFamily defaultTime) families, Cmd.none )



---- UPDATE ----

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | Toggle Family
    | StartAll
    | StopAll
    | ResetAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


---- VIEW ----

formatTime seconds =
    let hours = seconds // 3600
        afterHours = remainderBy 3600 seconds
        minutes = afterHours // 60
        afterMinutes = remainderBy 60 afterHours
    in
        interpolate "{0}:{1}:{2}" <| List.map String.fromInt [hours, minutes, afterMinutes]

viewFamily family =
    el [ inFront <| text <| formatTime family.remainingTime ] <|
        column []
            [ image [ height <| px 570, width <| px 205 ] { src = family.picture, description = family.name }
            , button [] { onPress =  Just <| Toggle family, label = text family.buttonText }
            ]
        
timerButton size msg btnText =
    button [] { onPress = Just msg, label = text btnText }

view : Model -> Browser.Document Msg
view model =
    { title = "Foobar"
    , body = [ layout [] <|
          column []
              [ row [] <| List.map viewFamily model.families
              , row []
                  [ timerButton 6 StartAll "start all"
                  , timerButton 6 StopAll "STOP     ALL"
                  , timerButton 12 ResetAll "RESET ALL"
                  ]
              ]
             ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


type alias Foo = { foo : Int }              
type alias Foobar = { foo : Int, bar : Int }

convert : Foo -> String
convert x = String.fromInt x.foo
