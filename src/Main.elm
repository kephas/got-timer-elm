module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Input exposing (button)
import Element.Background as Back
import Element.Border as Bord
import Element.Font as Font
import String.Interpolate exposing(interpolate)
import Url



white = rgb 1 1 1
colorPrimary = rgb255 0x00 0x7b 0xff

---- MODEL ----


type alias Player =
    { name : String
    , picture : String
    , remainingTime : Int
    , running : Bool
    }

type alias Model =
    { players : List Player
    }


defaultTime = 3600
players = ["Greyjoy", "Baratheon", "Lannister", "Stark", "Tyrell", "Martell"]

initPlayer : Int -> String -> Player
initPlayer time name =
    Player name (interpolate "/public/img/family/{0}.jpg" [name]) time False

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model <| List.map (initPlayer defaultTime) players, Cmd.none )



---- UPDATE ----

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | Toggle Player
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

paddedNumber : Int -> String
paddedNumber n =
  let raw = String.fromInt n
  in if n < 10 then
       "0" ++ raw
     else
       raw

formatTime seconds =
    let hours = seconds // 3600
        afterHours = remainderBy 3600 seconds
        minutes = afterHours // 60
        afterMinutes = remainderBy 60 afterHours
    in
        interpolate "{0}:{1}:{2}" <| List.map paddedNumber [hours, minutes, afterMinutes]

viewPlayer player =
    el [ inFront <| text <| formatTime player.remainingTime ] <|
        column []
            [ image [ height <| px 570, width <| px 205 ] { src = player.picture, description = player.name }
            , button [] { onPress =  Just <| Toggle player, label = text <| if player.running then "STOP" else "START" }
            ]
        
timerButton size msg btnText =
    button [ paddingXY 10 4
           , Bord.rounded 4
           , Back.color colorPrimary
           , Font.color white
           ]
        { onPress = Just msg, label = text btnText }

view : Model -> Browser.Document Msg
view model =
    { title = "Foobar"
    , body = [ layout [] <|
          column []
              [ row [] <| List.map viewPlayer model.players
              , row [ padding 20, spacing 10 ]
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
