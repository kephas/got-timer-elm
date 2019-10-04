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
players0 = ["Greyjoy", "Baratheon", "Lannister", "Stark", "Tyrell", "Martell"]

initPlayer : Int -> String -> Player
initPlayer time name =
    Player name (interpolate "/public/img/family/{0}.jpg" [name]) time False

model0 = Model <| List.map (initPlayer defaultTime) players0

init : () -> ( Model, Cmd Msg )
init _ =
    ( model0, Cmd.none )



---- UPDATE ----

type Msg
    = Toggle Player
    | StartAll
    | StopAll
    | ResetAll


toggleOne : Player -> Player
toggleOne player =
  { player | running = not player.running }

toggleOnly : Player -> List Player -> List Player
toggleOnly player players =
  List.map (\p -> if p == player then toggleOne p else p) players

runAll : Bool -> List Player -> List Player
runAll run players =
  List.map (\p -> { p | running = run }) players



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Toggle player ->
      ( { model | players = toggleOnly player model.players }, Cmd.none)

    StartAll ->
      ( { model | players = runAll True model.players }, Cmd.none)

    StopAll ->
      ( { model | players = runAll False model.players }, Cmd.none)

    ResetAll ->
      ( model0, Cmd.none)


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
    el [ inFront <|
           el [ centerX, alignBottom, moveUp 20
              , Bord.rounded 8
              , alpha 0.6, Back.color white ] <|
           button [  spacing 10, padding 20 ]
             { onPress =  Just <| Toggle player
             , label =
               column [ spacing 5 ]
                 [ el [ centerX ] <| text <| formatTime player.remainingTime
                 , el [ centerX ] <| text <| if player.running then "STOP" else "START"
                 ]
             }
       ] <|
      image [ height <| px 570, width <| px 205 ] { src = player.picture, description = player.name }
        
timerButton msg btnText =
    button [ height <| px 60, width fill, paddingXY 10 4
           , Bord.rounded 4
           , Back.color colorPrimary
           , Font.color white
           ]
        { onPress = Just msg, label = el [ centerX ] <| text btnText }

view : Model -> Browser.Document Msg
view model =
    { title = "Foobar"
    , body = [ layout [ height fill ] <|
          column [ height fill ]
              [ wrappedRow [] <| List.map viewPlayer model.players
              , row [ padding 20, spacing 10, width fill ]
                  [ timerButton StartAll "START ALL"
                  , timerButton StopAll "STOP ALL"
                  ]
              , el [ alignBottom ] <| timerButton ResetAll "RESET ALL"
              ]
             ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Foo = { foo : Int }              
type alias Foobar = { foo : Int, bar : Int }

convert : Foo -> String
convert x = String.fromInt x.foo
