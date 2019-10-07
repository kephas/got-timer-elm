module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events as Ev
import Browser.Navigation as Nav
import Element as El
import Element.Input as In
import Element.Background as Back
import Element.Border as Bord
import Element.Font as Font
import Html exposing (Html)
import String.Interpolate exposing(interpolate)
import Task
import Time
import Url



white = El.rgb 1 1 1
colorPrimary = El.rgb255 0x00 0x7b 0xff

---- MODEL ----


type alias Player =
    { name : String
    , picture : String
    , remainingTime : Int
    , running : Bool
    }

type alias ViewableModel =
    { width : Int
    , height : Int
    , players : List Player
    }

type Model = Unviewable (List Player) | Viewable ViewableModel


defaultTime = 3600
players0 = ["Greyjoy", "Baratheon", "Lannister", "Stark", "Tyrell", "Martell"]

initPlayer : Int -> String -> Player
initPlayer time name =
    Player name (interpolate "%PUBLIC_URL%/img/family/{0}.jpg" [name]) time False

model0 = Unviewable <| List.map (initPlayer defaultTime) players0

init : () -> ( Model, Cmd Msg )
init _ =
    ( model0, getViewport )

getViewport = Task.perform viewportResize Dom.getViewport

viewportResize : Dom.Viewport -> Msg
viewportResize viewport =
  Resize (round viewport.viewport.width) (round viewport.viewport.height)


---- UPDATE ----

type Msg
    = Toggle Player
    | StartAll
    | StopAll
    | ResetAll
    | Tick Time.Posix
    | Resize Int Int


toggleOne : Player -> Player
toggleOne player =
  { player | running = not player.running }

toggleOnly : Player -> List Player -> List Player
toggleOnly player players =
  List.map (\p -> if p == player then toggleOne p else p) players

runAll : Bool -> List Player -> List Player
runAll run players =
  List.map (\p -> { p | running = run }) players


tickOne : Player -> Player
tickOne player =
  { player | remainingTime = player.remainingTime - 1 }

tickRunning : List Player -> List Player
tickRunning players =
  List.map (\p -> if p.running then tickOne p else p) players


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Unviewable players ->
            case msg of
                Resize width height ->
                    ( Viewable { width = width, height = height, players = players }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Viewable viewableModel ->
            case msg of
                Toggle player ->
                    ( Viewable { viewableModel | players = toggleOnly player viewableModel.players }, Cmd.none)

                StartAll ->
                    ( Viewable { viewableModel | players = runAll True viewableModel.players }, Cmd.none)

                StopAll ->
                    ( Viewable { viewableModel | players = runAll False viewableModel.players }, Cmd.none)

                ResetAll ->
                    ( model0, getViewport)

                Tick _ ->
                    ( Viewable { viewableModel | players = tickRunning viewableModel.players }, Cmd.none)

                Resize width height ->
                    ( Viewable { viewableModel | height = height, width = width }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 Tick
    , Ev.onResize Resize
    ]


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

viewPlayer width player =
    let fontSize = Font.size <| width // 7
    in
    El.el [ El.inFront <|
           El.el [ El.centerX, El.alignBottom, El.moveUp ((toFloat width) / 10)
              , Bord.rounded 8
              , El.alpha 0.6, Back.color white ] <|
           In.button [  El.spacing 4, El.padding (width // 10) ]
             { onPress =  Just <| Toggle player
             , label =
               El.column [ El.spacing 5 ]
                 [ El.el [ fontSize, El.centerX ] <| El.text <| formatTime player.remainingTime
                 , El.el [ fontSize, El.centerX ] <| El.text <| if player.running then "STOP" else "START"
                 ]
             }
       ] <|
      El.image (imageSize width) { src = player.picture, description = player.name }
        
timerButton size msg btnText =
    In.button [ El.height <| El.px 60, El.width El.fill, El.paddingXY 10 4
           , Bord.rounded 4
           , Back.color colorPrimary
           , Font.color white, size
           ]
        { onPress = Just msg, label = El.el [ El.centerX ] <| El.text btnText }

imageSize width =
  let proportionalHeight = 570 * width // 205
  in
  [ El.height <| El.px proportionalHeight
  , El.width <| El.px width
  ]

viewViewable : ViewableModel -> Html Msg
viewViewable model =
    let btnFont = Font.size <| model.width // 25
    in
    El.layout [ El.height El.fill ] <|
        El.column [ El.height El.fill ]
            [ El.wrappedRow [] <| List.map (viewPlayer <| model.width * 10 // 61 ) model.players
            , El.row [ El.padding 20, El.spacing 10, El.width El.fill ]
                [ timerButton btnFont StartAll "START ALL"
                , timerButton btnFont StopAll "STOP ALL"
                ]
            , El.el [ El.alignBottom ] <| timerButton btnFont ResetAll "RESET ALL"
            ]



view : Model -> Browser.Document Msg
view model =
    { title = "GoT Timer"
    , body = [ case model of
                   Unviewable _ ->
                       El.layout [] <| El.text "…"

                   Viewable viewable ->
                       viewViewable viewable
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
