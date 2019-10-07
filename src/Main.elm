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

type alias GameSetup =
    { players : List Player
    , defaultTime : Int
    }

type alias GameView =
    { width : Int
    , height : Int
    , players : List Player
    , defaultTime : Int
    }

type Model = Unviewable GameSetup | Viewable GameView


defaultTime = 3600
players0 = ["Greyjoy", "Baratheon", "Lannister", "Stark", "Tyrell", "Martell"]

initPlayer : Int -> String -> Player
initPlayer time name =
    Player name (interpolate "%PUBLIC_URL%/img/family/{0}.jpg" [name]) time False

model0 = Unviewable <| GameSetup (List.map (initPlayer defaultTime) players0) defaultTime

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
    | ChangeDefault String


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


resetOne time player =
    { player | remainingTime = time, running = False }

resetPlayers game =
    { game | players = List.map (resetOne game.defaultTime) game.players }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Unviewable setup ->
            case msg of
                Resize width height ->
                    ( Viewable { width = width, height = height, players = setup.players, defaultTime = setup.defaultTime }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Viewable game ->
            case msg of
                Toggle player ->
                    ( Viewable { game | players = toggleOnly player game.players }, Cmd.none)

                StartAll ->
                    ( Viewable { game | players = runAll True game.players }, Cmd.none)

                StopAll ->
                    ( Viewable { game | players = runAll False game.players }, Cmd.none)

                ResetAll ->
                    ( Viewable <| resetPlayers game, Cmd.none)

                Tick _ ->
                    ( Viewable { game | players = tickRunning game.players }, Cmd.none)

                Resize width height ->
                    ( Viewable { game | height = height, width = width }, Cmd.none)

                ChangeDefault timeStr ->
                    case String.toInt timeStr of
                        Nothing ->
                            ( model, Cmd.none )

                        Just time ->
                            ( Viewable { game | defaultTime = time }, Cmd.none )


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

viewGame : GameView -> Html Msg
viewGame game =
    let btnFont = Font.size <| game.width // 25
    in
    El.layout [ El.height El.fill ] <|
        El.column [ El.height El.fill ]
            [ El.wrappedRow [] <| List.map (viewPlayer <| game.width * 10 // 61 ) game.players
            , El.row [ El.padding 20, El.spacing 10, El.width El.fill ]
                [ timerButton btnFont StartAll "START ALL"
                , timerButton btnFont StopAll "STOP ALL"
                ]
            , El.row [ El.alignBottom ]
                [ timerButton btnFont ResetAll "RESET ALL"
                , In.text [] { onChange = ChangeDefault
                             , placeholder = Nothing
                             , text = String.fromInt game.defaultTime
                             , label = In.labelHidden ""
                             }
                ]
            ]



view : Model -> Browser.Document Msg
view model =
    { title = "GoT Timer"
    , body = [ case model of
                   Unviewable _ ->
                       El.layout [] <| El.text "…"

                   Viewable game ->
                       viewGame game
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
