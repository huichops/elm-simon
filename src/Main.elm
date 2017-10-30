module Main exposing (Model, Msg, update, view, subscriptions, init)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing(Time, second)

import Task
import Array
import Random

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type Color
    = Red
    | Green
    | Blue
    | Yellow
    | Empty 

type alias Playing =
    { red : Bool
    , green : Bool
    , blue : Bool
    , yellow : Bool
    }

type alias Model =
    { sequence : Array.Array Color
    , time : Time
    , activeColor : Int
    , playingSequence : Bool
    , playing : Color
    , playIndex : Int
    , win : Bool
    }


type Msg
    = Move Color
    | NewMove Int
    | GenerateMove
    | Tick Time
    | PlayNext


mapColor n =
    case n of
        0 -> Red
        1 -> Green
        2 -> Blue
        3 -> Yellow
        _ -> Empty

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            { model | time = time, playing = Empty }
            |> update PlayNext
        Move color ->
            let
                nextColor = Array.get model.playIndex model.sequence
            in
                case nextColor of
                Nothing -> 
                    Debug.log("khe berga")
                    (model, Cmd.none)
                Just c ->
                    if c == color then
                        ({ model | playIndex = model.playIndex + 1 }, Cmd.none )
                    else
                        ({ model | win = False, playIndex = 0 }, Cmd.none )
        GenerateMove ->
          (model, Random.generate NewMove (Random.int 0 3))
        NewMove colorNumber ->
            let
                color = mapColor colorNumber
            in
            ({ model | sequence = (Array.push color model.sequence) }, Cmd.none)
        PlayNext ->
            let 
                len = Array.length(model.sequence)
                nextIndex = model.activeColor
                nextColor = Array.get nextIndex model.sequence
            in
                if model.playingSequence == True then
                    case nextColor of
                        Nothing -> 
                            ({ model | playingSequence = False, playing = Empty, activeColor = 0 }, Cmd.none)
                        Just c ->
                            Debug.log(toString c)
                            ({ model | playing = c, activeColor = nextIndex + 1 }, Cmd.none )
                else
                    (model, Cmd.none)


view : Model -> Html Msg
view model =
    let
        vc =  viewColor model.playing
    in
        case model.win of
            False ->
                div [] [ text "PINCHE PENDEJO MECO QLO" ]
            True ->
                div []
                [ vc Red
                , vc Yellow
                , vc Green
                , vc Blue
                ]

mapClass c =
    case c of
        Red -> "red"
        Green -> "green"
        Blue -> "blue"
        Yellow -> "yellow"
        Empty -> ""

viewColor activeColor color =
    let
        klass = mapClass color
    in
        div [ classList [ (klass, True) , ("active", activeColor == color ) ] , onClick (Move color) ] [ text klass ]

start msg =
    Task.succeed msg
    |> Task.perform identity

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every ( second / 2 ) Tick

initialModel = Model (Array.fromList [ Red, Green, Yellow ]) 0 0 True Empty 0 True


init : (Model, Cmd Msg)
init = 
    ( initialModel
    , start PlayNext
    )
