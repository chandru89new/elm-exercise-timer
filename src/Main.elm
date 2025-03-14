port module Main exposing (..)

import Browser
import Html as H
import Html.Attributes as Attr
import Html.Events as Ev
import Process exposing (sleep)
import String exposing (fromInt, toInt)
import Task exposing (perform)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- TYPES


type alias Model =
    { repTime : Int
    , restTime : Int
    , ogRepTime : Int
    , ogRestTime : Int
    , status : Status
    , appError : Maybe String
    , countdown : Int
    }


type Status
    = Paused
    | RepRunning
    | RestRunning


type Msg
    = Start
    | Stop
    | UpdateRepTime String
    | UpdateRestTime String
    | ClearError
    | Tick
    | Countdown Int



-- INIT


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        ogRepTime =
            30

        ogRestTime =
            10
    in
    ( { ogRepTime = ogRepTime, ogRestTime = ogRestTime, repTime = ogRepTime, restTime = ogRestTime, status = Paused, appError = Nothing, countdown = 0 }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            let
                cd =
                    3
            in
            ( { model | status = RepRunning, countdown = cd }, Cmd.batch [ doCountdown cd, beep 0 ] )

        Stop ->
            ( resetModel model, Cmd.none )

        UpdateRepTime time ->
            case toInt time of
                Just int ->
                    ( { model | ogRepTime = zeroIfNegative int, repTime = zeroIfNegative int }, Cmd.none )

                _ ->
                    ( { model | appError = Just "Invalid rep time." }, Cmd.none )

        UpdateRestTime time ->
            case toInt time of
                Just int ->
                    ( { model | ogRestTime = zeroIfNegative int, restTime = zeroIfNegative int }, Cmd.none )

                _ ->
                    ( { model | appError = Just "Invalid rest time." }, Cmd.none )

        ClearError ->
            ( { model | appError = Nothing }, Cmd.none )

        Countdown count ->
            if count == 0 then
                ( { model | status = RepRunning, countdown = 0 }, Cmd.batch [ nextTick, boop 0 ] )

            else
                ( { model | countdown = count }, Cmd.batch [ beep 0, doCountdown count ] )

        Tick ->
            case model.status of
                Paused ->
                    ( model, Cmd.none )

                RepRunning ->
                    let
                        newRepTime =
                            zeroIfNegative (model.repTime - 1)
                    in
                    if model.repTime == 0 then
                        ( { model | repTime = model.ogRepTime, status = RestRunning }, nextTick )

                    else
                        ( { model | repTime = zeroIfNegative (model.repTime - 1) }
                        , Cmd.batch
                            [ nextTick
                            , if newRepTime == 0 then
                                buup 0

                              else if newRepTime < 3 then
                                beep 0

                              else if modBy 10 newRepTime == 0 then
                                bip 0

                              else
                                Cmd.none
                            ]
                        )

                RestRunning ->
                    let
                        newRestTime =
                            zeroIfNegative (model.restTime - 1)
                    in
                    if model.restTime == 0 then
                        ( { model | restTime = model.ogRestTime, status = RepRunning }, nextTick )

                    else
                        ( { model | restTime = zeroIfNegative (model.restTime - 1) }
                        , Cmd.batch
                            [ nextTick
                            , if newRestTime == 0 then
                                boop 0

                              else if newRestTime < 3 then
                                beep 0

                              else
                                Cmd.none
                            ]
                        )



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div
        [ Attr.class
            ("w-full p-5 h-screen flex flex-col gap-10 justify-center items-center bg-slate-800 text-white"
                ++ (if model.status == Paused then
                        ""

                    else
                        ""
                   )
            )
        ]
        [ viewRepTime model
        , viewRestTime model
        , playPauseBtn model
        ]


viewRepTime : Model -> H.Html Msg
viewRepTime model =
    case model.status of
        RestRunning ->
            H.div [ Attr.class "hidden" ] []

        _ ->
            H.div [ Attr.class "w-full" ]
                [ H.div [ Attr.class "w-full text-center mb-2 uppercase text-xs text-slate-400" ] [ H.text "Rep Time" ]
                , H.div [ Attr.class "w-full flex items-center justify-between border border-slate-700 rounded p-5" ]
                    [ decrementBtn (UpdateRepTime (String.fromInt (model.ogRepTime - 1))) (model.status == Paused)
                    , repTimeInput model
                    , incrementBtn (UpdateRepTime (String.fromInt (model.ogRepTime + 1))) (model.status == Paused)
                    ]
                ]


viewRestTime : Model -> H.Html Msg
viewRestTime model =
    case model.status of
        RepRunning ->
            H.div [ Attr.class "hidden" ] []

        _ ->
            H.div [ Attr.class "w-full" ]
                [ H.div [ Attr.class "w-full flex items-center justify-between border border-slate-700 rounded p-5" ]
                    [ decrementBtn (UpdateRestTime (String.fromInt (model.ogRestTime - 1))) (model.status == Paused)
                    , restTimeInput model
                    , incrementBtn (UpdateRestTime (String.fromInt (model.ogRestTime + 1))) (model.status == Paused)
                    ]
                , H.div [ Attr.class "w-full text-center mt-2 uppercase text-xs text-slate-400" ] [ H.text "Rest Time" ]
                ]


btnClass : String
btnClass =
    "rounded-full border border-slate-600 flex items-center justify-center p-10 w-10 h-10 cursor-pointer"


incrementBtn : msg -> Bool -> H.Html msg
incrementBtn action isVisible =
    H.div
        [ Ev.onClick action
        , Attr.class
            (btnClass
                ++ " text-3xl"
                ++ (if isVisible then
                        ""

                    else
                        " opacity-0"
                   )
            )
        ]
        [ H.text "+" ]


decrementBtn : msg -> Bool -> H.Html msg
decrementBtn action isVisible =
    H.div
        [ Ev.onClick action
        , Attr.class
            (btnClass
                ++ " text-3xl"
                ++ (if isVisible then
                        ""

                    else
                        " opacity-0"
                   )
            )
        ]
        [ H.text "-" ]


repTimeInput : Model -> H.Html Msg
repTimeInput model =
    H.div []
        [ H.div
            [ Ev.onInput UpdateRepTime
            , Attr.value (String.fromInt model.repTime)
            , Attr.disabled True
            , Attr.class "text-center text-5xl"
            ]
            [ H.text (String.fromInt model.repTime) ]
        ]


restTimeInput : Model -> H.Html Msg
restTimeInput model =
    H.div []
        [ H.div
            [ Ev.onInput UpdateRestTime
            , Attr.disabled True
            , Attr.value (String.fromInt model.restTime)
            , Attr.class "text-center text-xl text-slate-300"
            ]
            [ H.text (String.fromInt model.restTime) ]
        ]


playPauseBtn : Model -> H.Html Msg
playPauseBtn model =
    case model.status of
        Paused ->
            H.div [ Ev.onClick Start, Attr.class (btnClass ++ " !border-0 bg-green-700 text-white") ] [ H.text "Start" ]

        _ ->
            H.div [ Ev.onClick Stop, Attr.class (btnClass ++ " !border-0 bg-red-600 text-white") ]
                [ H.text
                    (if model.countdown > 0 then
                        String.fromInt model.countdown

                     else
                        "Stop"
                    )
                ]



-- UTILS


zeroIfNegative : Int -> Int
zeroIfNegative n =
    if n < 0 then
        0

    else
        n


nextTick : Cmd Msg
nextTick =
    perform (\_ -> Tick) (sleep 1000)


resetModel : Model -> Model
resetModel m =
    { m | repTime = m.ogRepTime, restTime = m.ogRestTime, status = Paused, appError = Nothing }


doCountdown : Int -> Cmd Msg
doCountdown count =
    perform (\_ -> Countdown (count - 1)) (sleep 1000)


port beep : Int -> Cmd msg


port boop : Int -> Cmd msg


port buup : Int -> Cmd msg


port bip : Int -> Cmd msg
