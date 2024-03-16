module PoopyHead exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Time exposing (..)
import Keyboard exposing (..)
import Geometrics
import Physics201


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { slimeA : Geometrics.StaticBall, slimeB : Geometrics.StaticBall, ball : Geometrics.Ball }


init : ( Model, Cmd Msg )
init =
    ( { slimeA = { xVelocity = 0, yVelocity = 0, xAxis = 400, yAxis = 600, radius = 56, counter = 0 }
      , slimeB = { xVelocity = 0, yVelocity = 0, xAxis = 1220, yAxis = 600, radius = 56, counter = 0 }
      , ball = { xVelocity = 0, yVelocity = 0, xAxis = 400, yAxis = 200, radius = 14 }
      }
    , Cmd.none
    )


type Msg
    = GetDown Keyboard.KeyCode
    | GetUp Keyboard.KeyCode
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        slimeA =
            model.slimeA

        slimeB =
            model.slimeB

        ball =
            model.ball
    in
        case msg of
            GetDown signal ->
                if signal == 65 || signal == 68 || signal == 87 then
                    let
                        slimeA_xVelocity =
                            if signal == 65 then
                                -10
                            else if signal == 68 then
                                10
                            else
                                slimeA.xVelocity

                        slimeA_yVelocity =
                            if signal == 87 then
                                if slimeA.yAxis /= 600 then
                                    slimeA.yVelocity
                                else
                                    -9
                            else
                                slimeA.yVelocity

                        newSlimeA =
                            { slimeA | xVelocity = slimeA_xVelocity, yVelocity = slimeA_yVelocity }

                        newModel =
                            { model | slimeA = newSlimeA }
                    in
                        ( newModel, Cmd.none )
                else if signal == 37 || signal == 38 || signal == 39 then
                    let
                        slimeB_xVelocity =
                            if signal == 37 then
                                -10
                            else if signal == 39 then
                                10
                            else
                                slimeB.xVelocity

                        slimeB_yVelocity =
                            if signal == 38 then
                                if slimeB.yAxis /= 600 then
                                    slimeB.yVelocity
                                else
                                    -9
                            else
                                slimeB.yVelocity

                        newSlimeB =
                            { slimeB | xVelocity = slimeB_xVelocity, yVelocity = slimeB_yVelocity }

                        newModel =
                            { model | slimeB = newSlimeB }
                    in
                        ( newModel, Cmd.none )
                else
                    ( model, Cmd.none )

            GetUp signal ->
                if signal == 65 || signal == 68 then
                    let
                        slimeA_xVelocity =
                            if slimeA.xVelocity == 10 then
                                if signal == 65 then
                                    slimeA.xVelocity
                                else if signal == 68 then
                                    0
                                else
                                    slimeA.xVelocity
                            else if slimeA.xVelocity == -10 then
                                if signal == 65 then
                                    0
                                else if signal == 68 then
                                    slimeA.xVelocity
                                else
                                    slimeA.xVelocity
                            else
                                slimeA.xVelocity

                        newSlimeA =
                            { slimeA | xVelocity = slimeA_xVelocity }

                        newModel =
                            { model | slimeA = newSlimeA }
                    in
                        ( newModel, Cmd.none )
                else if signal == 37 || signal == 39 then
                    let
                        slimeB_xVelocity =
                            if slimeB.xVelocity == 10 then
                                if signal == 37 then
                                    slimeB.xVelocity
                                else if signal == 39 then
                                    0
                                else
                                    slimeB.xVelocity
                            else if slimeB.xVelocity == -10 then
                                if signal == 37 then
                                    0
                                else if signal == 39 then
                                    slimeB.xVelocity
                                else
                                    slimeB.xVelocity
                            else
                                slimeB.xVelocity

                        newSlimeB =
                            { slimeB | xVelocity = slimeB_xVelocity }

                        newModel =
                            { model | slimeB = newSlimeB }
                    in
                        ( newModel, Cmd.none )
                else
                    ( model, Cmd.none )

            Tick ->
                let
                    touchGround =
                        Geometrics.touch 0.7 600 ball
                in
                    if touchGround then
                        (if ball.xAxis <= 800 then
                            let
                                ( oldModel, msg ) =
                                    init

                                oldSlimeA =
                                    oldModel.slimeA

                                oldSlimeB =
                                    oldModel.slimeB

                                newSlimeA =
                                    { oldSlimeA | counter = slimeA.counter }

                                newSlimeB =
                                    { oldSlimeB | counter = slimeB.counter + 1 }

                                newModel =
                                    { oldModel | slimeA = newSlimeA, slimeB = newSlimeB }
                            in
                                if newSlimeB.counter >= 5 then
                                    init
                                else
                                    ( newModel, Cmd.none )
                         else
                            let
                                ( oldModel, msg ) =
                                    init

                                oldSlimeA =
                                    oldModel.slimeA

                                oldSlimeB =
                                    oldModel.slimeB

                                oldBall =
                                    oldModel.ball

                                newSlimeA =
                                    { oldSlimeA | counter = slimeA.counter + 1 }

                                newSlimeB =
                                    { oldSlimeB | counter = slimeB.counter }

                                newBall =
                                    { oldBall | xAxis = 1220 }

                                newModel =
                                    { oldModel | slimeA = newSlimeA, slimeB = newSlimeB, ball = newBall }
                            in
                                if newSlimeA.counter >= 5 then
                                    init
                                else
                                    ( newModel, Cmd.none )
                        )
                    else
                        (if ball.xAxis < (800 - ball.radius) then
                            let
                                ( newBall, newSlimeA ) =
                                    Physics201.response 0.7 0 ( 0, 800 ) ( ball, slimeA )

                                newSlimeB =
                                    Physics201.beingAlone 0.7 ( 820, 1620 ) slimeB

                                newModel =
                                    { model | slimeA = newSlimeA, slimeB = newSlimeB, ball = newBall }
                            in
                                ( newModel, Cmd.none )
                         else if ball.xAxis > (820 + ball.radius) then
                            let
                                ( newBall, newSlimeB ) =
                                    Physics201.response 0.7 0 ( 820, 1620 ) ( ball, slimeB )

                                newSlimeA =
                                    Physics201.beingAlone 0.7 ( 0, 800 ) slimeA

                                newModel =
                                    { model | slimeA = newSlimeA, slimeB = newSlimeB, ball = newBall }
                            in
                                ( newModel, Cmd.none )
                         else
                            let
                                rectangle =
                                    { x = 800, y = 400, width = 20, height = 200 }

                                netCollision =
                                    (Physics201.wallCollision 0.7 1 ( rectangle, ball ))

                                ( ( newRectangle, newBall ), whatever ) =
                                    Physics201.responsePlus 0.7 0 netCollision ( rectangle, ball )

                                newSlimeA =
                                    Physics201.beingAlone 0.7 ( 0, 800 ) slimeA

                                newSlimeB =
                                    Physics201.beingAlone 0.7 ( 820, 1620 ) slimeB

                                newModel =
                                    { model | slimeA = newSlimeA, slimeB = newSlimeB, ball = newBall }
                            in
                                ( newModel, Cmd.none )
                        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (20 * millisecond) (always Tick)
        , Keyboard.downs GetDown
        , Keyboard.ups GetUp
        ]


view : Model -> Html Msg
view model =
    let
        slimeA =
            model.slimeA

        slimeB =
            model.slimeB

        ball =
            model.ball

        k1 =
            (negate (ball.yAxis - slimeA.yAxis + 24)) / (ball.xAxis - slimeA.xAxis - 23)

        k2 =
            (negate (ball.yAxis - slimeB.yAxis + 24)) / (ball.xAxis - slimeB.xAxis + 23)

        x1 =
            if ball.yAxis <= (slimeA.yAxis - 24) then
                (if k1 > 0 then
                    6 / (sqrt (k1 ^ 2 + 1))
                 else
                    negate (6 / (sqrt (k1 ^ 2 + 1)))
                )
            else
                (if k1 < 0 then
                    6 / (sqrt (k1 ^ 2 + 1))
                 else
                    negate (6 / (sqrt (k1 ^ 2 + 1)))
                )

        x2 =
            if ball.yAxis <= (slimeB.yAxis - 24) then
                (if k2 > 0 then
                    6 / (sqrt (k2 ^ 2 + 1))
                 else
                    negate (6 / (sqrt (k2 ^ 2 + 1)))
                )
            else
                (if k2 < 0 then
                    6 / (sqrt (k2 ^ 2 + 1))
                 else
                    negate (6 / (sqrt (k2 ^ 2 + 1)))
                )

        ( slimeAColor1, slimeAColor2, slimeAColor3, slimeAColor4, slimeAColor5 ) =
            if slimeA.counter == 0 then
                ( 255, 255, 255, 255, 255 )
            else if slimeA.counter == 1 then
                ( 0, 255, 255, 255, 255 )
            else if slimeA.counter == 2 then
                ( 0, 0, 255, 255, 255 )
            else if slimeA.counter == 3 then
                ( 0, 0, 0, 255, 255 )
            else if slimeA.counter == 4 then
                ( 0, 0, 0, 0, 255 )
            else
                ( 0, 0, 0, 0, 0 )

        ( slimeBColor1, slimeBColor2, slimeBColor3, slimeBColor4, slimeBColor5 ) =
            if slimeB.counter == 0 then
                ( 255, 255, 255, 255, 255 )
            else if slimeB.counter == 1 then
                ( 0, 255, 255, 255, 255 )
            else if slimeB.counter == 2 then
                ( 0, 0, 255, 255, 255 )
            else if slimeB.counter == 3 then
                ( 0, 0, 0, 255, 255 )
            else if slimeB.counter == 4 then
                ( 0, 0, 0, 0, 255 )
            else
                ( 0, 0, 0, 0, 0 )
    in
        svg
            [ Svg.Attributes.width "1620", Svg.Attributes.height "650", viewBox "0 0 1620 650" ]
            ([ rect [ x "0", y "0", Svg.Attributes.width "1620", Svg.Attributes.height "600", fill "rgba(133,193,233,0.5)" ] []
             , rect [ x "800", y "400", Svg.Attributes.width "20", Svg.Attributes.height "200", fill "white" ] []
             , rect [ x "0", y "600", Svg.Attributes.width "1620", Svg.Attributes.height "50", fill "rgba(120,66,18,0.7)" ] []
             , Svg.path [ d ("M" ++ (toString (slimeA.xAxis - slimeA.radius)) ++ " " ++ (toString slimeA.yAxis) ++ " a1 1 0 0 1 " ++ (toString (2 * slimeA.radius)) ++ " 0"), fill "rgb(44,62,80)" ] []
             , Svg.path [ d ("M" ++ (toString (slimeB.xAxis - slimeB.radius)) ++ " " ++ (toString slimeB.yAxis) ++ " a1 1 0 0 1 " ++ (toString (2 * slimeB.radius)) ++ " 0"), fill "rgb(46,204,113)" ] []
             , circle [ cx (toString (floor ball.xAxis)), cy (toString (floor ball.yAxis)), r (toString ball.radius) ] []
             , circle [ cx (toString (floor slimeA.xAxis + 23)), cy (toString (floor slimeA.yAxis - 24)), r (toString 12), fill "rgba(255,255,255,0.8)" ] []
             , circle [ cx (toString (floor slimeB.xAxis - 23)), cy (toString (floor slimeB.yAxis - 24)), r (toString 12), fill "rgba(255,255,255,0.8)" ] []
             , circle [ cx (toString (floor (slimeA.xAxis + 23 + x1))), cy (toString (floor (slimeA.yAxis - 24 - k1 * x1))), r (toString 6) ] []
             , circle [ cx (toString (floor (slimeB.xAxis - 23 + x2))), cy (toString (floor (slimeB.yAxis - 24 - k2 * x2))), r (toString 6) ] []
             , circle [ cx "30", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeAColor1) ++ ",1)") ] []
             , circle [ cx "70", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeAColor2) ++ ",1)") ] []
             , circle [ cx "110", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeAColor3) ++ ",1)") ] []
             , circle [ cx "150", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeAColor4) ++ ",1)") ] []
             , circle [ cx "190", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeAColor5) ++ ",1)") ] []
             , circle [ cx "1590", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeBColor1) ++ ",1)") ] []
             , circle [ cx "1550", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeBColor2) ++ ",1)") ] []
             , circle [ cx "1510", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeBColor3) ++ ",1)") ] []
             , circle [ cx "1470", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeBColor4) ++ ",1)") ] []
             , circle [ cx "1430", cy "30", r "15", stroke "black", strokeWidth "3", fill ("rgba(255,255," ++ (toString slimeBColor5) ++ ",1)") ] []
             ]
            )
