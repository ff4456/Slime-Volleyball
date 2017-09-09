module Geometrics exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Ball =
    { xVelocity : Float, yVelocity : Float, xAxis : Float, yAxis : Float, radius : Float }


type alias StaticBall =
    { xVelocity : Float, yVelocity : Float, xAxis : Float, yAxis : Float, radius : Float, counter : Int }


type alias Rectangle =
    { x : Float, y : Float, width : Float, height : Float }


type alias Msg =
    ()


view : Ball -> Svg msg
view ball =
    circle [ cx (toString (floor ball.xAxis)), cy (toString (floor ball.yAxis)), r (toString ball.radius) ] []


touch : Float -> Float -> Ball -> Bool
touch gravity height ball =
    let
        freeFall =
            ball.yVelocity + 0.5 * gravity

        yAxis =
            ball.yAxis + freeFall
    in
        yAxis >= height - ball.radius



{-
   bouncing : Ball -> Ball
   bouncing ball =
       let
           newX =
               ball.xAxis + ball.xVelocity

           newY =
               ball.yAxis + ball.yVelocity
       in
           if (newX > 750 || newX < 50) && (newY > 750 || newY < 50) then
               { ball
                   | xVelocity = negate ball.xVelocity
                   , yVelocity = negate ball.yVelocity
               }
           else if (newX > 750 || newX < 50) && (50 < newY && newY < 750) then
               { ball
                   | xVelocity = negate ball.xVelocity
               }
           else if (50 < newX && newX < 750) && (newY > 750 || newY < 50) then
               { ball
                   | yVelocity = negate ball.yVelocity
               }
           else
               ball
-}


bouncing : Ball -> Ball
bouncing ball =
    let
        newX =
            ball.xAxis + ball.xVelocity

        newY =
            ball.yAxis + ball.yVelocity
    in
        if (newX > (600 - ball.radius) || newX < ball.radius) && (newY > (500 - ball.radius) || newY < ball.radius) then
            { ball
                | xVelocity = negate ball.xVelocity
                , yVelocity = negate ball.yVelocity
            }
        else if (newX > (600 - ball.radius) || newX < ball.radius) && (ball.radius < newY && newY < (500 - ball.radius)) then
            { ball
                | xVelocity = negate ball.xVelocity
            }
        else if (ball.radius < newX && newX < (600 - ball.radius)) && (newY > (500 - ball.radius) || newY < ball.radius) then
            { ball
                | yVelocity = negate ball.yVelocity
            }
        else
            ball


closestP : Ball -> StaticBall -> ( Float, Float )
closestP movingBall staticBall =
    let
        c1 =
            movingBall.yVelocity * movingBall.xAxis - movingBall.xVelocity * movingBall.yAxis

        c2 =
            movingBall.xVelocity * staticBall.xAxis + movingBall.yVelocity * staticBall.yAxis

        det =
            movingBall.xVelocity ^ 2 + movingBall.yVelocity ^ 2

        cx =
            (movingBall.yVelocity * c1 + movingBall.xVelocity * c2) / det

        cy =
            (movingBall.yVelocity * c2 - movingBall.xVelocity * c1) / det
    in
        ( cx, cy )


update : Msg -> Ball -> Ball
update msg ball =
    bouncing ball
