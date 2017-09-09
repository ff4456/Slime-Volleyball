module Physics201 exposing (..)

import Geometrics
import List exposing (..)


type alias TwoBalls =
    ( Geometrics.Ball, Geometrics.StaticBall )


type alias TwoObjects =
    ( Geometrics.Rectangle, Geometrics.Ball )


type alias Range =
    ( Float, Float )


type Touch
    = Side
    | Top
    | Corner
    | NoTouch


touch : Float -> TwoBalls -> Bool
touch gravity ( moving, static ) =
    let
        freeFall =
            moving.yVelocity + 0.5 * gravity

        yAxis =
            moving.yAxis + freeFall
    in
        yAxis >= 800 - moving.radius


beingAlone : Float -> ( Float, Float ) -> Geometrics.StaticBall -> Geometrics.StaticBall
beingAlone gravity ( leftBound, rightBound ) singleSlime =
    let
        freeFall =
            singleSlime.yVelocity + 0.5 * gravity

        vertDistance =
            600 - singleSlime.yAxis

        vertCollision =
            vertDistance - freeFall <= 0

        yVelocity =
            if vertCollision then
                0
            else
                singleSlime.yVelocity + gravity

        yAxis =
            if vertCollision then
                600
            else
                singleSlime.yAxis + freeFall

        xAxis =
            if singleSlime.xAxis + singleSlime.xVelocity <= (singleSlime.radius + leftBound) then
                singleSlime.radius + leftBound
            else if singleSlime.xAxis + singleSlime.xVelocity >= rightBound - singleSlime.radius then
                rightBound - singleSlime.radius
            else
                singleSlime.xAxis + singleSlime.xVelocity

        aloneSlime =
            { singleSlime
                | xAxis = xAxis
                , yAxis = yAxis
                , yVelocity = yVelocity
            }
    in
        aloneSlime


wallCollision2 : Float -> Float -> TwoObjects -> Touch
wallCollision2 gravity timeLeft ( rectangle, ball ) =
    let
        centerX =
            rectangle.x + 0.5 * rectangle.width

        centerY =
            rectangle.y + 0.5 * rectangle.height

        newX =
            ball.xAxis + ball.xVelocity * timeLeft

        newY =
            ball.yAxis + ball.yVelocity * timeLeft + 0.5 * gravity * timeLeft ^ 2

        rcDistX =
            abs (newX - centerX)

        rcDistY =
            abs (newY - centerY)

        rcDistCorner =
            sqrt (newX - rectangle.width / 2) ^ 2 + (newY - rectangle.height / 2) ^ 2
    in
        if ball.xAxis <= (rectangle.x - ball.radius) then
            let
                conditionSide =
                    rcDistX <= (rectangle.width / 2 + ball.radius) && rcDistY <= (rectangle.height / 2)
            in
                (if conditionSide then
                    Side
                 else
                    NoTouch
                )
        else if ball.xAxis >= (rectangle.x + rectangle.width + ball.radius) then
            let
                conditionSide =
                    rcDistX <= (rectangle.width / 2 + ball.radius) && rcDistY <= (rectangle.height / 2)
            in
                (if conditionSide then
                    Side
                 else
                    NoTouch
                )
        else
            let
                conditionTop =
                    rcDistY <= (rectangle.height / 2 + ball.radius) && rcDistX <= (rectangle.width / 2)

                conditionCorner =
                    rcDistX < (rectangle.width / 2 + ball.radius) && rcDistY < (rectangle.height / 2 + ball.radius)
            in
                (if conditionCorner then
                    Corner
                 else if conditionTop then
                    Top
                 else
                    NoTouch
                )


wallCollision : Float -> Float -> TwoObjects -> Touch
wallCollision gravity timeLeft ( rectangle, ball ) =
    let
        newX =
            ball.xAxis + ball.xVelocity * timeLeft
    in
        if (ball.xAxis <= rectangle.x - ball.radius && newX < rectangle.x - ball.radius) || (ball.xAxis >= rectangle.x + rectangle.width + ball.radius && newX > rectangle.x + rectangle.width + ball.radius) then
            NoTouch
        else
            let
                hitLeftSide =
                    if ball.xAxis < rectangle.x - ball.radius && ball.xVelocity > 0 then
                        (rectangle.x - ball.radius - ball.xAxis) / ball.xVelocity
                    else if newX < rectangle.x - ball.radius && ball.xVelocity < 0 then
                        (ball.xAxis - rectangle.x + ball.radius) / ball.xVelocity
                    else
                        27

                leftHitTime =
                    if hitLeftSide == 27 then
                        27
                    else
                        (if hitLeftSide > timeLeft then
                            27
                         else if (ball.yAxis + ball.yVelocity * hitLeftSide + 0.5 * gravity * hitLeftSide ^ 2) < rectangle.y then
                            27
                         else
                            hitLeftSide
                        )

                hitRightSide =
                    if ball.xAxis > rectangle.x + rectangle.width + ball.radius && ball.xVelocity < 0 then
                        (ball.xAxis - rectangle.x - rectangle.width - ball.radius) / (negate ball.xVelocity)
                    else if newX > rectangle.x + rectangle.width + ball.radius && ball.xVelocity > 0 then
                        (rectangle.x + rectangle.width + ball.radius - ball.xAxis) / ball.xVelocity
                    else
                        27

                rightHitTime =
                    if hitRightSide == 27 then
                        27
                    else
                        (if hitRightSide > timeLeft then
                            27
                         else if (ball.yAxis + ball.yVelocity * hitRightSide + 0.5 * gravity * hitRightSide ^ 2) < rectangle.y then
                            27
                         else
                            hitRightSide
                        )

                hitTop =
                    let
                        a =
                            0.5 * gravity

                        b =
                            ball.yVelocity

                        c =
                            ball.yAxis - rectangle.y + ball.radius

                        delta =
                            b ^ 2 - 4 * a * c

                        solA =
                            (((negate b) - (sqrt delta)) / (2 * a))

                        solB =
                            (((negate b) + (sqrt delta)) / (2 * a))
                    in
                        if 0 < solA && solB <= timeLeft then
                            solA
                        else if 0 < solA && solA <= timeLeft && solB > timeLeft then
                            solA
                        else if solA < 0 && 0 < solB && solB <= timeLeft then
                            solB
                        else
                            27

                topHitTime =
                    if hitTop == 27 then
                        27
                    else
                        (if (ball.xAxis + ball.xVelocity * hitTop) <= rectangle.x + rectangle.width && (ball.xAxis + ball.xVelocity * hitTop) >= rectangle.x then
                            hitTop
                         else
                            27
                        )

                leftCorner =
                    rectangle

                rightCorner =
                    { rectangle | x = rectangle.x + rectangle.width }

                leftCornerHitTime =
                    approxSolutionPlus gravity ( 0, timeLeft ) ( leftCorner, ball )

                rightCornerHitTime =
                    approxSolutionPlus gravity ( 0, timeLeft ) ( rightCorner, ball )

                timeList =
                    [ leftHitTime, rightHitTime, topHitTime, leftCornerHitTime, rightCornerHitTime ]

                shortestTime =
                    minimum timeList
            in
                case shortestTime of
                    Nothing ->
                        NoTouch

                    Just hitTimeee ->
                        if hitTimeee == 27 then
                            NoTouch
                        else
                            (if hitTimeee == leftHitTime then
                                Side
                             else if hitTimeee == rightHitTime then
                                Side
                             else if hitTimeee == topHitTime then
                                Top
                             else if hitTimeee == leftCornerHitTime then
                                Corner
                             else
                                Corner
                            )


responsePlus : Float -> Float -> Touch -> TwoObjects -> ( TwoObjects, Float )
responsePlus gravity timeUsed collisionResult ( rectangle, ball ) =
    case collisionResult of
        NoTouch ->
            let
                newX =
                    ball.xAxis + ball.xVelocity * (1 - timeUsed)

                newY =
                    ball.yAxis + ball.yVelocity * (1 - timeUsed) + 0.5 * gravity * (1 - timeUsed) ^ 2

                newYvelocity =
                    ball.yVelocity + gravity * (1 - timeUsed)

                newBall =
                    { ball | xAxis = newX, yAxis = newY, yVelocity = newYvelocity }
            in
                ( ( rectangle, newBall ), 1 )

        Side ->
            if ball.xAxis <= (rectangle.x - ball.radius) then
                let
                    horzDist =
                        rectangle.x - ball.xAxis - ball.radius

                    hitTime =
                        (horzDist / ball.xVelocity)

                    newTimeUsed =
                        timeUsed + hitTime

                    newXvelocity =
                        negate ball.xVelocity

                    newYvelocity =
                        ball.yVelocity + gravity * hitTime

                    newX =
                        rectangle.x - ball.radius

                    newY =
                        ball.yAxis + ball.yVelocity * hitTime + 0.5 * gravity * hitTime ^ 2

                    newBall =
                        { ball
                            | xAxis = newX
                            , yAxis = newY
                            , xVelocity = newXvelocity
                            , yVelocity = newYvelocity
                        }
                in
                    ( ( rectangle, newBall ), newTimeUsed )
            else if ball.xAxis >= (rectangle.x + rectangle.width + ball.radius) then
                let
                    horzDist =
                        ball.xAxis - rectangle.x - rectangle.width - ball.radius

                    hitTime =
                        horzDist / (negate ball.xVelocity)

                    newTimeUsed =
                        timeUsed + hitTime

                    newXvelocity =
                        negate ball.xVelocity

                    newYvelocity =
                        ball.yVelocity + gravity * hitTime

                    newX =
                        rectangle.x + rectangle.width + ball.radius

                    newY =
                        ball.yAxis + ball.yVelocity * hitTime + 0.5 * gravity * hitTime ^ 2

                    newBall =
                        { ball
                            | xAxis = newX
                            , yAxis = newY
                            , xVelocity = newXvelocity
                            , yVelocity = newYvelocity
                        }
                in
                    ( ( rectangle, newBall ), newTimeUsed )
            else
                ( ( rectangle, ball ), 0 )

        Top ->
            let
                a =
                    0.5 * gravity

                b =
                    ball.yVelocity

                c =
                    ball.yAxis - rectangle.y + ball.radius

                delta =
                    b ^ 2 - 4 * a * c

                solA =
                    (((negate b) - (sqrt delta)) / (2 * a))

                solB =
                    (((negate b) + (sqrt delta)) / (2 * a))

                hitTime =
                    if 0 < solA && solB <= (1 - timeUsed) then
                        solA
                    else if 0 < solA && solA <= (1 - timeUsed) && solB > (1 - timeUsed) then
                        solA
                    else if solA < 0 && 0 < solB && solB <= (1 - timeUsed) then
                        solB
                    else
                        27
            in
                if hitTime /= 27 then
                    let
                        newTimeUsed =
                            timeUsed + hitTime

                        newYvelocity =
                            negate (ball.yVelocity + gravity * hitTime)

                        newX =
                            ball.xAxis + ball.xVelocity * hitTime

                        newY =
                            rectangle.y - ball.radius

                        newBall =
                            { ball
                                | xAxis = newX
                                , yAxis = newY
                                , yVelocity = newYvelocity
                            }
                    in
                        responsePlus gravity newTimeUsed NoTouch ( rectangle, newBall )
                else
                    ( ( rectangle, ball ), 27 )

        Corner ->
            let
                leftCorner =
                    rectangle

                rightCorner =
                    { rectangle | x = rectangle.x + rectangle.width }

                solA =
                    approxSolutionPlus gravity ( 0, 1 - timeUsed ) ( leftCorner, ball )

                solB =
                    approxSolutionPlus gravity ( 0, 1 - timeUsed ) ( rightCorner, ball )

                hitTime =
                    if solA /= 27 && solB == 27 then
                        solA
                    else if solA == 27 && solB /= 27 then
                        solB
                    else if solA /= 27 && solB /= 27 && solA < solB then
                        solA
                    else if solA /= 27 && solB /= 27 && solB < solA then
                        solB
                    else
                        27
            in
                if hitTime == 27 then
                    ( ( rectangle, ball ), 27 )
                else
                    let
                        newTimeUsed =
                            timeUsed + hitTime

                        newX =
                            ball.xAxis + ball.xVelocity * hitTime

                        newY =
                            ball.yAxis + ball.yVelocity * hitTime + 0.5 * gravity * hitTime ^ 2
                    in
                        (if hitTime == solA then
                            let
                                distX =
                                    ball.xAxis - leftCorner.x

                                distY =
                                    ball.yAxis - leftCorner.y

                                c =
                                    (negate 2) * (ball.xVelocity * distX + (ball.yVelocity + gravity * hitTime) * distY) / (distX ^ 2 + distY ^ 2)

                                newXvelocity =
                                    ball.xVelocity + c * distX

                                newYvelocity =
                                    ball.yVelocity + c * distY

                                newBall =
                                    { ball
                                        | xAxis = newX
                                        , yAxis = newY
                                        , xVelocity = newXvelocity
                                        , yVelocity = newYvelocity
                                    }

                                finalX =
                                    newBall.xAxis + newBall.xVelocity * (1 - newTimeUsed)

                                finalY =
                                    newBall.yAxis + newBall.yVelocity * (1 - newTimeUsed) + 0.5 * gravity * (1 - newTimeUsed) ^ 2

                                finalYvelocity =
                                    newBall.yVelocity + gravity * (1 - newTimeUsed)

                                finalNewBall =
                                    { newBall
                                        | xAxis = finalX
                                        , yAxis = finalY
                                        , yVelocity = finalYvelocity
                                    }
                            in
                                ( ( rectangle, finalNewBall ), newTimeUsed )
                         else
                            let
                                distX =
                                    ball.xAxis - rightCorner.x

                                distY =
                                    ball.yAxis - rightCorner.y

                                c =
                                    (negate 2) * (ball.xVelocity * distX + (ball.yVelocity + gravity * hitTime) * distY) / (distX ^ 2 + distY ^ 2)

                                newXvelocity =
                                    ball.xVelocity + c * distX

                                newYvelocity =
                                    ball.yVelocity + c * distY

                                newBall =
                                    { ball
                                        | xAxis = newX
                                        , yAxis = newY
                                        , xVelocity = newXvelocity
                                        , yVelocity = newYvelocity
                                    }

                                finalX =
                                    newBall.xAxis + newBall.xVelocity * (1 - newTimeUsed)

                                finalY =
                                    newBall.yAxis + newBall.yVelocity * (1 - newTimeUsed) + 0.5 * gravity * (1 - newTimeUsed) ^ 2

                                finalYvelocity =
                                    newBall.yVelocity + gravity * (1 - newTimeUsed)

                                finalNewBall =
                                    { newBall
                                        | xAxis = finalX
                                        , yAxis = finalY
                                        , yVelocity = finalYvelocity
                                    }
                            in
                                ( ( rectangle, finalNewBall ), newTimeUsed )
                        )


approxSolution : Float -> ( Range, TwoBalls ) -> Float
approxSolution gravity ( ( lowerLimit, upperLimit ), ( moving, static ) ) =
    if upperLimit - lowerLimit <= 0.0000001 then
        (lowerLimit + upperLimit) / 2
    else
        let
            lowerOutput =
                (\t -> sqrt ((moving.xAxis - static.xAxis + (moving.xVelocity - static.xVelocity) * t) ^ 2 + (moving.yAxis - static.yAxis + (moving.yVelocity * t + 0.5 * gravity * t ^ 2)) ^ 2) - sqrt ((moving.radius + static.radius) ^ 2))
                    lowerLimit

            midOutput =
                (\t -> sqrt ((moving.xAxis - static.xAxis + (moving.xVelocity - static.xVelocity) * t) ^ 2 + (moving.yAxis - static.yAxis + (moving.yVelocity * t + 0.5 * gravity * t ^ 2)) ^ 2) - sqrt ((moving.radius + static.radius) ^ 2))
                    ((lowerLimit + upperLimit)
                        / 2
                    )

            upperOutput =
                (\t -> sqrt ((moving.xAxis - static.xAxis + (moving.xVelocity - static.xVelocity) * t) ^ 2 + (moving.yAxis - static.yAxis + (moving.yVelocity * t + 0.5 * gravity * t ^ 2)) ^ 2) - sqrt ((moving.radius + static.radius) ^ 2))
                    upperLimit
        in
            if (lowerOutput > 0 && midOutput < 0) || (lowerOutput < 0 && midOutput > 0) then
                approxSolution gravity ( ( lowerLimit, (lowerLimit + upperLimit) / 2 ), ( moving, static ) )
            else if (midOutput > 0 && upperOutput < 0) || (midOutput < 0 && upperOutput > 0) then
                approxSolution gravity ( ( (lowerLimit + upperLimit) / 2, upperLimit ), ( moving, static ) )
            else
                42


approxSolutionPlus : Float -> Range -> TwoObjects -> Float
approxSolutionPlus gravity ( lowerLimit, upperLimit ) ( rectangle, ball ) =
    if upperLimit - lowerLimit <= 0.0000001 then
        (lowerLimit + upperLimit) / 2
    else
        let
            lowerOutput =
                (\t -> (sqrt (((ball.xAxis - rectangle.x) + ball.xVelocity * t) ^ 2 + ((ball.yAxis - rectangle.y) + ball.yVelocity * t + 0.5 * gravity * t ^ 2) ^ 2)) - ball.radius)
                    lowerLimit

            midOutput =
                (\t -> (sqrt (((ball.xAxis - rectangle.x) + ball.xVelocity * t) ^ 2 + ((ball.yAxis - rectangle.y) + ball.yVelocity * t + 0.5 * gravity * t ^ 2) ^ 2)) - ball.radius)
                    ((lowerLimit + upperLimit)
                        / 2
                    )

            upperOutput =
                (\t -> (sqrt (((ball.xAxis - rectangle.x) + ball.xVelocity * t) ^ 2 + ((ball.yAxis - rectangle.y) + ball.yVelocity * t + 0.5 * gravity * t ^ 2) ^ 2)) - ball.radius)
                    upperLimit
        in
            if (lowerOutput > 0 && midOutput < 0) || (lowerOutput < 0 && midOutput > 0) then
                approxSolutionPlus gravity ( lowerLimit, (lowerLimit + upperLimit) / 2 ) ( rectangle, ball )
            else if (midOutput > 0 && upperOutput < 0) || (midOutput < 0 && upperOutput > 0) then
                approxSolutionPlus gravity ( (lowerLimit + upperLimit) / 2, upperLimit ) ( rectangle, ball )
            else
                27


actualSolution : Float -> Range -> TwoBalls -> Float
actualSolution gravity ( lowerLimit, upperLimit ) ( moving, static ) =
    let
        b =
            2 * (moving.xAxis - static.xAxis) * (moving.xVelocity - static.xVelocity) + 2 * (moving.yAxis - static.yAxis) * (moving.yVelocity - static.yVelocity)

        a =
            (moving.xVelocity - static.xVelocity) ^ 2 + (moving.yVelocity - static.yVelocity) ^ 2

        c =
            (moving.xAxis - static.xAxis) ^ 2 + (moving.yAxis - static.yAxis) ^ 2 - (moving.radius + static.radius) ^ 2

        delta =
            b ^ 2 - 4 * a * c
    in
        if delta < 0 then
            42
        else if delta == 0 then
            (negate b) / (2 * a)
        else
            let
                solA =
                    (((negate b) - (sqrt delta)) / (2 * a))

                solB =
                    (((negate b) + (sqrt delta)) / (2 * a))

                timeInterval =
                    ( lowerLimit, upperLimit )
            in
                (if solA >= lowerLimit && solB <= upperLimit then
                    solA
                 else if (solA >= lowerLimit && solA <= upperLimit) && solB > upperLimit then
                    solA
                 else if (solB >= lowerLimit && solB <= upperLimit) && solA < lowerLimit then
                    solB
                 else
                    42
                )


possibleCollision : Float -> Float -> ( Float, Float ) -> TwoBalls -> ( ( Bool, Bool ), TwoBalls )
possibleCollision gravity timeLeft ( leftBound, rightBound ) ( moving, static ) =
    let
        moving_freeFall =
            moving.yVelocity * timeLeft + 0.5 * gravity * timeLeft ^ 2

        moving_yVelocity =
            moving.yVelocity + gravity * timeLeft

        moving_yAxis =
            moving.yAxis + moving_freeFall

        moving_xVelocity =
            if moving.xAxis + moving.xVelocity * timeLeft < moving.radius || moving.xAxis + moving.xVelocity * timeLeft > 1620 - moving.radius then
                negate moving.xVelocity
            else
                moving.xVelocity

        moving_xAxis =
            if moving.xAxis + moving.xVelocity * timeLeft < moving.radius then
                (abs ((moving.xAxis - moving.radius) + moving.xVelocity * timeLeft)) + moving.radius
            else if moving.xAxis + moving.xVelocity * timeLeft > 1620 - moving.radius then
                1620 - (moving.xVelocity * timeLeft - (1620 - moving.radius - moving.xAxis)) - moving.radius
            else
                moving.xAxis + moving.xVelocity * timeLeft

        --updated position of the moving ball after a frame if no collision happened
        alphaMoving =
            { moving
                | xAxis = moving_xAxis
                , yAxis = moving_yAxis
                , xVelocity = moving_xVelocity
                , yVelocity = moving_yVelocity
            }

        static_freeFall =
            static.yVelocity * timeLeft + 0.5 * gravity * timeLeft ^ 2

        static_vertDistance =
            600 - static.yAxis

        static_vertCollision =
            static_vertDistance - static_freeFall <= 0

        static_yVelocity =
            if static_vertCollision then
                0
            else
                static.yVelocity + gravity * timeLeft

        static_yAxis =
            if static_vertCollision then
                600
            else
                static.yAxis + static_freeFall

        static_xAxis =
            if static.xAxis + static.xVelocity * timeLeft <= (leftBound + static.radius) then
                leftBound + static.radius
            else if static.xAxis + static.xVelocity * timeLeft >= rightBound - static.radius then
                rightBound - static.radius
            else
                static.xAxis + static.xVelocity * timeLeft

        --updated position of the static ball after a frame
        alphaStatic =
            { static
                | xAxis = static_xAxis
                , yAxis = static_yAxis
                , yVelocity = static_yVelocity
            }

        distanceSq =
            (alphaMoving.xAxis - alphaStatic.xAxis) ^ 2 + (alphaMoving.yAxis - alphaStatic.yAxis) ^ 2

        collisionResult =
            distanceSq <= (alphaMoving.radius + alphaStatic.radius) ^ 2

        touchGround =
            alphaStatic.yAxis == 600 && distanceSq <= 4 * (alphaMoving.radius + alphaStatic.radius) ^ 2
    in
        ( ( collisionResult, touchGround ), ( alphaMoving, alphaStatic ) )


response : Float -> Float -> ( Float, Float ) -> TwoBalls -> TwoBalls
response gravity timeUsed ( leftBound, rightBound ) ( moving, static ) =
    if timeUsed >= 1 then
        ( moving, static )
    else
        let
            ( ( collisionResult, touchGround ), ( pseudoMoving, pseudoStatic ) ) =
                (possibleCollision gravity (1 - timeUsed) ( leftBound, rightBound ) ( moving, static ))

            rectangle =
                { x = 800, y = 400, width = 20, height = 200 }
        in
            if touchGround then
                let
                    timeInterval =
                        ( 0, 1 - timeUsed )

                    hitTime =
                        (approxSolution gravity ( timeInterval, ( moving, static ) ))
                in
                    if hitTime == 42 then
                        let
                            netCollision =
                                (wallCollision gravity (1 - timeUsed) ( rectangle, moving ))
                        in
                            case netCollision of
                                NoTouch ->
                                    ( pseudoMoving, pseudoStatic )

                                Corner ->
                                    let
                                        ( ( newRectangle, newBall ), finalTimeUsed ) =
                                            responsePlus gravity timeUsed netCollision ( rectangle, moving )

                                        timeLeft =
                                            1 - timeUsed

                                        static_freeFall =
                                            static.yVelocity * timeLeft + 0.5 * gravity * timeLeft ^ 2

                                        static_vertDistance =
                                            600 - static.yAxis

                                        static_vertCollision =
                                            static_vertDistance - static_freeFall <= 0

                                        static_yVelocity =
                                            if static_vertCollision then
                                                0
                                            else
                                                static.yVelocity + gravity * timeLeft

                                        static_yAxis =
                                            if static_vertCollision then
                                                600
                                            else
                                                static.yAxis + static_freeFall

                                        static_xAxis =
                                            if static.xAxis + static.xVelocity * timeLeft <= (leftBound + static.radius) then
                                                leftBound + static.radius
                                            else if static.xAxis + static.xVelocity * timeLeft >= rightBound - static.radius then
                                                rightBound - static.radius
                                            else
                                                static.xAxis + static.xVelocity * timeLeft

                                        newSlime =
                                            { static
                                                | xAxis = static_xAxis
                                                , yAxis = static_yAxis
                                                , yVelocity = static_yVelocity
                                            }
                                    in
                                        ( newBall, newSlime )

                                _ ->
                                    let
                                        ( ( newRectangle, newBall ), finalTimeUsed ) =
                                            responsePlus gravity timeUsed netCollision ( rectangle, moving )

                                        timeLeft =
                                            finalTimeUsed - timeUsed

                                        static_freeFall =
                                            static.yVelocity * timeLeft + 0.5 * gravity * timeLeft ^ 2

                                        static_vertDistance =
                                            600 - static.yAxis

                                        static_vertCollision =
                                            static_vertDistance - static_freeFall <= 0

                                        static_yVelocity =
                                            if static_vertCollision then
                                                0
                                            else
                                                static.yVelocity + gravity * timeLeft

                                        static_yAxis =
                                            if static_vertCollision then
                                                600
                                            else
                                                static.yAxis + static_freeFall

                                        static_xAxis =
                                            if static.xAxis + static.xVelocity * timeLeft <= (leftBound + static.radius) then
                                                leftBound + static.radius
                                            else if static.xAxis + static.xVelocity * timeLeft >= rightBound - static.radius then
                                                rightBound - static.radius
                                            else
                                                static.xAxis + static.xVelocity * timeLeft

                                        newSlime =
                                            { static
                                                | xAxis = static_xAxis
                                                , yAxis = static_yAxis
                                                , yVelocity = static_yVelocity
                                            }
                                    in
                                        response gravity finalTimeUsed ( leftBound, rightBound ) ( newBall, newSlime )
                    else if (timeUsed > 0 && hitTime <= 0.005) || ((moving.xAxis - static.xAxis) ^ 2 + (moving.yAxis - static.yAxis) ^ 2) < (moving.radius + static.radius) ^ 2 then
                        --it would be great if another testing statement is added: distance_between the objects <= the sum of radii of the objects
                        ( pseudoMoving, pseudoStatic )
                    else
                        let
                            newTimeUsed =
                                timeUsed + hitTime

                            moving_newXaxis =
                                moving.xAxis + moving.xVelocity * hitTime

                            moving_newYaxis =
                                moving.yAxis + moving.yVelocity * hitTime + 0.5 * gravity * hitTime ^ 2

                            moving_newYvelocity =
                                moving.yVelocity + gravity * hitTime

                            static_newXaxis =
                                static.xAxis + static.xVelocity * hitTime

                            colliDist =
                                (moving.radius + static.radius)

                            nx =
                                (static_newXaxis - moving_newXaxis) / colliDist

                            ny =
                                (static.yAxis - moving_newYaxis) / colliDist

                            p =
                                moving.xVelocity * nx + moving_newYvelocity * ny

                            newVx =
                                moving.xVelocity - 2 * p * nx

                            newVy =
                                moving_newYvelocity - 2 * p * ny

                            newMoving =
                                { moving
                                    | xAxis = moving_newXaxis
                                    , yAxis = moving_newYaxis
                                    , xVelocity = newVx
                                    , yVelocity = newVy
                                }

                            newStatic =
                                { static | xAxis = static_newXaxis }

                            netCollision =
                                wallCollision gravity (1 - newTimeUsed) ( rectangle, newMoving )
                        in
                            case netCollision of
                                NoTouch ->
                                    response gravity newTimeUsed ( leftBound, rightBound ) ( newMoving, newStatic )

                                _ ->
                                    let
                                        ( ( newRectangle, newBall ), finalTimeUsed ) =
                                            responsePlus gravity newTimeUsed netCollision ( rectangle, newMoving )

                                        newSlime =
                                            { newStatic
                                                | xAxis = newStatic.xAxis + newStatic.xVelocity * (finalTimeUsed - newTimeUsed)
                                                , yAxis = newStatic.yAxis + newStatic.yVelocity * (finalTimeUsed - newTimeUsed) + 0.5 * gravity * (finalTimeUsed - newTimeUsed) ^ 2
                                                , yVelocity = newStatic.yVelocity + gravity * (finalTimeUsed - newTimeUsed)
                                            }
                                    in
                                        response gravity finalTimeUsed ( leftBound, rightBound ) ( newBall, newSlime )
            else
                (if collisionResult then
                    let
                        hitTime =
                            (actualSolution gravity ( 0, 1 - timeUsed ) ( moving, static ))
                    in
                        if hitTime == 42 then
                            ( pseudoMoving, pseudoStatic )
                        else if hitTime == 0 then
                            ( pseudoMoving, pseudoStatic )
                        else
                            let
                                newTimeUsed =
                                    timeUsed + hitTime

                                newX =
                                    moving.xAxis + moving.xVelocity * hitTime

                                newY =
                                    moving.yAxis + moving.yVelocity * hitTime + 0.5 * gravity * hitTime ^ 2

                                realStatic =
                                    { static
                                        | xAxis = static.xAxis + static.xVelocity * hitTime
                                        , yAxis = static.yAxis + static.yVelocity * hitTime + 0.5 * gravity * hitTime ^ 2
                                        , yVelocity = static.yVelocity + gravity * hitTime
                                    }

                                colliDist =
                                    (moving.radius + realStatic.radius)

                                nx =
                                    (realStatic.xAxis - newX) / colliDist

                                ny =
                                    (realStatic.yAxis - newY) / colliDist

                                p =
                                    moving.xVelocity * nx + (moving.yVelocity + gravity * hitTime) * ny

                                newVx =
                                    moving.xVelocity - 2 * p * nx

                                newVy =
                                    moving.yVelocity + gravity * hitTime - 2 * p * ny

                                realMoving =
                                    { moving
                                        | xAxis = newX
                                        , yAxis = newY
                                        , xVelocity = newVx
                                        , yVelocity = newVy
                                    }

                                netCollision =
                                    wallCollision gravity (1 - newTimeUsed) ( rectangle, realMoving )
                            in
                                case netCollision of
                                    NoTouch ->
                                        response gravity newTimeUsed ( leftBound, rightBound ) ( realMoving, realStatic )

                                    _ ->
                                        let
                                            ( ( newRectangle, newBall ), finalTimeUsed ) =
                                                responsePlus gravity newTimeUsed netCollision ( rectangle, realMoving )

                                            newSlime =
                                                { realStatic
                                                    | xAxis = realStatic.xAxis + realStatic.xVelocity * (finalTimeUsed - newTimeUsed)
                                                    , yAxis = realStatic.yAxis + realStatic.yVelocity * (finalTimeUsed - newTimeUsed) + 0.5 * gravity * (finalTimeUsed - newTimeUsed) ^ 2
                                                    , yVelocity = realStatic.yVelocity + gravity * (finalTimeUsed - newTimeUsed)
                                                }
                                        in
                                            response gravity finalTimeUsed ( leftBound, rightBound ) ( newBall, newSlime )
                 else
                    let
                        netCollision =
                            (wallCollision gravity (1 - timeUsed) ( rectangle, moving ))
                    in
                        case netCollision of
                            NoTouch ->
                                ( pseudoMoving, pseudoStatic )

                            Corner ->
                                let
                                    ( ( newRectangle, newBall ), finalTimeUsed ) =
                                        responsePlus gravity timeUsed netCollision ( rectangle, moving )

                                    timeLeft =
                                        1 - timeUsed

                                    static_freeFall =
                                        static.yVelocity * timeLeft + 0.5 * gravity * timeLeft ^ 2

                                    static_vertDistance =
                                        600 - static.yAxis

                                    static_vertCollision =
                                        static_vertDistance - static_freeFall <= 0

                                    static_yVelocity =
                                        if static_vertCollision then
                                            0
                                        else
                                            static.yVelocity + gravity * timeLeft

                                    static_yAxis =
                                        if static_vertCollision then
                                            600
                                        else
                                            static.yAxis + static_freeFall

                                    static_xAxis =
                                        if static.xAxis + static.xVelocity * timeLeft <= (leftBound + static.radius) then
                                            leftBound + static.radius
                                        else if static.xAxis + static.xVelocity * timeLeft >= rightBound - static.radius then
                                            rightBound - static.radius
                                        else
                                            static.xAxis + static.xVelocity * timeLeft

                                    newSlime =
                                        { static
                                            | xAxis = static_xAxis
                                            , yAxis = static_yAxis
                                            , yVelocity = static_yVelocity
                                        }
                                in
                                    ( newBall, newSlime )

                            _ ->
                                let
                                    ( ( newRectangle, newBall ), finalTimeUsed ) =
                                        responsePlus gravity timeUsed netCollision ( rectangle, moving )

                                    timeLeft =
                                        finalTimeUsed - timeUsed

                                    static_freeFall =
                                        static.yVelocity * timeLeft + 0.5 * gravity * timeLeft ^ 2

                                    static_vertDistance =
                                        600 - static.yAxis

                                    static_vertCollision =
                                        static_vertDistance - static_freeFall <= 0

                                    static_yVelocity =
                                        if static_vertCollision then
                                            0
                                        else
                                            static.yVelocity + gravity * timeLeft

                                    static_yAxis =
                                        if static_vertCollision then
                                            600
                                        else
                                            static.yAxis + static_freeFall

                                    static_xAxis =
                                        if static.xAxis + static.xVelocity * timeLeft <= (leftBound + static.radius) then
                                            leftBound + static.radius
                                        else if static.xAxis + static.xVelocity * timeLeft >= rightBound - static.radius then
                                            rightBound - static.radius
                                        else
                                            static.xAxis + static.xVelocity * timeLeft

                                    newSlime =
                                        { static
                                            | xAxis = static_xAxis
                                            , yAxis = static_yAxis
                                            , yVelocity = static_yVelocity
                                        }
                                in
                                    response gravity finalTimeUsed ( leftBound, rightBound ) ( newBall, newSlime )
                )



{-
   responseA : Float -> TwoBalls -> TwoBalls
   responseA gravity ( moving, static ) =
       let
           moving_freeFall =
               moving.yVelocity + 0.5 * gravity

           moving_yVelocity =
               moving.yVelocity + gravity

           moving_yAxis =
               moving.yAxis + moving_freeFall

           moving_xVelocity =
               if moving.xAxis + moving.xVelocity < moving.radius || moving.xAxis + moving.xVelocity > 1620 - moving.radius then
                   negate moving.xVelocity
               else
                   moving.xVelocity

           moving_xAxis =
               if moving.xAxis + moving.xVelocity < moving.radius then
                   (abs ((moving.xAxis - moving.radius) + moving.xVelocity)) + moving.radius
               else if moving.xAxis + moving.xVelocity > 1620 - moving.radius then
                   1620 - (moving.xVelocity - (1620 - moving.radius - moving.xAxis)) - moving.radius
               else
                   moving.xAxis + moving.xVelocity

           alphaMoving =
               { moving
                   | xAxis = moving_xAxis
                   , yAxis = moving_yAxis
                   , xVelocity = moving_xVelocity
                   , yVelocity = moving_yVelocity
               }

           static_freeFall =
               static.yVelocity + 0.5 * gravity

           static_vertDistance =
               800 - static.yAxis

           static_vertCollision =
               static_vertDistance - static_freeFall <= 0

           static_yVelocity =
               if static_vertCollision then
                   0
               else
                   static.yVelocity + gravity

           static_yAxis =
               if static_vertCollision then
                   800
               else
                   static.yAxis + static_freeFall

           static_xAxis =
               if static.xAxis + static.xVelocity <= static.radius then
                   static.radius
               else if static.xAxis + static.xVelocity >= 800 - static.radius then
                   800 - static.radius
               else
                   static.xAxis + static.xVelocity

           alphaStatic =
               { static | xAxis = static_xAxis, yAxis = static_yAxis, yVelocity = static_yVelocity }

           distanceSq =
               (alphaMoving.xAxis - alphaStatic.xAxis) ^ 2 + (alphaMoving.yAxis - alphaStatic.yAxis) ^ 2

           alphaCollision =
               distanceSq <= (alphaMoving.radius + alphaStatic.radius) ^ 2
       in
           if alphaCollision then
               let
                   ( cx, cy ) =
                       Geometrics.closestP moving static

                   moveDist =
                       sqrt ((moving.radius + static.radius) ^ 2 - ((static.xAxis - cx) ^ 2 + (static.yAxis - cy) ^ 2))

                   normVectx =
                       moving.xVelocity / (sqrt (moving.xVelocity ^ 2 + moving.yVelocity ^ 2))

                   normVecty =
                       moving.yVelocity / (sqrt (moving.xVelocity ^ 2 + moving.yVelocity ^ 2))

                   newX =
                       cx - moveDist * normVectx

                   newY =
                       cy - moveDist * normVecty

                   colliDist =
                       (moving.radius + static.radius)

                   nx =
                       (static.xAxis - newX) / colliDist

                   ny =
                       (static.yAxis - newY) / colliDist

                   p =
                       moving.xVelocity * nx + moving.yVelocity * ny

                   newVx =
                       moving.xVelocity - 2 * p * nx

                   newVy =
                       moving.yVelocity - 2 * p * ny

                   newMoving =
                       { moving
                           | xAxis = newX
                           , yAxis = newY
                           , xVelocity = newVx
                           , yVelocity = newVy
                       }
               in
                   ( newMoving, static )
           else
               ( alphaMoving, alphaStatic )


   responseB : Bool -> TwoBalls -> TwoBalls
   responseB bool ( moving, static ) =
       case bool of
           False ->
               let
                   newMoving =
                       { moving
                           | xAxis = moving.xAxis + moving.xVelocity
                           , yAxis = moving.yAxis + moving.yVelocity
                       }
               in
                   ( newMoving, static )

           True ->
               let
                   ( cx, cy ) =
                       Geometrics.closestP moving static

                   moveDist =
                       sqrt ((moving.radius + static.radius) ^ 2 - ((static.xAxis - cx) ^ 2 + (static.yAxis - cy) ^ 2))

                   normVectx =
                       moving.xVelocity / (sqrt (moving.xVelocity ^ 2 + moving.yVelocity ^ 2))

                   normVecty =
                       moving.yVelocity / (sqrt (moving.xVelocity ^ 2 + moving.yVelocity ^ 2))

                   newX =
                       cx - moveDist * normVectx

                   newY =
                       cy - moveDist * normVecty

                   colliDist =
                       (moving.radius + static.radius)

                   nx =
                       (static.xAxis - newX) / colliDist

                   ny =
                       (static.yAxis - newY) / colliDist

                   p =
                       moving.xVelocity * nx + moving.yVelocity * ny

                   newVx =
                       moving.xVelocity - 2 * p * nx

                   newVy =
                       moving.yVelocity - 2 * p * ny

                   newMoving =
                       { moving
                           | xAxis = newX
                           , yAxis = newY
                           , xVelocity = newVx
                           , yVelocity = newVy
                       }
               in
                   ( newMoving, static )
-}
