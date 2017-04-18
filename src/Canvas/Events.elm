module Canvas.Events
    exposing
        ( onMouseDown
        , onMouseUp
        , onMouseMove
        , onClick
        , onDoubleClick
        , onTouchStart
        , onTouchEnd
        , onTouchCancel
        , onTouchMove
        )

{-| These functions are just like the `Html.Events` functions `onMouseDown`, `onMouseUp`, etc, except that they pass along a `Point`, representing exactly where on the canvas the mouse activity occured. They can be used on other elements too, like divs.

@docs onMouseDown, onMouseUp, onMouseMove, onClick, onDoubleClick
-}

import Html exposing (Attribute)
import Html.Events exposing (on, onWithOptions, Options)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import Json.Decode as Json


{-| Just like the `onMouseDown` in `Html.Events`, but this one passes along a `Point` that is relative to the `Canvas`. So clicking right in the middle of a 200x200 `Canvas` will return a `Point.toInts point == ( 100, 100 )`.

    Canvas.toHtml
        [ Canvas.Events.onClick CanvasClick ]
        canvas

    -- ..

    case message of
        CanvasClick point ->
            -- ..
-}
onMouseDown : (Point -> msg) -> Attribute msg
onMouseDown message =
    on "mousedown" <|
        Json.map
            (positionInCanvas >> message)
            mouseEventPositionDecoder


{-| -}
onMouseUp : (Point -> msg) -> Attribute msg
onMouseUp message =
    on "mouseup" <|
        Json.map
            (positionInCanvas >> message)
            mouseEventPositionDecoder


{-| -}
onMouseMove : (Point -> msg) -> Attribute msg
onMouseMove message =
    on "mousemove" <|
        Json.map
            (positionInCanvas >> message)
            mouseEventPositionDecoder


{-| -}
onClick : (Point -> msg) -> Attribute msg
onClick message =
    on "click" <|
        Json.map
            (positionInCanvas >> message)
            mouseEventPositionDecoder


{-| -}
onDoubleClick : (Point -> msg) -> Attribute msg
onDoubleClick message =
    on "dblclick" <|
        Json.map
            (positionInCanvas >> message)
            mouseEventPositionDecoder


{-| -}
onTouchStart : Options -> (Point -> msg) -> Attribute msg
onTouchStart options message =
    onWithOptions "touchstart" options <|
        Json.map
            (positionInCanvas >> message)
            touchEventPositionDecoder


{-| -}
onTouchEnd : Options -> (Point -> msg) -> Attribute msg
onTouchEnd options message =
    onWithOptions "touchend" options <|
        Json.map
            (positionInCanvas >> message)
            touchEventPositionDecoder


{-| -}
onTouchCancel : Options -> (Point -> msg) -> Attribute msg
onTouchCancel options message =
    onWithOptions "touchcancel" options <|
        Json.map
            (positionInCanvas >> message)
            touchEventPositionDecoder


{-| -}
onTouchMove : Options -> (Point -> msg) -> Attribute msg
onTouchMove options message =
    onWithOptions "touchmove" options <|
        Json.map
            (positionInCanvas >> message)
            touchEventPositionDecoder


positionInCanvas : ( ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) ) -> Point
positionInCanvas ( client, offset, body, documentElement ) =
    let
        ( cx, cy ) =
            client

        ( ox, oy ) =
            offset

        ( bx, by ) =
            body

        ( dx, dy ) =
            documentElement
    in
        Point.fromFloats ( (cx + bx + dx) - ox, (cy + by + dy) - oy )


mouseEventPositionDecoder : Json.Decoder ( ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) )
mouseEventPositionDecoder =
    Json.map4 (,,,)
        (toTuple [ "clientX" ] [ "clientY" ])
        (toTuple [ "target", "offsetLeft" ] [ "target", "offsetTop" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


touchEventPositionDecoder : Json.Decoder ( ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) )
touchEventPositionDecoder =
    Json.map4 (,,,)
        -- Select the first Touch and get clientX and clientY
        (toTuple [ "changedTouches", "0", "clientX" ] [ "changedTouches", "0", "clientY" ])
        (toTuple [ "target", "offsetLeft" ] [ "target", "offsetTop" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


toTuple : List String -> List String -> Json.Decoder ( Float, Float )
toTuple x y =
    Json.map2 (,) (Json.at x Json.float) (Json.at y Json.float)
