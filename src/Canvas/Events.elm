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


touchOptions : Options
touchOptions =
    { stopPropagation = True
    , preventDefault = True
    }


{-| -}
onTouchStart : (Point -> msg) -> Attribute msg
onTouchStart message =
    onWithOptions "touchstart" touchOptions <|
        Json.map
            (positionInCanvas >> message)
            touchEventPositionDecoder


{-| -}
onTouchEnd : (Point -> msg) -> Attribute msg
onTouchEnd message =
    onWithOptions "touchend" touchOptions <|
        Json.map
            (positionInCanvas >> message)
            touchEventPositionDecoder


{-| -}
onTouchCancel : (Point -> msg) -> Attribute msg
onTouchCancel message =
    onWithOptions "touchcancel" touchOptions <|
        Json.map
            (positionInCanvas >> message)
            touchEventPositionDecoder


{-| -}
onTouchMove : (Point -> msg) -> Attribute msg
onTouchMove message =
    onWithOptions "touchmove" touchOptions <|
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
        (Json.map2 (,)
            (Json.field "touches" <| Json.index 0 <| Json.field "clientX" <| Json.float)
            (Json.field "touches" <| Json.index 0 <| Json.field "clientY" <| Json.float)
        )
        (toTuple [ "target", "offsetLeft" ] [ "target", "offsetTop" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


toTuple : List String -> List String -> Json.Decoder ( Float, Float )
toTuple x y =
    Json.map2 (,) (Json.at x Json.float) (Json.at y Json.float)
