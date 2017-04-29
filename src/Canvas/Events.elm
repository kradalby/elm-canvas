module Canvas.Events
    exposing
        ( onMouseDown
        , onMouseUp
        , onMouseMove
        , onClick
        , onDoubleClick
        , onSingleTouchStart
        , onSingleTouchEnd
        , onSingleTouchCancel
        , onSingleTouchMove
        , onMultiTouchStart
        , onMultiTouchEnd
        , onMultiTouchCancel
        , onMultiTouchMove
        )

{-| These functions are just like the `Html.Events` functions `onMouseDown`, `onMouseUp`, etc, except that they pass along a `Point`, representing exactly where on the canvas the mouse activity occured. They can be used on other elements too, like divs.

@docs onMouseDown, onMouseUp, onMouseMove, onClick, onDoubleClick

-}

import Html exposing (Attribute)
import Html.Events exposing (on, onWithOptions, Options)
import Canvas.Point exposing (Point)
import Canvas.Point as Point
import Json.Decode as Json
import List
import Dict


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
onSingleTouchStart : Options -> (Point -> msg) -> Attribute msg
onSingleTouchStart options message =
    onWithOptions "touchstart" options <|
        Json.map
            (positionInCanvas >> message)
            (traceDecoder "onSingleTouchStart" singleTouchEventPositionDecoder)


{-| -}
onSingleTouchEnd : Options -> (Point -> msg) -> Attribute msg
onSingleTouchEnd options message =
    onWithOptions "touchend" options <|
        Json.map
            (positionInCanvas >> message)
            singleTouchEventPositionDecoder


{-| -}
onSingleTouchCancel : Options -> (Point -> msg) -> Attribute msg
onSingleTouchCancel options message =
    onWithOptions "touchcancel" options <|
        Json.map
            (positionInCanvas >> message)
            singleTouchEventPositionDecoder


{-| -}
onSingleTouchMove : Options -> (Point -> msg) -> Attribute msg
onSingleTouchMove options message =
    onWithOptions "touchmove" options <|
        Json.map
            (positionInCanvas >> message)
            singleTouchEventPositionDecoder


{-| -}
onMultiTouchStart : Options -> (List Point -> msg) -> Attribute msg
onMultiTouchStart options message =
    onWithOptions "touchstart" options <|
        Json.map
            (positionsInCanvas >> message)
            (traceDecoder "onMultiTouchStart" multiTouchEventPositionDecoder)


{-| -}
onMultiTouchEnd : Options -> (List Point -> msg) -> Attribute msg
onMultiTouchEnd options message =
    onWithOptions "touchend" options <|
        Json.map
            (positionsInCanvas >> message)
            multiTouchEventPositionDecoder


{-| -}
onMultiTouchCancel : Options -> (List Point -> msg) -> Attribute msg
onMultiTouchCancel options message =
    onWithOptions "touchcancel" options <|
        Json.map
            (positionsInCanvas >> message)
            multiTouchEventPositionDecoder


{-| -}
onMultiTouchMove : Options -> (List Point -> msg) -> Attribute msg
onMultiTouchMove options message =
    onWithOptions "touchmove" options <|
        Json.map
            (positionsInCanvas >> message)
            multiTouchEventPositionDecoder


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


positionsInCanvas : ( List ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) ) -> List Point
positionsInCanvas ( clients, offset, body, documentElement ) =
    List.map (\client -> positionInCanvas ( client, offset, body, documentElement )) clients


mouseEventPositionDecoder : Json.Decoder ( ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) )
mouseEventPositionDecoder =
    Json.map4 (,,,)
        (toTuple [ "clientX" ] [ "clientY" ])
        (toTuple [ "target", "offsetLeft" ] [ "target", "offsetTop" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


singleTouchEventPositionDecoder : Json.Decoder ( ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) )
singleTouchEventPositionDecoder =
    Json.map4 (,,,)
        -- Select the first Touch and get clientX and clientY
        (toTuple [ "changedTouches", "0", "clientX" ] [ "changedTouches", "0", "clientY" ])
        (toTuple [ "target", "offsetLeft" ] [ "target", "offsetTop" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


multiTouchEventPositionDecoder : Json.Decoder ( List ( Float, Float ), ( Float, Float ), ( Float, Float ), ( Float, Float ) )
multiTouchEventPositionDecoder =
    Json.map4 (,,,)
        (Json.at [ "changedTouches" ] touchListDecoder)
        (toTuple [ "target", "offsetLeft" ] [ "target", "offsetTop" ])
        (toTuple [ "view", "document", "body", "scrollLeft" ] [ "view", "document", "body", "scrollTop" ])
        (toTuple [ "view", "document", "documentElement", "scrollLeft" ] [ "view", "document", "documentElement", "scrollTop" ])


toTuple : List String -> List String -> Json.Decoder ( Float, Float )
toTuple x y =
    Json.map2 (,) (Json.at x Json.float) (Json.at y Json.float)


touchListDecoder : Json.Decoder (List ( Float, Float ))
touchListDecoder =
    Json.maybe
        (Json.map2
            (,)
            (Json.field "identifier" Json.int)
            (toTuple [ "clientX" ] [ "clientY" ])
        )
        |> Json.dict
        |> Json.map
            (Dict.values
                >> (List.filterMap identity)
                >> List.map (\( i, ( x, y ) ) -> ( x, y ))
            )


traceDecoder : String -> Json.Decoder msg -> Json.Decoder msg
traceDecoder message decoder =
    Json.value
        |> Json.andThen
            (\value ->
                case Json.decodeValue decoder value of
                    Ok decoded ->
                        Json.succeed <| Debug.log ("Success: " ++ message) <| decoded

                    Err err ->
                        Json.fail <| Debug.log ("Fail: " ++ message) <| err
            )
