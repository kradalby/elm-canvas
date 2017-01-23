import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick)
import Canvas exposing (Canvas, Position, Image, Error, Size)
import Color
import Array exposing (Array)
import Task


main = 
  Html.program
  { init  = (initModel, initCmd) 
  , view   = view 
  , update = update
  , subscriptions = always Sub.none
  }



-- TYPES



type Msg
  = ImageLoaded (Result Error Image)
  | Invert


initModel : Canvas
initModel =
  Size 770 770
  |>Canvas.initialize
  |>Canvas.fill Color.black


initCmd : Cmd Msg
initCmd =
  Task.attempt ImageLoaded (Canvas.loadImage "./agnes-martin-piece.png")




-- UPDATE



update : Msg -> Canvas -> (Canvas, Cmd Msg)
update message canvas =
  case message of 

    Invert ->
      (invertCanvas canvas, Cmd.none)

    ImageLoaded imageResult ->
      case Result.toMaybe imageResult of
        Just image ->
          let 

            newCanvas =
              Canvas.getImageSize image
              |>Canvas.initialize
              |>Canvas.drawImage image (Position 0 0)
          
          in
            (newCanvas, Cmd.none)

        Nothing ->
          (canvas, Cmd.none)


invertCanvas : Canvas -> Canvas
invertCanvas canvas = 
  let

    imageData =
      Canvas.getImageData canvas

    size = 
      Canvas.getCanvasSize canvas
  
  in
    Canvas.fromImageData size (invertColors imageData)


invertColors : Array Int -> Array Int
invertColors = 
  Array.indexedMap invertColor 


invertColor : Int -> Int -> Int
invertColor index colorValue =
  if index % 4 == 3 then
    colorValue
  else
    255 - colorValue



-- VIEW



view : Canvas -> Html Msg
view canvas =
  div []
  [ h1 [] [ text "Click on the canvas to invert its colors" ]
  , Canvas.toHtml [ onClick Invert ] canvas
  ]




