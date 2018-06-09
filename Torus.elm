module Torus exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Time exposing (Time)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import Color exposing (..)
import Window
import Shaders exposing (..)
import Task exposing (..)

main : Program Never Model Msg
main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

type alias Attributes = { position : Vec3 , normal : Vec3 }

type Msg
    = Animate Time
    | WindowResized Window.Size

type alias Model = { p : Float , q : Float , winSize : Window.Size }

init: ( Model , Cmd Msg )
init =
    ( Model 1.0 1.0 (Window.Size 1 1)
    , Cmd.batch [ Task.perform WindowResized Window.size ] )


subscriptions: Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes WindowResized ]

update: Msg -> Model -> (Model , Cmd Msg)
update msg model =
    let m = case msg of
            Animate dt -> model
            WindowResized size -> { model | winSize = size }
    in ( m , Cmd.none )

view: Model -> Html Msg
view model =
    WebGL.toHtml
        [ width model.winSize.width
        , height model.winSize.height
        , style [ ( "display" , "block") , ( "background" , "black" ) ] ]
        ([ WebGL.entity
            diffuseVS
            diffuseFS
            (torus model)
            (DiffuseColor
                (Mat4.makePerspective
                    50
                    (toFloat model.winSize.width / toFloat model.winSize.height)
                    0.01
                    1000)
                (Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0))
                Mat4.identity
                (colorToVec3 Color.white)
                (vec3 1 1 0)
                (vec3 1 0 1)
                (vec3 0 0 1)
                1.0) ] )

torus: Model -> Mesh Attributes
torus model =
    WebGL.lines
        [ (Attributes (vec3 1 0 -1) Vec3.i , Attributes (vec3 -1 0 1) Vec3.i)
        , (Attributes (vec3 -1 1 1) Vec3.i , Attributes (vec3 1 -1 1) Vec3.i)
        ]

colorToVec3: Color -> Vec3
colorToVec3 color =
    let to01 x = toFloat x / 255
        c = Color.toRgb color
    in vec3 (to01 c.red) (to01 c.green) (to01 c.blue)

