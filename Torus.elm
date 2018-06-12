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
import WebGL.Texture as Texture exposing (..)
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

type alias Model =
    { p : Float
    , q : Float
    , time : Float
    , winSize : Window.Size }

init: ( Model , Cmd Msg )
init =
    ( { p = 1 , q = 1 , time = 0.0 , winSize = (Window.Size 1 1) }
--    ( { p = 9 , q = 7 , time = 0.0 , winSize = (Window.Size 1 1) }
    , Cmd.batch [ Task.perform WindowResized Window.size ] )


subscriptions: Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes WindowResized ]

update: Msg -> Model -> (Model , Cmd Msg)
update msg model =
    let m = case msg of
            Animate dt ->
                { model | time = model.time + dt * 0.001 }
--                { model | time = model.time + dt * 0.001 , p = model.p + 0.002 , q = model.q + 0.015}
--            Animate dt -> model
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
            (torusPoints model |> torusShell)
            (DiffuseColor
                (Mat4.makePerspective
                    50
                    (toFloat model.winSize.width / toFloat model.winSize.height)
                    0.01
                    1000)
                (Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0))
                (Mat4.makeRotate (model.time * 0.5) (vec3 1 1 1 ) )
--                (Mat4.makeRotate (pi) (vec3 0.3 0.5 1 ) )
--                Mat4.identity
                (colorToVec3 Color.white)
                (vec3 1 1 0)
                (vec3 1 0 1)
                (vec3 0 0 1)
                1.0) ] )

totalLinePoints = 30
ringRadius = 2
ringVerts = 8

torusPoints: Model -> List (Vec3 , Vec3)
torusPoints model =
    List.range 0 totalLinePoints
    |> List.map (\ step -> (pi * 2.0 / totalLinePoints * (toFloat step)) )
    |> List.map
        (\ t ->
            let r = 0.5 * (2 + sin (model.q * t))
            in vec3
                 (cos (t * model.p) * r)
                 (cos (t * model.q) * r * 0.5)
                 (sin (t * model.p) * r) )
    |> 


torusShell: List Vec3 -> Mesh Attributes
torusShell verts =
    verts
    |> List.map (\ v1 -> Attributes v1 Vec3.i )
    |> WebGL.lineStrip



colorToVec3: Color -> Vec3
colorToVec3 color =
    let to01 x = toFloat x / 255
        c = Color.toRgb color
    in vec3 (to01 c.red) (to01 c.green) (to01 c.blue)

type alias DiffuseColor =
    { projection : Mat4
    , view : Mat4
    , model : Mat4
    , color : Vec3
    , ambient : Vec3
    , diffuse : Vec3
    , specular : Vec3
    , shininess : Float }


diffuseVS: Shader { position : Vec3 , normal : Vec3 } DiffuseColor { vlightWeight : Vec3 }
diffuseVS =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 projection;
        uniform mat4 view;
        uniform mat4 model;
        uniform vec3 ambient;
        uniform vec3 diffuse;
        uniform vec3 specular;
        uniform float shininess;
        varying vec3 vlightWeight;

        void main()
        {
            gl_Position = projection * view * model * vec4(position, 1.0);

            vec3 lightDir = normalize(vec3(0.0, -0.0, -1.0));
            vec4 norm =  model * vec4(normal, 0.0);
            vec3 n = norm.xyz;
            float dir = max(dot(n, lightDir), 0.0);
            float v = 0.5;
            vlightWeight = diffuse * dir + vec3(v, v, v);
        }
    |]

diffuseFS: Shader {} DiffuseColor { vlightWeight : Vec3 }
diffuseFS =
    [glsl|

        precision mediump float;
        uniform vec3 color;
        varying vec3 vlightWeight;

        void main()
        {
            gl_FragColor = vec4(color * vlightWeight, 1.0);
        }
    |]



