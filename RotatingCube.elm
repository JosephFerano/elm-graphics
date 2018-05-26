module RotatingCube exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Time exposing (Time)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import Color


main : Program Never Float Time
main = Html.program
    { init = ( 0 , Cmd.none )
    , view = view
    , update = (\ dt t -> ( t + dt / 4000 , Cmd.none ))
    , subscriptions = (\m -> AnimationFrame.diffs Basics.identity) }

type alias Vertex = { position : Vec3 , color : Vec3 }

type alias Uniforms =
    { perspective : Mat4
    , rotation : Mat4
    , camera : Mat4
    , shade : Float }

w = 500
h = 400

view : Float -> Html Time
view t =
    WebGL.toHtml
        [ width w , height h , style [ ( "display" , "block") ] ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (uniforms t) ]

uniforms: Float -> Uniforms
uniforms t =
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (2 * t) (vec3 0 1 0) )
            (Mat4.makeRotate (2 * t) (vec3 1 0 0) )
    , perspective = Mat4.makePerspective 60 (w / h) 0.1 100
    , camera = Mat4.makeLookAt (vec3 0 0 7) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8 }

mesh: Mesh Vertex
mesh =
    let rft = vec3 1 1 1
        lft = vec3 -1 1 1
        lbt = vec3 -1 -1 1
        rbt = vec3 1 -1 1
        rbb = vec3 1 -1 -1
        rfb = vec3 1 1 -1
        lfb = vec3 -1 1 -1
        lbb = vec3 -1 -1 -1
    in
        [ face Color.green rft rfb rbb rbt
        , face Color.blue rft rfb lfb lft
        , face Color.yellow rft lft lbt rbt
        , face Color.red rfb lfb lbb rbb
        , face Color.purple lft lfb lbb lbt
        , face Color.orange rbt rbb lbb lbt
        ] |> List.concat
          |> WebGL.triangles


face: Color.Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Vertex , Vertex , Vertex)
face rawColor a b c d =
    let cc = Color.toRgb rawColor
        f = (\x -> toFloat x / 255)
        color = vec3 (f cc.red) (f cc.green) (f cc.blue)
        vertex position = Vertex position color
    in
        [ ( vertex a , vertex b , vertex c ) , ( vertex c , vertex d , vertex a )]

vertexShader: Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        varying vec3 vcolor;

        void main()
        {
            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            vcolor = color;
        }
    |]

fragmentShader: Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;

        void main()
        {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]
