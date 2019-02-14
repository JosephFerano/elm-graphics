module Torus exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Time exposing (Time)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
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
--    , mesh : Mesh Attributes
    , time : Float
    , winSize : Window.Size }

init: ( Model , Cmd Msg )
init =
    ( { p = 1 , q = 0 , time = 0.0 , winSize = (Window.Size 1 1)
--    ( { p = 949 , q = 956 , time = 0.0 , winSize = (Window.Size 1 1)
--    , mesh = (torusPoints 2 15 |> torusShell)
    }
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
--                { model | time = model.time + dt * 0.001 }
                { model | time = model.time + dt * 0.001 , p = model.p + 0.08 , q = model.q + 0.04}
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
--            model.mesh
--            (constructTorus model)
            (constructTorus2 model)
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
                (colorToVec3 Color.red)
--                (colorToVec3 Color.darkGrey)
--                (colorToVec3 Color.white)
                (vec3 1 1 1)
                (vec3 1 1 1)
                (vec3 1 1 1)
                1.0) ] )

totalLinePoints = 100
ringRadius = 0.15
ringVerts = 18

constructTorus: Model -> Mesh Attributes
constructTorus model =
    let points = torusPoints model.p model.q |> makePairs
        rings = torusRings points |> List.concatMap makePairs
    in points ++ rings |> toLines
--    in points |> toLines


constructTorus2: Model -> Mesh Attributes
constructTorus2 model =
    torusPoints model.p model.q
    |> makePairs
    |> torusRings
    |> torusTris
    |> withTris
--    |> wireframe |> List.map (\ x -> toAttributes x Vec3.i) |> WebGL.lineStrip

withTris: List (Vec3 , Vec3 , Vec3) -> Mesh Attributes
withTris tris =
    tris
    |> List.map (\ (v1, v2, v3) ->
        let n = Vec3.cross (Vec3.sub v1 v2) (Vec3.sub v3 v1) |> Vec3.normalize
        in (toAttributes v1 n, toAttributes v2 n, toAttributes v3 n))
    |> WebGL.triangles

torusPoints: Float -> Float -> List Vec3
torusPoints p q =
    interpolatedCircle totalLinePoints
    |> List.map
        (\ t ->
            let r = 0.5 * (2 + sin (q * t))
            in vec3
                 (cos (t * p) * r)
                 (cos (t * q) * r * 0.5)
                 (sin (t * p) * r) )


torusRings: List (Vec3, Vec3) -> List (List Vec3)
torusRings verts =
    verts
    |> List.map
        (\ (p1, p2)->
            (List.map circlePoint <| interpolatedCircle ringVerts)
            |> List.map (\ p ->
                let (mid , dir) = (Vec3.add p1 p2 |> Vec3.scale 0.5 , Vec3.sub p2 p1 |> Vec3.normalize)
                    p_ = Vec3.toRecord p
                    dir_ = Vec3.toRecord dir
                    u = Vec3.cross dir Vec3.j |> Vec3.normalize
                    v = Vec3.cross dir u |> Vec3.normalize
                    point = Vec3.add (Vec3.scale p_.x u) (Vec3.scale p_.y v) |> Vec3.scale ringRadius
                in Vec3.add point mid))

torusTris: List (List Vec3) -> List (Vec3 , Vec3 , Vec3)
torusTris rings =
    makePairs rings
    |> List.concatMap
        (\ (rs1 , rs2) ->
            List.map2 (,) (makePairs rs1) (makePairs rs2)
            |> List.concatMap
                (\ (pair1 , pair2) ->
                    let a = Tuple.first pair1
                        b = Tuple.second pair1
                        c = Tuple.first pair2
                        d = Tuple.second pair2
                    in [ (a , b , c) , (d , c , b) ] ) )

wireframe: List (Vec3 , Vec3 , Vec3) -> List Vec3
wireframe tris =
    List.foldl (\ (v1, v2, v3) acc -> acc ++ [ v1 , v2 , v3 ] ) [] tris

circlePoint: Float -> Vec3
circlePoint x = vec3 (cos x) (sin x) 0

makePairs: List a -> List (a ,a)
makePairs ps = List.map2 (,) ps (List.drop 1 ps)

closedPairs: List a -> List (a ,a)
closedPairs xs =
    List.map2 (,)
        xs
        (case List.head xs of
            Just x -> List.append (List.drop 1 xs) [ x ]
            Nothing -> []
        )

toLines: List (Vec3 , Vec3) -> Mesh Attributes
toLines vs = List.map (\ (v1 , v2) -> (toAttributes v1 Vec3.i , toAttributes v2 Vec3.i) ) vs |> WebGL.lines

interpolatedCircle: Int -> List Float
interpolatedCircle steps =
    List.range 0 steps
    |> List.map (\ step -> (pi * 2 / (toFloat steps) * (toFloat step) ) )

toAttributes: Vec3 -> Vec3 -> Attributes
toAttributes v n = Attributes v n


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

            vec3 lightDir = normalize(vec3(0.0, -0.5, -0.5));
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



