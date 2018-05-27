module Scene exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Time exposing (Time)
import Dict exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import Mouse
import Keyboard exposing (..)
import Task
import WebGL.Texture as Texture exposing (..)
import Color exposing (..)
import Window


main : Program Never Model Msg
main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }


type alias Model =
    { t1 : Maybe Texture
    , t2 : Maybe Texture
    , winSize : Window.Size
    , keys : Keys
    , pos : Vec3
    , pitchAndYaw : ( Float , Float )
    , lookDir : Vec3
    , lastMousePos : Mouse.Position }

type alias Vertex = { position : Vec3 , coord : Vec2 }
type alias Keys = { left : Bool ,  down : Bool , up : Bool , right : Bool , space : Bool }
type alias Uniforms = { projection : Mat4 , view : Mat4 , model : Mat4 , texture : Texture }
type TextureId = One | Two

type Msg
    = TextureLoaded1 (Result Texture.Error Texture)
    | TextureLoaded2 (Result Texture.Error Texture)
    | Animate Time
    | WindowResized Window.Size
    | MouseMove Mouse.Position
    | KeyChange Bool Keyboard.KeyCode

init: ( Model , Cmd Msg )
init =
    ( Model
        Nothing
        Nothing
        (Window.Size 1 1)
        (Keys False False False False False)
        (vec3 0 0 -10)
        (0 , -90)
        (vec3 0 0 1)
        { x = 0 , y = 0 }
    , Cmd.batch
        [ loadTexture "textures/thwomp-face.jpg" TextureLoaded1
        , loadTexture "textures/uv_big.png" TextureLoaded2
        , Task.perform WindowResized Window.size] )

subscriptions: Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Window.resizes WindowResized
        , Mouse.moves MouseMove
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False) ]


update: Msg -> Model -> (Model , Cmd Msg)
update msg model =
    let m = case msg of
            TextureLoaded1 result -> { model | t1 = Result.toMaybe result }
            TextureLoaded2 result -> { model | t2 = Result.toMaybe result }
            KeyChange b k -> { model | keys = getKeys b k model.keys }
            Animate dt -> { model | pos = movePos model.keys model.lookDir model.pos }
            WindowResized size -> { model | winSize = size }
            MouseMove mp ->
                let (ld , py) = getLookPos model.lastMousePos mp model.pitchAndYaw
                in { model | lookDir = ld , lastMousePos = mp , pitchAndYaw = py }
    in ( m , Cmd.none )

view: Model -> Html Msg
view model =
    WebGL.toHtml
        [ width model.winSize.width , height model.winSize.height , style [ ( "display" , "block") ] ]
        ([ getEntity model wall (texturedPlane |> WebGL.triangles) model.t1
        , getEntity model tetra (tetrahedron |> WebGL.triangles) model.t2
        , getEntity model floor (texturedPlane |> WebGL.triangles) model.t2 ] |> List.concat)

wall = Mat4.identity
floor = Mat4.makeTranslate3 0 -1 0
        |> Mat4.rotate (pi / 2) ( vec3 1 0 0)
        |> Mat4.scale3 15 15 0
tetra = Mat4.makeTranslate3 5 4 5
        |> Mat4.scale3 2 2 2

getEntity: Model -> Mat4 -> Mesh Vertex -> Maybe Texture -> List WebGL.Entity
getEntity model local mesh tex =
    case tex of
        Just t ->
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                (Uniforms (projectionMatrix model) (viewMatrix model ) local t) ]
        Nothing -> []

projectionMatrix: Model -> Mat4
projectionMatrix model =
    Mat4.makePerspective 50 (toFloat model.winSize.width / toFloat model.winSize.height) 0.01 1000

viewMatrix: Model -> Mat4
viewMatrix model =
    Mat4.makeLookAt model.pos (Vec3.add model.pos model.lookDir) Vec3.j

getLookPos: Mouse.Position -> Mouse.Position -> ( Float , Float ) -> ( Vec3 , (Float , Float) )
getLookPos lmp mp ( lastPitch , lastYaw ) =
    let sensitivity = 0.0039
        rangeY = 89
        ox = mp.x - lmp.x |> toFloat
        oy = lmp.y - mp.y |> toFloat
        yaw = ox * sensitivity + lastYaw |> radians
        pitch = -oy * sensitivity + lastPitch |> radians
        pitch_ = if pitch > rangeY then rangeY else if pitch < -rangeY then -rangeY else pitch
        lookDir = vec3 (cos yaw * cos pitch_) (sin pitch_) (sin yaw * cos pitch_)
    in (Vec3.normalize lookDir , ( pitch_ , yaw ) )

colorToRGB: Color -> Vec3
colorToRGB c =
    let cc = Color.toRgb c
        convert x = toFloat x / 255
    in vec3 (convert cc.red) (convert cc.green) (convert cc.blue)


movePos: Keys -> Vec3 -> Vec3 -> Vec3
movePos { left , down , up , right , space } lookDir pos =
    let speed = 0.2
        lookDir_ = Vec3.setY 0 lookDir
        forward = if up then 1 else if down then -1 else 0
        strafe = if right then 1 else if left then -1 else 0
        cross = Vec3.cross lookDir_ Vec3.j
        dir = Vec3.add (Vec3.scale strafe cross) (Vec3.scale forward lookDir_)
        dir_ = if Vec3.length dir <= 0 then dir else Vec3.normalize dir
        asdf = Task.map
    in Vec3.add pos <| Vec3.scale speed dir_

getKeys: Bool -> KeyCode -> Keys -> Keys
getKeys isOn code keys =
    case code of
        32 -> { keys | space = isOn }
        -- ◀ ▼ ▲ ▶
        37 -> { keys | left = isOn }
        40 -> { keys | down = isOn }
        38 -> { keys | up = isOn }
        39 -> { keys | right = isOn }
        -- WASD
        87 -> { keys | up = isOn }
        65 -> { keys | left = isOn }
        83 -> { keys | down = isOn }
        68 -> { keys | right = isOn }
        _ -> keys


loadTexture: String -> (Result Error Texture -> Msg) -> Cmd Msg
loadTexture path msg =
    Task.attempt msg (Texture.load path)

-------------
-- Geometry
-------------

tetrahedron: List ( Vertex, Vertex, Vertex )
tetrahedron =
    let peak = Vertex (vec3 0 1 0) (vec2 1 1)
        bottomLeft = Vertex (vec3 -1 -1 -1) (vec2 0 0)
        bottomRight = Vertex (vec3 -1 -1 1) (vec2 1 0)
        topLeft = Vertex (vec3 1 -1 1) (vec2 0 0)
        topRight = Vertex (vec3 1 -1 -1) (vec2 0 1)
    in [ ( peak , bottomLeft , bottomRight )
       , ( peak , bottomLeft , topRight )
       , ( peak , bottomRight , topLeft )
       , ( peak , topRight , topLeft )
       , ( bottomLeft , bottomRight , topRight)
       , ( bottomRight, topLeft , topRight ) ]

texturedPlane: List ( Vertex, Vertex, Vertex )
texturedPlane =
    let topLeft = Vertex (vec3 -1 1 1) (vec2 0 1)
        topRight = Vertex (vec3 1 1 1) (vec2 1 1)
        bottomLeft = Vertex (vec3 -1 -1 1) (vec2 0 0)
        bottomRight = Vertex (vec3 1 -1 1) (vec2 1 0)
    in [ ( topLeft, topRight, bottomLeft ) , ( bottomLeft, topRight, bottomRight ) ]

-------------
-- Shaders
-------------

vertexShader: Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 projection;
        uniform mat4 view;
        uniform mat4 model;
        varying vec2 vcoord;

        void main()
        {
            gl_Position = projection * view * model * vec4(position, 1.0);
            vcoord = coord.xy;
        }
    |]

fragmentShader: Shader {} Uniforms { vcoord : Vec2 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;

        void main()
        {
            gl_FragColor = texture2D(texture, vcoord);
        }
    |]
