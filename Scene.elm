module Scene exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import AnimationFrame
import Time exposing (Time)
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
    , winSize : Window.Size
    , keys : Keys
    , pos : Vec3
    , pitchAndYaw : ( Float , Float )
    , lookDir : Vec3
    , lastMousePos : Mouse.Position }

type alias Vertex = { position : Vec3 , coord : Vec2 }
type alias Keys = { left : Bool ,  down : Bool , up : Bool , right : Bool , space : Bool }
type alias Uniforms = { projection : Mat4 , view : Mat4 , model : Mat4 , texture : Texture }

type Msg
    = TextureLoaded (Result Texture.Error Texture)
    | Animate Time
    | WindowResized Window.Size
    | MouseMove Mouse.Position
    | KeyChange Bool Keyboard.KeyCode

speed = 0.2

init: ( Model , Cmd Msg )
init =
    ( Model
        Nothing
        (Window.Size 1 1)
        (Keys False False False False False)
        (vec3 0 0 -10)
        (0 , -90)
        (vec3 0 0 1)
        { x = 0 , y = 0 }
    , Cmd.batch [ loadTexture "textures/thwomp-face.jpg" , Task.perform WindowResized Window.size] )

textures = [ "textures/thwomp-face.jpg" , "textures/uv_big.png" ]

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
            TextureLoaded result -> { model | t1 = Result.toMaybe result }
            KeyChange b k -> { model | keys = getKeys b k model.keys }
            Animate dt -> { model | pos = movePos model.keys model.lookDir model.pos }
            WindowResized size -> { model | winSize = size }
            MouseMove mp ->
                let (ld , py) = getLookPos model.lastMousePos mp model.pitchAndYaw
                in { model | lookDir = ld , lastMousePos = mp , pitchAndYaw = py }
    in ( m , Cmd.none )

view: Model -> Html Msg
view model =
    case model.t1 of
        Just t -> WebGL.toHtml
            [ width model.winSize.width , height model.winSize.height , style [ ( "display" , "block") ] ]
            [ getEntity model wall (texturedPlane |> WebGL.triangles) t
            , getEntity model floor (texturedPlane |> WebGL.triangles) t ]
        Nothing ->
            Html.text "Loading texture"

wall = Mat4.identity
floor = Mat4.makeTranslate3 0 -1 0
        |> Mat4.rotate (pi / 2) ( vec3 1 0 0)
        |> Mat4.scale3 50 50 0

getEntity: Model -> Mat4 -> Mesh Vertex -> Texture -> WebGL.Entity
getEntity model local mesh tex =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        (Uniforms
            (Mat4.makePerspective 50 (toFloat model.winSize.width / toFloat model.winSize.height) 0.01 1000)
            (Mat4.makeLookAt model.pos (Vec3.add model.pos model.lookDir) Vec3.j)
            local
            tex)

getLookPos: Mouse.Position -> Mouse.Position -> ( Float , Float ) -> ( Vec3 , (Float , Float) )
getLookPos lmp mp ( lastPitch , lastYaw ) =
    let sensitivity = 0.005
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


texturedPlane : List ( Vertex, Vertex, Vertex )
texturedPlane =
    let topLeft = Vertex (vec3 -1 1 1) (vec2 0 1)
        topRight = Vertex (vec3 1 1 1) (vec2 1 1)
        bottomLeft = Vertex (vec3 -1 -1 1) (vec2 0 0)
        bottomRight = Vertex (vec3 1 -1 1) (vec2 1 0)
    in [ ( topLeft, topRight, bottomLeft ) , ( bottomLeft, topRight, bottomRight ) ]

movePos: Keys -> Vec3 -> Vec3 -> Vec3
movePos { left , down , up , right , space } lookDir pos =
    let lookDir_ = Vec3.setY 0 lookDir
        forward = if up then 1 else if down then -1 else 0
        strafe = if right then 1 else if left then -1 else 0
        cross = Vec3.cross lookDir_ Vec3.j
        dir = Vec3.add (Vec3.scale strafe cross) (Vec3.scale forward lookDir_)
        dir_ = if Vec3.length dir <= 0 then dir else Vec3.normalize dir
    in Vec3.add pos dir_

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


loadTexture: String -> Cmd Msg
loadTexture path =
    Task.attempt TextureLoaded (Texture.load path)


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
