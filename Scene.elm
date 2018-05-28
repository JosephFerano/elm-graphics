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
import OBJ
import OBJ.Types

type alias ObjMesh = OBJ.Types.MeshWith OBJ.Types.Vertex

main : Program Never Model Msg
main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }


type alias Model =
    { texDict : Dict String Texture
    , objDict : Dict String (OBJ.Types.MeshWith OBJ.Types.Vertex)
    , winSize : Window.Size
    , keys : Keys
    , pos : Vec3
    , pitchAndYaw : ( Float , Float )
    , lookDir : Vec3
    , lastMousePos : Mouse.Position }

type alias UTVertex = { position : Vec3 , coord : Vec2 }
type alias Keys = { left : Bool ,  down : Bool , up : Bool , right : Bool , space : Bool }

type Msg
    = TextureLoaded (Result Texture.Error (String , Texture))
    | ObjLoaded (Result String (String , ObjMesh))
    | Animate Time
    | WindowResized Window.Size
    | MouseMove Mouse.Position
    | KeyChange Bool Keyboard.KeyCode

init: ( Model , Cmd Msg )
init =
    ( Model
        Dict.empty
        Dict.empty
        (Window.Size 1 1)
        (Keys False False False False False)
        (vec3 0 0 -10)
        (0 , -90)
        (vec3 0 0 1)
        { x = 0 , y = 0 }
    , Cmd.batch
        [ loadTex "Thwomp" "textures/thwomp-face.jpg"
        , loadTex "UV" "textures/uv_big.png"
        , loadTex "Tetra" "textures/tetra.png"
        , loadObj "Teapot" "suz.obj"
        , Task.perform WindowResized Window.size] )

loadTex: String -> String -> Cmd Msg
loadTex id path =
    Task.attempt
        TextureLoaded
        (Texture.load path
        |> Task.map (\t -> (id , t) ) )

loadObj: String -> String -> Cmd Msg
loadObj id path =
    OBJ.loadMeshWithoutTexture path (\ r -> Result.map (\ o -> (id , o)) r ) |> Cmd.map ObjLoaded

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
            KeyChange b k -> { model | keys = getKeys b k model.keys }
            Animate dt -> { model | pos = movePos model.keys model.lookDir model.pos }
            WindowResized size -> { model | winSize = size }
            TextureLoaded result ->
                case result of
                    Ok (id , tex) -> { model | texDict = Dict.insert id tex model.texDict }
                    Err e -> model
            ObjLoaded result ->
                case result of
                    Ok (id , obj) -> { model | objDict = Dict.insert id obj model.objDict }
                    Err e -> model |> Debug.log e
            MouseMove mp ->
                let (ld , py) = getLookPos model.lastMousePos mp model.pitchAndYaw
                in { model | lookDir = ld , lastMousePos = mp , pitchAndYaw = py }
    in ( m , Cmd.none )

view: Model -> Html Msg
view model =
    WebGL.toHtml
        [ width model.winSize.width , height model.winSize.height , style [ ( "display" , "block") ] ]
        ([ getEntity model wall (texturedPlane |> WebGL.triangles) "Thwomp"
        , getEntity model tetraB (tetraBasic |> WebGL.triangles) "UV"
        , getEntity model tetra (tetraF |> WebGL.triangles) "Tetra"
        , getModel model (Mat4.makeScale3 5 5 5) "Teapot"
        , getEntity model floor (texturedPlane |> WebGL.triangles) "UV" ] |> List.concat)

getModel: Model -> Mat4 -> String -> List WebGL.Entity
getModel model local id =
    case Dict.get id model.objDict of
        Just mesh ->
            case Dict.get "UV" model.texDict of
                Just t -> [ WebGL.entity
                    unlitColorVS
                    unlitColorFS
                    (WebGL.indexedTriangles mesh.vertices mesh.indices)
                    (UnlitColor
                        (projectionMatrix model)
                        (viewMatrix model)
                        local
                        (colorToVec3 Color.blue)) ]
                Nothing -> []
        Nothing -> []

colorToVec3: Color -> Vec3
colorToVec3 color =
    let to01 x = toFloat x / 255
        c = Color.toRgb color
    in vec3 (to01 c.red) (to01 c.green) (to01 c.blue)

getEntity: Model -> Mat4 -> Mesh UTVertex -> String -> List WebGL.Entity
getEntity model local mesh texId =
    case Dict.get texId model.texDict of
        Just t ->
            [ WebGL.entity
                unlitTexturedVS
                unlitTexturedFS
                mesh
                (UnlitTextured (projectionMatrix model) (viewMatrix model ) local t) ]
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


-------------
-- Geometry
-------------

wall = Mat4.identity
floor = Mat4.makeTranslate3 0 -1 0
        |> Mat4.rotate (pi / 2) ( vec3 1 0 0)
        |> Mat4.scale3 15 15 0
tetraB = Mat4.makeTranslate3 -5 1.5 5
        |> Mat4.scale3 2 2 2
tetra = Mat4.makeTranslate3 5 0 5

tetraBasic: List ( UTVertex, UTVertex, UTVertex )
tetraBasic =
    let peak = UTVertex (vec3 0 1 0) (vec2 1 1)
        bottomLeft = UTVertex (vec3 -1 -1 -1) (vec2 0 0)
        bottomRight = UTVertex (vec3 -1 -1 1) (vec2 1 0)
        topLeft = UTVertex (vec3 1 -1 1) (vec2 0 0)
        topRight = UTVertex (vec3 1 -1 -1) (vec2 0 1)
    in [ ( peak , bottomLeft , bottomRight )
       , ( peak , bottomLeft , topRight )
       , ( peak , bottomRight , topLeft )
       , ( peak , topRight , topLeft )
       , ( bottomLeft , bottomRight , topRight)
       , ( bottomRight, topLeft , topRight ) ]

tetraF: List ( UTVertex, UTVertex, UTVertex )
tetraF =
    let f0a = UTVertex (vec3 -1 -1 1) (vec2 0 0.5)
        f0b = UTVertex (vec3 1 -1 1) (vec2 0.5 0.5)
        f0c = UTVertex (vec3 0 1 0) (vec2 0.25 1)

        f1a = UTVertex (vec3 -1 -1 -1) (vec2 0.5 0.5)
        f1b = UTVertex (vec3 -1 -1 1) (vec2 1 0.5)
        f1c = UTVertex (vec3 0 1 0) (vec2 0.75 1)

        f2a = UTVertex (vec3 1 -1 -1) (vec2 0 0)
        f2b = UTVertex (vec3 -1 -1 -1) (vec2 0.5 0)
        f2c = UTVertex (vec3 0 1 0) (vec2 0.25 0.5)

        f3a = UTVertex (vec3 1 -1 1) (vec2 0.5 0)
        f3b = UTVertex (vec3 1 -1 -1) (vec2 1 0)
        f3c = UTVertex (vec3 0 1 0) (vec2 0.75 0.5)
    in [ ( f0a , f0b , f0c ) , ( f1a , f1b , f1c ) , ( f2a , f2b , f2c ) , ( f3a , f3b , f3c ) ]

texturedPlane: List ( UTVertex, UTVertex, UTVertex )
texturedPlane =
    let topLeft = UTVertex (vec3 -1 1 1) (vec2 0 1)
        topRight = UTVertex (vec3 1 1 1) (vec2 1 1)
        bottomLeft = UTVertex (vec3 -1 -1 1) (vec2 0 0)
        bottomRight = UTVertex (vec3 1 -1 1) (vec2 1 0)
    in [ ( topLeft, topRight, bottomLeft ) , ( bottomLeft, topRight, bottomRight ) ]

-------------
-- Uniforms
-------------

type alias UnlitColor = { projection : Mat4 , view : Mat4 , model : Mat4 , color : Vec3 }
type alias UnlitTextured = { projection : Mat4 , view : Mat4 , model : Mat4 , texture : Texture }

-------------
-- Shaders
-------------

unlitColorVS: Shader OBJ.Types.Vertex UnlitColor { vcolor : Vec3 }
unlitColorVS =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform mat4 projection;
        uniform mat4 view;
        uniform mat4 model;
        uniform vec3 color;
        varying vec3 vcolor;

        void main()
        {
            gl_Position = projection * view * model * vec4(position, 1.0);
            vcolor = color;
        }
    |]

unlitColorFS: Shader {} UnlitColor { vcolor : Vec3 }
unlitColorFS =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main()
        {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]


unlitTexturedVS: Shader UTVertex UnlitTextured { vcoord : Vec2 }
unlitTexturedVS =
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

unlitTexturedFS: Shader {} UnlitTextured { vcoord : Vec2 }
unlitTexturedFS =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;

        void main()
        {
            gl_FragColor = texture2D(texture, vcoord);
        }
    |]
