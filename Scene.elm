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
import Shaders exposing (..)

type alias ObjVert = OBJ.Types.Vertex
type alias ObjMesh = OBJ.Types.MeshWith ObjVert

main : Program Never Model Msg
main = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }


type alias Model =
    { texDict : Dict String Texture
    , objDict : Dict String ObjMesh
    , rot : Float
    , winSize : Window.Size
    , keys : Keys
    , pos : Vec3
    , pitchAndYaw : ( Float , Float )
    , lookDir : Vec3
    , lastMousePos : Mouse.Position
    , robot : Robot }

type alias Robot = { pos : Vec3 , rot : Float , armRot : Float , handRot : Float }

type alias Keys =
    { left : Bool , down : Bool , up : Bool , right : Bool
    , w : Bool , a : Bool , s : Bool , d : Bool
    , y : Bool , h : Bool , u : Bool , j : Bool
    , n : Bool , m : Bool }

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
        0
        (Window.Size 1 1)
        -- God this is horrendous
        (Keys False False False False False False False False False False False False False False)
        (vec3 0 0 -10)
        (0 , -90)
        (vec3 0 0 1)
        { x = 0 , y = 0 }
        { pos = vec3 0 -0.5 -3 , rot = 45 , armRot = 0 , handRot = 0 }
    , Cmd.batch
        [ loadTex "Thwomp" "textures/thwomp-face.jpg"
        , loadTex "UV" "textures/uv_big.png"
        , loadTex "Tetra" "textures/tetra.png"
        , loadObj "Teapot" "models/suz.obj"
        , Task.perform WindowResized Window.size] )

loadTex: String -> String -> Cmd Msg
loadTex id path =
    Task.attempt TextureLoaded (Texture.load path |> Task.map (\t -> (id , t) ) )

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
            Animate dt ->
                { model
                | pos = movePos
                    (model.keys.a , model.keys.s , model.keys.w , model.keys.d)
                    model.lookDir
                    model.pos
                    0.2
                , robot = updateRobot model dt
                , rot = model.rot + 0.001 * dt}
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

updateRobot: Model -> Time -> Robot
updateRobot { robot , keys } dt =
    let rr part = part + dt * 0.005
        rl part = part - dt * 0.005
        rot = radians robot.rot
    in { pos =
           movePos
               (keys.left, keys.down, keys.up, keys.right)
               (vec3 (sin rot) (sin rot) (cos rot))
               robot.pos
               0.1
    , rot = if keys.n then rr robot.rot else if keys.m then rl robot.rot else robot.rot
    , armRot =
        let angle = if keys.y then rr robot.armRot else if keys.h then rl robot.armRot else robot.armRot
        in clamp -0.5 2.5 angle
    , handRot =
        let angle = if keys.u then rr robot.handRot else if keys.j then rl robot.handRot else robot.handRot
        in clamp -1 1 angle}

view: Model -> Html Msg
view model =
    WebGL.toHtml
        [ width model.winSize.width , height model.winSize.height , style [ ( "display" , "block") ] ]
        ([ getEntity model wall texturedPlane "Thwomp"
        , getEntity model tetraB tetraBasic "UV"
        , getEntity model tetra tetraF "Tetra"
        , getRobot model
        , getModel model (Mat4.makeRotate model.rot Vec3.j) "Teapot"
        , getEntity model floor texturedPlane "UV" ] |> List.concat)

getRobot: Model -> List WebGL.Entity
getRobot model =
    let body = Mat4.makeTranslate model.robot.pos
            |> Mat4.rotate model.robot.rot Vec3.j
        arm  = Mat4.makeTranslate3 0 0 -0.35
            |> Mat4.rotate model.robot.armRot Vec3.i
            |> Mat4.inverse |> Maybe.withDefault Mat4.identity
            |> Mat4.mul (Mat4.makeTranslate3 0 0.5 0.5)
            |> Mat4.mul body
        hand = Mat4.makeTranslate3 0 0 -0.25
            |> Mat4.rotate model.robot.handRot Vec3.i
            |> Mat4.inverse |> Maybe.withDefault Mat4.identity
            |> Mat4.mul (Mat4.makeTranslate3 0 0 0.6)
            |> Mat4.mul arm
    in [ getEntity3 model (body |> Mat4.scale3 0.5 0.5 0.5) cube Color.blue
       , getEntity3 model (arm |> Mat4.scale3 0.2 0.2 0.5) cube Color.green
       , getEntity3 model (hand |> Mat4.scale3 0.15 0.15 0.45) cube Color.red
       ] |> List.concat

getModel: Model -> Mat4 -> String -> List WebGL.Entity
getModel model local id =
    case Dict.get id model.objDict of
        Just mesh ->
            case Dict.get "UV" model.texDict of
                Just t -> [ WebGL.entity
                    diffuseVS
                    diffuseFS
                    (WebGL.indexedTriangles mesh.vertices mesh.indices)
                    (DiffuseColor
                        (projectionMatrix model)
                        (viewMatrix model)
                        local
                        (colorToVec3 Color.blue)
                        (vec3 1 1 0)
                        (vec3 1 0 1)
                        (vec3 0 0 1)
                        1.0) ]
                Nothing -> []
        Nothing -> []

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

getEntity2: Model -> Mat4 -> Mesh ObjVert -> Color -> List WebGL.Entity
getEntity2 model local mesh color =
    [ WebGL.entity
        unlitColorVS
        unlitColorFS
        mesh
        (UnlitColor (projectionMatrix model) (viewMatrix model ) local (colorToVec3 color) ) ]

getEntity3: Model -> Mat4 -> Mesh ObjVert -> Color -> List WebGL.Entity
getEntity3 model local mesh color =
    [ WebGL.entity
        diffuseVS
        diffuseFS
        mesh
        (DiffuseColor
            (projectionMatrix model)
            (viewMatrix model)
            local
            (colorToVec3 color)
            (vec3 1 1 0)
            (vec3 1 0 1)
            (vec3 0 0 1)
            1.0 ) ]

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

colorToVec3: Color -> Vec3
colorToVec3 color =
    let to01 x = toFloat x / 255
        c = Color.toRgb color
    in vec3 (to01 c.red) (to01 c.green) (to01 c.blue)

movePos: (Bool, Bool, Bool, Bool) -> Vec3 -> Vec3 -> Float -> Vec3
movePos ( left , down , up , right ) lookDir pos speed =
    let lookDir_ = Vec3.setY 0 lookDir
        forward = if up then 1 else if down then -1 else 0
        strafe = if right then 1 else if left then -1 else 0
        cross = Vec3.cross lookDir_ Vec3.j
        dir = Vec3.add (Vec3.scale strafe cross) (Vec3.scale forward lookDir_)
        dir_ = if Vec3.length dir <= 0 then dir else Vec3.normalize dir
    in Vec3.add pos <| Vec3.scale speed dir_

getKeys: Bool -> KeyCode -> Keys -> Keys
getKeys isOn code keys =
    case code of
        -- ◀ ▼ ▲ ▶
        37 -> { keys | left = isOn }
        40 -> { keys | down = isOn }
        38 -> { keys | up = isOn }
        39 -> { keys | right = isOn }
        -- WASD
        87 -> { keys | w = isOn }
        65 -> { keys | a = isOn }
        83 -> { keys | s = isOn }
        68 -> { keys | d = isOn }
        -- YHUJNM
        89 -> { keys | y = isOn }
        72 -> { keys | h = isOn }
        85 -> { keys | u = isOn }
        74 -> { keys | j = isOn }
        78 -> { keys | n = isOn }
        77 -> { keys | m = isOn }
        _ -> keys


-------------
-- Geometry
-------------

wall = Mat4.makeTranslate3 0 0 3
floor = Mat4.makeTranslate3 0 -1 0
        |> Mat4.rotate (pi / -2) ( vec3 1 0 0)
        |> Mat4.rotate pi ( vec3 0 0 1)
        |> Mat4.scale3 15 15 0
tetraB = Mat4.makeTranslate3 -5 1.5 5
        |> Mat4.scale3 2 2 2
tetra = Mat4.makeTranslate3 5 0 5

-- right/left front/back top/bottom
cube: Mesh ObjVert
cube =
    let rtf = vec3 1 1 1
        ltf = vec3 -1 1 1
        ltb = vec3 -1 1 -1
        rtb = vec3 1 1 -1
        rbb = vec3 1 -1 -1
        rbf = vec3 1 -1 1
        lbf = vec3 -1 -1 1
        lbb = vec3 -1 -1 -1
        front = Vec3.k
        back = Vec3.scale -1 front
        top = Vec3.j
        bottom = Vec3.scale -1 top
        right = Vec3.i
        left = Vec3.scale -1 right
    in
        [ face right rtf rbf rbb rtb
        , face left ltf lbf lbb ltb
        , face front rtf rbf lbf ltf
        , face back rtb rbb lbb ltb
        , face top rtf ltf ltb rtb
        , face bottom rbf lbf lbb rbb
        ] |> List.concat
          |> WebGL.triangles

face: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (ObjVert , ObjVert , ObjVert)
face norm a b c d =
    let v pos = OBJ.Types.Vertex pos norm
    in [ ( v a , v b , v c ) , ( v c , v d , v a ) ]

tetraBasic: Mesh UTVertex
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
       |> WebGL.triangles

tetraF: Mesh UTVertex
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
       |> WebGL.triangles

texturedPlane: Mesh UTVertex
texturedPlane =
    let topLeft = UTVertex (vec3 -1 1 1) (vec2 0 1)
        topRight = UTVertex (vec3 1 1 1) (vec2 1 1)
        bottomLeft = UTVertex (vec3 -1 -1 1) (vec2 0 0)
        bottomRight = UTVertex (vec3 1 -1 1) (vec2 1 0)
    in [ ( topLeft, topRight, bottomLeft ) , ( bottomLeft, topRight, bottomRight ) ]
       |> WebGL.triangles

