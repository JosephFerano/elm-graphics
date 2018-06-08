module Shaders exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture as Texture exposing (..)
import OBJ
import OBJ.Types

type alias UnlitColor = { projection : Mat4 , view : Mat4 , model : Mat4 , color : Vec3 }
type alias UnlitTextured = { projection : Mat4 , view : Mat4 , model : Mat4 , texture : Texture }
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


unlitTexturedVS: Shader { position : Vec3 , coord : Vec2 } UnlitTextured { vcoord : Vec2 }
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
            vcoord = coord;
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
