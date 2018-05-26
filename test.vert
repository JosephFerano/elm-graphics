attribute vec3 position;
attribute vec2 coord;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 local;
varying vec2 vcoord;

void main()
{
    gl_Position = perspective * camera * local * vec4(position, 1.0);
    vcoord = coord.xy;
}
