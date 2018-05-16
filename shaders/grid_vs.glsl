#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform vec3 cameraPosition;

layout(location = 0) in vec3 inPosition;

out vec3 position;

void main() {
    position = inPosition;
    vec4 p = vec4(inPosition.x + cameraPosition.x, inPosition.y + cameraPosition.y, inPosition.z, 1.0);
    gl_Position = projection * camera * p;
}
