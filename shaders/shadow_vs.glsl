#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

layout(location = 0) in vec3 inPosition;

void main() {
    gl_Position = projection * camera * transformation * vec4(inPosition, 1.0);
}

