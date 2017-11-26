#version 400 core

layout(location = 0) in vec3 inPosition;
layout(location = 3) in vec3 inNormal;

out vec3 normal;

void main() {
    gl_Position = vec4(inPosition, 1.0);
    normal = inNormal;
}

