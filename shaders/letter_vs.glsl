#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

layout(location = 0) in vec3 vertexPosition;
layout(location = 2) in vec2 inTexCoord;

out vec2 texCoord;

void main() {
    gl_Position = projection * camera * transformation * vec4(vertexPosition, 1.0);
    texCoord = inTexCoord;
}

