#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec3 vertexColour;

out vec3 colour;

void main() {
    colour = vertexColour;
    gl_Position = projection * camera * transformation * vec4(vertexPosition, 1.0);
}
