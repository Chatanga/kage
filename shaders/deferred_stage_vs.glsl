#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in vec3 inNormal;

out vec3 position;
out vec2 texCoord;
out vec3 normal;

void main() {
    gl_Position = projection * camera * transformation * vec4(inPosition, 1.0);
    position = gl_Position.xyz;
    texCoord = inTexCoord;
    normal = (transformation * vec4(inNormal, 0.0)).xyz;
}
