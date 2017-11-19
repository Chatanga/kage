#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

uniform int shadowUsed;

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in vec3 inNormal;

out vec4 worldSpacePosition;
out vec4 cameraSpacePosition;
out vec2 texCoord;
out vec3 normal;

#include "shared/constants.glsl"

void main() {
    worldSpacePosition = transformation * vec4(inPosition, 1.0);
    cameraSpacePosition = camera * worldSpacePosition;
    gl_Position = projection * cameraSpacePosition;
    texCoord = inTexCoord;
    normal = (transformation * vec4(inNormal, 0.0)).xyz;
}

