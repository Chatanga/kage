#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

uniform int shadowUsed;

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in vec3 inNormal;
layout(location = 4) in vec3 inTangent;

out vec4 worldSpacePosition;
out vec4 cameraSpacePosition;
out vec2 texCoord;
out mat3 tangentSpace;

#include "shared/constants.glsl"

void main() {
    worldSpacePosition = transformation * vec4(inPosition, 1.0);
    cameraSpacePosition = camera * worldSpacePosition;
    gl_Position = projection * cameraSpacePosition;
    texCoord = inTexCoord;
    vec3 normal = vec3(transformation * vec4(inNormal, 0.0));
    vec3 tangent = vec3(transformation * vec4(inTangent, 0.0));
    vec3 bitangent = normalize(cross(normal, tangent));
    tangentSpace = mat3(tangent, bitangent, normal); // From tangent to world space.
}

