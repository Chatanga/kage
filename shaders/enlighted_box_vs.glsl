#version 400 core

uniform mat4 shadow;

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in vec3 inNormal;

out vec2 texCoord;
out vec3 normal;
out vec4 worldSpacePosition;
out vec4 shadowCoord;
out vec4 cameraSpacePosition;

void main() {
    worldSpacePosition = transformation * vec4(inPosition, 1.0);
    shadowCoord = shadow * worldSpacePosition;
    cameraSpacePosition = camera * worldSpacePosition;
    gl_Position = projection * cameraSpacePosition;
    texCoord = inTexCoord;
    normal = (transformation * vec4(inNormal, 0.0)).xyz;
}
