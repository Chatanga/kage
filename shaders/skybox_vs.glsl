#version 400 core

uniform mat4 projection;
uniform mat4 camera;

layout(location = 0) in vec3 inPosition;
layout(location = 2) in vec3 inTexCoord;

out vec3 texCoord;

void main() {
    mat4 centerCamera = camera;
    // Negate the distance to the camera.
    centerCamera[3][0] = 0;
    centerCamera[3][1] = 0;
    centerCamera[3][2] = 0;
    gl_Position = projection * centerCamera * vec4(inPosition, 1.0);
    texCoord = inTexCoord;
}

