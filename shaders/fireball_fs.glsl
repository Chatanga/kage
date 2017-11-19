#version 400 core

uniform vec4 emissiveColor;

in float angleWithCamera;

#include "shared/constants.glsl"

void main() {
    gl_FragColor = mix(emissiveColor, vec4(1, 1, 1, 1), pow(angleWithCamera, 2));
}

