#version 400 core

uniform vec3 cameraPosition;

uniform float materialSpecularIntensity;
uniform float materialSpecularPower;

uniform vec4 color;

in vec4 worldSpacePosition;
in vec4 cameraSpacePosition;
in vec3 normal;

#include "shared/constants.glsl"
#include "shared/lighting.glsl"

void main()
{
    gl_FragColor = getFragColor(materialSpecularIntensity, materialSpecularPower, color, normal, cameraSpacePosition, worldSpacePosition);
}

