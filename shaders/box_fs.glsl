#version 400 core

uniform vec3 cameraPosition;

uniform float materialSpecularIntensity;
uniform float materialSpecularPower;

uniform sampler2D sampler;

in vec2 texCoord;
in vec3 normal;
in vec4 worldSpacePosition;
in vec4 cameraSpacePosition;

#include "shared/constants.glsl"
#include "shared/lighting.glsl"

void main()
{
    vec4 baseColor = texture(sampler, texCoord);
    gl_FragColor = getFragColor(materialSpecularIntensity, materialSpecularPower, baseColor, normal, cameraSpacePosition, worldSpacePosition);
}

