#version 400 core

uniform float materialSpecularIntensity;
uniform float materialSpecularPower;

uniform sampler2D sampler;

in vec4 worldSpacePosition;
in vec2 texCoord;
in vec3 normal;

layout(location = 0) out vec3 outPosition;
layout(location = 1) out vec3 outNormal;
layout(location = 2) out vec4 outAlbedoSpec;

#include "shared/constants.glsl"

void main()
{
    outPosition = worldSpacePosition.xyz;
    outNormal = normal;
    outAlbedoSpec.rgb = texture(sampler, texCoord).rgb;
    outAlbedoSpec.a = materialSpecularIntensity; // TODO materialSpecularPower lost in translation...
}

