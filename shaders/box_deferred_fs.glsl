#version 400 core

uniform float materialSpecularIntensity;
uniform float materialSpecularPower;

uniform sampler2D samplers[4];

in vec4 worldSpacePosition;
in vec2 texCoord;
in mat3 tangentSpace;
in vec3 tangent;

layout(location = 0) out vec3 outPosition;
layout(location = 1) out vec3 outNormal;
layout(location = 2) out vec4 outAlbedoSpec;

#include "shared/constants.glsl"

void main()
{
    outPosition = worldSpacePosition.xyz;

    outAlbedoSpec.rgb = texture(samplers[0], texCoord).rgb;

    // obtain normal from normal map in range [0,1]
    outNormal = texture(samplers[1], texCoord).rgb;
    // transform normal vector to range [-1,1]
    outNormal = normalize(outNormal * 2.0 - 1.0);
    outNormal = tangentSpace * outNormal;

    vec4 reflexion = texture(samplers[2], texCoord);
    outAlbedoSpec.a = reflexion.r + reflexion.g + reflexion.b;

    vec4 gloss = texture(samplers[3], texCoord);
}

