#version 400 core

uniform float materialSpecularIntensity;
uniform float materialSpecularPower;

uniform sampler2D samplers[3];

in vec4 worldSpacePosition;
in vec2 texCoord;
in vec3 normal;

layout(location = 0) out vec3 outPosition;
layout(location = 1) out vec3 outNormal;
layout(location = 2) out vec4 outAlbedoSpec;

#include "shared/constants.glsl"

void main()
{
    // Need to be done outside the conditions bellow to avoid mipmapping stitching... But why?
    vec4 baseColor0 = texture(samplers[0], texCoord);
    vec4 baseColor1 = texture(samplers[1], texCoord);
    vec4 baseColor2 = texture(samplers[2], texCoord);

    vec4 baseColor;
    float height = clamp(worldSpacePosition.z / 20, 0.0, 1.0);
    if (height < 0.2) {
        baseColor = baseColor0;
    } else if (height < 0.4) {
        float r = (height - 0.2) / 0.2;
        baseColor = mix(baseColor0, baseColor1, r);
    } else if (height < 0.6) {
        baseColor = baseColor1;
    } else if (height < 0.8) {
        float r = (height - 0.6) / 0.2;
        baseColor = mix(baseColor1, baseColor2, r);
    } else {
        baseColor = baseColor2;
    }

    outPosition = worldSpacePosition.xyz;
    outNormal = normal;
    outAlbedoSpec.rgb = baseColor.rgb;
    outAlbedoSpec.a = materialSpecularIntensity;
}

