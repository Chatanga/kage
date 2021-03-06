#version 400 core

uniform vec3 cameraPosition;

uniform float materialSpecularIntensity;
uniform float materialSpecularPower;

uniform sampler2D samplers[3];

in vec2 texCoord;
in vec3 normal;
in vec4 worldSpacePosition;
in vec4 cameraSpacePosition;

#include "shared/constants.glsl"
#include "shared/lighting.glsl"

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

    gl_FragColor = getFragColor(materialSpecularIntensity, materialSpecularPower, baseColor, normal, cameraSpacePosition, worldSpacePosition);
}

