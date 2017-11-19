#version 400 core

uniform vec3 cameraPosition;

uniform float materialSpecularIntensity;
uniform float materialSpecularPower;

uniform sampler2D samplers[3];

uniform vec4 grassColor;
uniform float grassAlphaTest;
uniform float grassAlphaMultiplier;

in vec2 texCoord;
flat in int textureIndex;
in vec4 worldSpacePosition;
in vec4 cameraSpacePosition;

#include "shared/constants.glsl"
#include "shared/lighting.glsl"

// -----------------------------------------------------------------------------

vec4 getTexture(int index)
{
    switch (index) {
    case 0:
        return texture(samplers[0], texCoord);
    case 1:
        return texture(samplers[1], texCoord);
    case 2:
        return texture(samplers[2], texCoord);
    default:
        return texture(samplers[0], texCoord);
    }
}

void main()
{
    vec3 normal = vec3(0, 0, 1);

    vec4 baseColor = getTexture(textureIndex);
    float boostedAlpha = baseColor.a * grassAlphaMultiplier;
    if (boostedAlpha < grassAlphaTest) discard;
    if (boostedAlpha > 1.0f) boostedAlpha = 1.0f;
    baseColor *= grassColor;
    baseColor.a = boostedAlpha;

    gl_FragColor = getFragColor(materialSpecularIntensity, materialSpecularPower, baseColor, normal, cameraSpacePosition, worldSpacePosition);
}

