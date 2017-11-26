#version 400 core

uniform vec3 cameraPosition;

uniform sampler2D samplers[4];

in vec2 texCoord;
in mat3 tangentSpace;
in vec4 worldSpacePosition;
in vec4 cameraSpacePosition;

#include "shared/constants.glsl"
#include "shared/lighting.glsl"

void main()
{
    vec4 baseColor = texture(samplers[0], texCoord);

    // obtain normal from normal map in range [0,1]
    vec3 normal = texture(samplers[1], texCoord).rgb;
    // transform normal vector to range [-1,1]
    normal = normalize(normal * 2.0 - 1.0);
    normal = tangentSpace * normal;

    vec4 reflexion = texture(samplers[2], texCoord);

    vec4 gloss = texture(samplers[3], texCoord);

    float materialSpecularIntensity = reflexion.r + reflexion.g + reflexion.b;
    float materialSpecularPower = 20;

    gl_FragColor = getFragColor(materialSpecularIntensity, materialSpecularPower, baseColor, normal, cameraSpacePosition, worldSpacePosition);
}

