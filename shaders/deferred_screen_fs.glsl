#version 400 core

uniform mat4 camera;
uniform vec3 cameraPosition;

uniform sampler2D samplers[3];

in vec2 texCoord;

#include "shared/constants.glsl"
#include "shared/lighting.glsl"

void main()
{
    vec4 worldSpacePosition = vec4(texture(samplers[0], texCoord).rgb, 1);
    vec3 normal = texture(samplers[1], texCoord).rgb;
    vec4 albedoSpec = texture(samplers[2], texCoord);
    vec4 albedo = vec4(albedoSpec.rgb, 1);
    float specular = albedoSpec.a;

    vec4 cameraSpacePosition = camera * worldSpacePosition;

    gl_FragColor = getFragColor(specular, 20, albedo, normal, cameraSpacePosition, worldSpacePosition);
}

