#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

uniform sampler2D samplers[4];

uniform vec3 samples[64];

uniform float screenWidth;
uniform float screenHeight;

in vec2 texCoord;

layout(location = 0) out float occlusion;

void main()
{
    vec2 noiseScale = vec2(screenWidth / 4.0, screenHeight / 4.0);

    vec3 noise = texture(samplers[0], texCoord * noiseScale).xyz;
    vec3 worldSpacePosition = texture(samplers[1], texCoord).xyz;
    vec3 worldSpaceNormal = texture(samplers[2], texCoord).xyz;
    vec4 albedoSpec = texture(samplers[3], texCoord);
    vec4 albedo = vec4(albedoSpec.rgb, 1);
    float specular = albedoSpec.a;

    vec3 viewSpacePosition = (camera * vec4(worldSpacePosition, 1.0)).xyz;
    vec3 viewSpaceNormal = (camera * vec4(worldSpaceNormal, 0.0)).xyz;

    // Banding appears with random noise...
    noise = vec3(1, 0, 0);
    vec3 tangent = normalize(noise - viewSpaceNormal * dot(noise, viewSpaceNormal));
    vec3 bitangent = cross(viewSpaceNormal, tangent);
    mat3 TBN = mat3(tangent, bitangent, viewSpaceNormal);

    const int kernelSize = 64;
    const float radius = 0.5;
    const float bias = 0.025;

    occlusion = 0.0;
    for (int i = 0; i < kernelSize; ++i)
    {
        // get theSample position
        vec3 theSample = TBN * samples[i]; // From tangent to view-space
        theSample = viewSpacePosition + theSample * radius;

        vec4 offset = vec4(theSample, 1.0);
        offset = projection * offset; // from view to clip-space
        offset.xyz /= offset.w; // perspective divide
        offset.xyz = offset.xyz * 0.5 + 0.5; // transform to range 0.0 - 1.0

        vec3 worldSpaceOffset = texture(samplers[1], offset.xy).xyz;
        vec3 viewSpaceOffset = (camera * vec4(worldSpaceOffset, 1.0)).xyz;
        float sampleDepth = viewSpaceOffset.z; // float sampleDepth = texture(samplers[1], offset.xy).z;

        float rangeCheck = smoothstep(0.0, 1.0, radius / abs(viewSpacePosition.z - sampleDepth));
        occlusion += (sampleDepth >= theSample.z + bias ? 1.0 : 0.0) * rangeCheck;
    }
    occlusion = 1.0 - (occlusion / kernelSize);
}
