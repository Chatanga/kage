#version 400 core

layout (location = 0) out vec3 gPosition;
layout (location = 1) out vec3 gNormal;
layout (location = 2) out vec4 gAlbedoSpec;

in vec3 position;
in vec2 texCoord;
in vec3 normal;

//uniform sampler2D sampler;

void main()
{
    // store the fragment position vector in the first gbuffer texture
    gPosition = normalize(position);

    // also store the per-fragment normals into the gbuffer
    gNormal = normalize(normal);

    // and the diffuse per-fragment color
    //gAlbedoSpec.rgb = texture(sampler, texCoord).rgb;
    gAlbedoSpec.rgb = vec3(1.0, 0.0, 0.0);

    // store specular intensity in gAlbedoSpec's alpha component
    //gAlbedoSpec.a = texture(samplerBis, texCoord).r;
    gAlbedoSpec.a = 1.0;
}
