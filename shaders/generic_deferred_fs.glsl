#version 400 core

in vec3 position;
in vec3 normal;
in vec3 albedo;
in float specular;

layout(location = 0) out vec3 outPosition;
layout(location = 1) out vec3 outNormal;
layout(location = 2) out vec4 outAlbedoSpec;

void main() {
    // store the fragment position vector in the first gbuffer texture
    gPosition = normalize(position);

    // also store the per-fragment normals into the gbuffer
    gNormal = normalize(normal);

    // and the diffuse per-fragment color
    gAlbedoSpec.rgb = albedo;

    // store specular intensity in gAlbedoSpec's alpha component
    gAlbedoSpec.a = specular;
}
