#version 400 core

uniform sampler2D sampler;

in vec2 texCoord;

layout(location = 0) out vec3 outLdrColor;
layout(location = 1) out vec3 outBloom;

vec4 passThroughAlgorithm(vec3 hdrColor)
{
    return vec4(hdrColor, 1.0);
}

vec4 reinhardAlgorithm(vec3 hdrColor)
{
    const float gamma = 2.2;

    // reinhard tone mapping
    vec3 mapped = hdrColor / (hdrColor + vec3(1.0));
    // gamma correction
    mapped = pow(mapped, vec3(1.0 / gamma));

    return vec4(mapped, 1.0);
}

/*uniform*/ float exposure = 1.0;

vec4 exposureAlgorithm(vec3 hdrColor)
{
    const float gamma = 2.2;

    // Exposure tone mapping
    vec3 mapped = vec3(1.0) - exp(-hdrColor * exposure);
    // Gamma correction
    mapped = pow(mapped, vec3(1.0 / gamma));

    return vec4(mapped, 1.0);
}

void main()
{
    vec3 hdrColor = texture(sampler, texCoord).rgb;
    outLdrColor = exposureAlgorithm(hdrColor).rgb;
    float i = (hdrColor.r + hdrColor.g + hdrColor.b) / 3.0;
    outBloom = i > 1.0 ? outLdrColor : vec3(0.0, 0.0, 0.0);
}

