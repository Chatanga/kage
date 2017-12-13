#version 400 core

uniform sampler2D sampler;
uniform bool horizontal;

in vec2 texCoord;

layout(location = 0) out vec3 result;

const float weight[5] = float[] (0.227027, 0.1945946, 0.1216216, 0.054054, 0.016216);

// Gaussian blur
void main()
{
    vec2 tex_offset = 1.0 / textureSize(sampler, 0); // gets size of single texel
    result = texture(sampler, texCoord).rgb * weight[0]; // current fragment's contribution
    if (horizontal)
    {
        for(int i = 1; i < 5; ++i)
        {
            result += texture(sampler, texCoord + vec2(tex_offset.x * i, 0.0)).rgb * weight[i];
            result += texture(sampler, texCoord - vec2(tex_offset.x * i, 0.0)).rgb * weight[i];
        }
    }
    else
    {
        for(int i = 1; i < 5; ++i)
        {
            result += texture(sampler, texCoord + vec2(0.0, tex_offset.y * i)).rgb * weight[i];
            result += texture(sampler, texCoord - vec2(0.0, tex_offset.y * i)).rgb * weight[i];
        }
    }
}

