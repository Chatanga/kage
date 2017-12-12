#version 400 core

uniform sampler2D sampler;

in vec2 texCoord;

layout(location = 0) out float result;

void main()
{
    vec2 texelSize = 1.0 / vec2(textureSize(sampler, 0));
    result = 0.0;
    for (int x = -2; x < 2; ++x)
    {
        for (int y = -2; y < 2; ++y)
        {
            vec2 offset = vec2(float(x), float(y)) * texelSize;
            result += texture(sampler, texCoord + offset).r;
        }
    }
    result /= (4.0 * 4.0);
}
