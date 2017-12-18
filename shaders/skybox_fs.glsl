#version 400 core

uniform sampler2D samplers[6];

in vec3 texCoord;

vec4 getTexture(int index)
{
    switch (index) {
    case 0:
        return texture(samplers[0], texCoord.xy);
    case 1:
        return texture(samplers[1], texCoord.xy);
    case 2:
        return texture(samplers[2], texCoord.xy);
    case 3:
        return texture(samplers[3], texCoord.xy);
    case 4:
        return texture(samplers[4], texCoord.xy);
    case 5:
        return texture(samplers[5], texCoord.xy);
    default:
        return texture(samplers[0], texCoord.xy);
    }
}

void main()
{
    gl_FragColor = getTexture(int(texCoord.z));
}

