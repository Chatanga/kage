#version 400 core

uniform sampler2D sampler;

in vec2 texCoord;

void main()
{
    float i = texture(sampler, texCoord).r;
    gl_FragColor = vec4(i, i, i, 1.0);
}
