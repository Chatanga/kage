#version 400 core

uniform sampler2D sampler;

in vec2 texCoord;

void main()
{
    gl_FragColor = texture(sampler, texCoord);
}
