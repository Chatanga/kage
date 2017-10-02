#version 400 core

uniform sampler2D sampler;

in vec2 texCoord;

out vec4 color;

void main()
{
    color = texture(sampler, texCoord);
}
