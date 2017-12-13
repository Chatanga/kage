#version 400 core

uniform sampler2D samplers[2];

in vec2 texCoord;

void main()
{
    vec3 ldrColor = texture(samplers[0], texCoord).rgb;
    vec3 bloom = texture(samplers[1], texCoord).rgb;
    gl_FragColor = vec4(ldrColor + bloom, 1.0);
    //gl_FragColor = vec4(bloom, 1.0);
}

