#version 400 core

uniform vec4 emissiveColor;
uniform vec3 sunLightColor;
uniform vec3 sunLightDirection;
uniform float sunLightAmbientIntensity;

in vec3 normal;

out vec4 fragColour;

void main() {
    float diffuseIntensity = max(0.0, dot(normalize(normal), -sunLightDirection));
    fragColour = emissiveColor * vec4(sunLightColor * (sunLightAmbientIntensity + diffuseIntensity), 1.0);
}
