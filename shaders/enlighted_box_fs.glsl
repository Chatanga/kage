#version 400 core

uniform sampler2D shadowMap;

uniform sampler2D sampler;

uniform vec4 emissiveColor;
uniform vec3 sunLightColor;
uniform vec3 sunLightDirection;
uniform float sunLightAmbientIntensity;

uniform vec4 fogColor;
uniform float fogStart; // This is only for linear fog
uniform float fogEnd; // This is only for linear fog
uniform float fogDensity; // For exp and exp2 equation
uniform int fogEquation; // 0 = linear, 1 = exp, 2 = exp2

uniform int lightCount;
uniform vec3 lightPositions[100];
uniform vec3 lightColors[100];

uniform float materialSpecularIntensity;
uniform float materialSpecularPower;

uniform vec3 cameraPosition;

in vec2 texCoord;
in vec3 normal;
in vec4 worldSpacePosition;
in vec4 shadowCoord;
in vec4 cameraSpacePosition;

out vec4 color;

vec3 getSpecularColor(vec3 lightColor, vec3 rayDirection);
float getVisibility();
float getFogFactor(float fogDistance);

vec2 poissonDisk[4] = vec2[](
    vec2( -0.94201624, -0.39906216 ),
    vec2( 0.94558609, -0.76890725 ),
    vec2( -0.094184101, -0.92938870 ),
    vec2( 0.34495938, 0.29387760 )
);

void main()
{
    vec4 texColor = texture(sampler, texCoord);
    float diffuseIntensity = max(0.0, dot(normalize(normal), -sunLightDirection)) * getVisibility();
    vec4 baseColor = texColor * vec4(sunLightColor * (sunLightAmbientIntensity + diffuseIntensity) + getSpecularColor(sunLightColor, sunLightDirection), 1.0);

    // Light contributions
    for (int i = 0; i < min(10, lightCount); ++i) {
        float distance = length(worldSpacePosition - vec4(lightPositions[i], 1.0));
        float attenuation = 0.01 + 0.07 * distance + 0.00008 * distance * distance;
        baseColor += vec4(lightColors[i] / attenuation, 1.0);
    }

    // Add fog
    float fogDistance = abs(cameraSpacePosition.z / cameraSpacePosition.w);
    color = mix(baseColor, fogColor, getFogFactor(fogDistance));
}

vec3 getSpecularColor(vec3 lightColor, vec3 rayDirection)
{
    vec3 bouncingRayDirection = normalize(reflect(rayDirection, normal));
    vec3 cameraDirection = normalize(cameraPosition - worldSpacePosition.xyz);
    float specularFactor = dot(cameraDirection, bouncingRayDirection);

    if (specularFactor > 0) {
        specularFactor = pow(specularFactor, materialSpecularPower);
        return lightColor * materialSpecularIntensity * specularFactor;
    } else {
        return vec3(0.0, 0.0, 0.0);
    }
}

float getVisibility()
{
    float cosTheta = clamp(dot(normal, sunLightDirection), 0.0, 1.0);
    float bias = 0.005 * tan(acos(cosTheta));
    bias = clamp(bias, 0.0, 0.01);
    float visibility = 1;
    for (int i = 0; i < 4; i++) {
        if (texture(shadowMap, shadowCoord.xy + poissonDisk[i] / 700.0).z  <  shadowCoord.z - bias) {
            visibility -= 0.2;
        }
    }
    return visibility;
}

float getFogFactor(float fogDistance)
{
    float factor;

    switch (fogEquation) {
    case 0:
        factor = (fogEnd - fogDistance) / (fogEnd - fogStart);
        break;
    case 1:
        factor = exp(-fogDensity * fogDistance);
        break;
    case 2:
        factor = exp(-pow(fogDensity * fogDistance, 2.0));
        break;
    default:
        factor = 0.0;
    }
    factor = 1.0 - clamp(factor, 0.0, 1.0);

    return factor;
}

