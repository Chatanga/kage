#version 400 core

uniform sampler2D shadowMap;

uniform sampler2D samplers[3];

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

in vec2 texCoord;
in vec3 normal;
in vec4 worldSpacePosition;
in vec4 shadowCoord;
in vec4 cameraSpacePosition;

out vec4 color;

float getVisibility();
float getVisibility2();
float getFogFactor(float fogDistance);

vec2 poissonDisk[16] = vec2[]( 
   vec2( -0.94201624, -0.39906216 ), 
   vec2( 0.94558609, -0.76890725 ), 
   vec2( -0.094184101, -0.92938870 ), 
   vec2( 0.34495938, 0.29387760 ), 
   vec2( -0.91588581, 0.45771432 ), 
   vec2( -0.81544232, -0.87912464 ), 
   vec2( -0.38277543, 0.27676845 ), 
   vec2( 0.97484398, 0.75648379 ), 
   vec2( 0.44323325, -0.97511554 ), 
   vec2( 0.53742981, -0.47373420 ), 
   vec2( -0.26496911, -0.41893023 ), 
   vec2( 0.79197514, 0.19090188 ), 
   vec2( -0.24188840, 0.99706507 ), 
   vec2( -0.81409955, 0.91437590 ), 
   vec2( 0.19984126, 0.78641367 ), 
   vec2( 0.14383161, -0.14100790 ) 
);

void main()
{
    // Need to be done outside the conditions bellow to avoid mipmapping stitching... But why?
    vec4 texColor0 = texture(samplers[0], texCoord);
    vec4 texColor1 = texture(samplers[1], texCoord);
    vec4 texColor2 = texture(samplers[2], texCoord);

    vec4 texColor;
    float height = clamp(worldSpacePosition.z / 20, 0.0, 1.0);
    if (height < 0.2) {
        texColor = texColor0;
    } else if (height < 0.4) {
        float r = (height - 0.2) / 0.2;
        texColor = mix(texColor0, texColor1, r);
    } else if (height < 0.6) {
        texColor = texColor1;
    } else if (height < 0.8) {
        float r = (height - 0.6) / 0.2;
        texColor = mix(texColor1, texColor2, r);
    } else {
        texColor = texColor2;
    }

    float diffuseIntensity = max(0.0, dot(normalize(normal), -sunLightDirection)) * getVisibility2();
    vec4 baseColor = texColor * vec4(sunLightColor * (sunLightAmbientIntensity + diffuseIntensity), 1.0);

    // Light contributions
    for (int i = 0; i < min(10, lightCount); ++i) {
        float distance = length(worldSpacePosition - vec4(lightPositions[i], 1.0));
        float attenuation = 0.01 + 0.07 * distance + 0.00008 * distance * distance;
        baseColor += vec4(lightColors[i] / attenuation, 1.0);
    }

    // Add fog
    float fogDistance = length(cameraSpacePosition); // abs(cameraSpacePosition.z / cameraSpacePosition.w);
    color = mix(baseColor, fogColor, getFogFactor(fogDistance));
}

float getVisibility()
{
    // if (texture(shadowMap, shadowCoord.xy).z  <  shadowCoord.z - 0.005) {
    if (texture(shadowMap, shadowCoord.xy).z  <  shadowCoord.z) {
        return 0.0;
    }
    else {
        return 1.0; // 0.5;
    }
}

float getVisibility2()
{
    float cosTheta = clamp(dot(normal, sunLightDirection), 0.0, 1.0);
    float bias = 0.005 * tan(acos(cosTheta));
    bias = clamp(bias, 0.0, 0.01);
    float visibility = 1;
    for (int i = 0; i < 16; i++) {
        if (texture(shadowMap, shadowCoord.xy + poissonDisk[i] / 700.0).z  <  shadowCoord.z - bias) {
            vec4 shadowSmooth =
                vec4(
                    shadowCoord.x + poissonDisk[i].x / 700.0,
                    shadowCoord.y + poissonDisk[i].y / 700.0,
                    (shadowCoord.z - bias) / shadowCoord.w, 1.0);
            float fSub = texture(shadowMap, shadowSmooth.st).r; 
            visibility -= 0.1*(1.0-fSub);
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
