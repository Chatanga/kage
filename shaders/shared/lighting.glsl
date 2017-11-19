// uniform vec3 cameraPosition;

uniform vec3 sunLightColor;
uniform vec3 sunLightDirection;
uniform float sunLightAmbientIntensity;

uniform int lightCount;
uniform vec3 lightPositions[100];
uniform vec3 lightColors[100];

uniform vec4 fogColor;
uniform float fogStart; // This is only for linear fog
uniform float fogEnd; // This is only for linear fog
uniform float fogDensity; // For exp and exp2 equation
uniform int fogEquation; // 0 = linear, 1 = exp, 2 = exp2

uniform int shadowUsed;
uniform mat4 shadow;
uniform sampler2D shadowMap;

const vec2 SMALL_POISSON_DISK[4] = vec2[](
    vec2( -0.94201624, -0.39906216 ),
    vec2( 0.94558609, -0.76890725 ),
    vec2( -0.094184101, -0.92938870 ),
    vec2( 0.34495938, 0.29387760 )
);

const vec2 POISSON_DISK[16] = vec2[](
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

// -----------------------------------------------------------------------------

vec3 getSpecularColor(vec4 theWorldSpacePosition, vec3 normal, float specularIntensity, float specularPower, vec3 lightColor, vec3 rayDirection)
{
    vec3 bouncingRayDirection = normalize(reflect(rayDirection, normal));
    vec3 cameraDirection = normalize(cameraPosition - theWorldSpacePosition.xyz);
    float specularFactor = dot(cameraDirection, bouncingRayDirection);

    if (specularFactor > 0) {
        specularFactor = pow(specularFactor, specularPower);
        return lightColor * specularIntensity * specularFactor;
    } else {
        return vec3(0.0, 0.0, 0.0);
    }
}

// -----------------------------------------------------------------------------

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

vec4 applyFog(vec4 color, float fogDistance)
{
    return mix(color, fogColor, getFogFactor(fogDistance));
}

// -----------------------------------------------------------------------------

float getVisibility1(vec3 normal, vec4 shadowCoord)
{
    float cosTheta = clamp(dot(normal, sunLightDirection), 0.0, 1.0);
    float bias = 0.005 * tan(acos(cosTheta));
    bias = clamp(bias, 0.0, 0.01);

    if (texture(shadowMap, shadowCoord.xy).r < shadowCoord.z - bias) {
        return 0.0;
    }
    else {
        return 1.0;
    }
}

float getVisibility2(vec3 normal, vec4 shadowCoord)
{
    float cosTheta = clamp(dot(normal, sunLightDirection), 0.0, 1.0);
    float bias = 0.005 * tan(acos(cosTheta));
    bias = clamp(bias, 0.0, 0.01);

    float visibility = 1;
    for (int i = 0; i < 4; i++) {
        if (texture(shadowMap, shadowCoord.xy + SMALL_POISSON_DISK[i] / 700.0).r < shadowCoord.z - bias) {
            visibility -= 0.2;
        }
    }
    return max(0, visibility);
}

float getVisibility3(vec3 normal, vec4 shadowCoord)
{
    float cosTheta = clamp(dot(normal, sunLightDirection), 0.0, 1.0);
    float bias = 0.005 * tan(acos(cosTheta));
    bias = clamp(bias, 0.0, 0.01);

    float visibility = 1;
    for (int i = 0; i < 16; i++) {
        float z = texture(shadowMap, shadowCoord.xy).r;
        if (z < shadowCoord.z - bias) {
            visibility -= 0.1 * (1.0 - z);
        }
    }
    return max(0, visibility);
}

// -----------------------------------------------------------------------------

vec4 getFragColor(
    float specularIntensity,
    float specularPower,
    vec4 baseColor,
    vec3 normal,
    vec4 theCameraSpacePosition,
    vec4 theWorldSpacePosition)
{
    // Base color.
    vec4 fragColor = baseColor;

    // Shadow.
    float visibility;
    if (shadowUsed == TRUE) {
        vec4 shadowCoord = shadow * theWorldSpacePosition;
        visibility = getVisibility3(normal, shadowCoord);
    } else {
        visibility = 1.0;
    }

    // Sunlight.
    float diffuseIntensity = max(0.0, dot(normal, normalize(-sunLightDirection)));
    vec3 specularColor = getSpecularColor(theWorldSpacePosition, normal, specularIntensity, specularPower, sunLightColor, sunLightDirection);
    fragColor *= vec4((sunLightColor * (sunLightAmbientIntensity + diffuseIntensity) + specularColor) * visibility, 1.0);

    // Light contributions.
    for (int i = 0; i < min(10, lightCount); ++i) {
        float lightDistance = length(theWorldSpacePosition - vec4(lightPositions[i], 1.0));
        float attenuation = 0.01 + 0.07 * lightDistance + 0.00008 * lightDistance * lightDistance;
        fragColor += vec4(lightColors[i] / attenuation, 1.0);
    }

    // Add fog.
    float fogDistance = length(theCameraSpacePosition);
    fragColor = applyFog(fragColor, fogDistance);

    /*
    if (shadowUsed == TRUE) {
        vec4 shadowCoord = shadow * theWorldSpacePosition;
        float gap = (texture(shadowMap, shadowCoord.xy).z - shadowCoord.z - 0.005) * -1.0;
        float i = gap;
        fragColor = vec4(i, i, i, 1.0);
    }
    */

    return fragColor;
}

