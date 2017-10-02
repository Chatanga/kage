#version 400 core

layout(points) in;
layout(triangle_strip) out;
layout(max_vertices = 12) out;

uniform mat4 shadow;

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

uniform float timePassed;

out vec2 texCoord;
out vec4 worldSpacePosition;
out vec4 shadowCoord;
out vec4 cameraSpacePosition;
flat out int textureIndex;

mat4 rotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat4(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,  0.0,
                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,  0.0,
                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c,           0.0,
                0.0,                                0.0,                                0.0,                                1.0);
}

vec3 localSeed;

// This function returns random number from zero to one
float randZeroOne()
{
    uint n = floatBitsToUint(localSeed.y * 214013.0 + localSeed.x * 2531011.0 + localSeed.z * 141251.0);
    n = n * (n * n * 15731u + 789221u);
    n = (n >> 9u) | 0x3F800000u;

    float res =  2.0 - uintBitsToFloat(n);
    localSeed = vec3(localSeed.x + 147158.0 * res, localSeed.y * res  + 415161.0 * res, localSeed.z + 324154.0 * res);
    return res;
}

int randomInt(int min, int max)
{
    return int(float(min) + randZeroOne() * float(max - min));
}

void emitPoint(mat4 mvp, vec3 point, float u, float v)
{
    worldSpacePosition = transformation * vec4(point, 1.0);
    shadowCoord = shadow * worldSpacePosition;
    cameraSpacePosition = camera * worldSpacePosition;
    gl_Position = projection * cameraSpacePosition;
    texCoord = vec2(u, v);
    EmitVertex();
}

void main()
{
    vec3 grassFieldPos = gl_in[0].gl_Position.xyz;

    if (grassFieldPos.z < 5 || grassFieldPos.z > 15) {
        EndPrimitive();
        return;
    }

    mat4 mvp = projection * camera * transformation;

    float PIover180 = 3.1415/180.0;
    vec3 baseDir[3];
    baseDir[0] = vec3(1.0, 0.0, 0.0);
    baseDir[1] = vec3(float(cos(60.0 * PIover180)), float(sin(60.0 * PIover180)), 0.0f);
    baseDir[2] = vec3(float(cos(-60.0 * PIover180)), float(sin(-60.0 * PIover180)), 0.0f);

    float grassPatchSize = 4.0;
    float windStrength = 3;

    vec3 windDirection = normalize(vec3(1.0, 1.0, 0.0));

    for (int i = 0; i < 3; i++)
    {
        localSeed = grassFieldPos * float(i);

        int grassPatchIndex = randomInt(0, 2);
        textureIndex = grassPatchIndex;
        float fTCStartX = 0; // float(grassPatchIndex) * 0.25f;
        float fTCEndX = 1; // fTCStartX + 0.25f;

        float windPower = 0.5f + sin(grassFieldPos.x/30 + grassFieldPos.y/30 + timePassed*(1.2f+windStrength/20.0f));
        if (windPower < 0.0f) {
            windPower = windPower * 0.2f;
        }
        else {
            windPower = windPower * 0.3f;
        }
        windPower *= windStrength;

        vec3 baseDirRotated = (rotationMatrix(vec3(0, 1, 0), sin(timePassed * 0.7f) * 0.1f) * vec4(baseDir[i], 1.0)).xyz;
        float grassPatchHeight = 2 + randZeroOne() * 1;

        // Grass patch top left vertex
        vec3 topLeft = grassFieldPos - baseDirRotated*grassPatchSize*0.5f + windDirection*windPower;
        topLeft.z += grassPatchHeight;
        emitPoint(mvp, topLeft, fTCStartX, 0.0);

        // Grass patch bottom left vertex
        vec3 bottomLeft = grassFieldPos - baseDir[i]*grassPatchSize*0.5f;
        emitPoint(mvp, bottomLeft, fTCStartX, 1.0);

        // Grass patch top right vertex
        vec3 topRight = grassFieldPos + baseDirRotated*grassPatchSize*0.5f + windDirection*windPower;
        topRight.z += grassPatchHeight;
        emitPoint(mvp, topRight, fTCEndX, 0.0);

        // Grass patch bottom right vertex
        vec3 bottomRight = grassFieldPos + baseDir[i]*grassPatchSize*0.5f;
        emitPoint(mvp, bottomRight, fTCEndX, 1.0);

        EndPrimitive();
    }
}
