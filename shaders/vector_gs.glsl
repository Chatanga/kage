#version 440 core

const int N = 8;

layout(points) in;
layout(triangle_strip) out;
layout(max_vertices = N * 6 * 3) out;

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

// varying?
out vec4 worldSpacePosition;
out vec4 cameraSpacePosition;
out vec3 normal;

#include "shared/constants.glsl"

mat3 orientationMatrix(vec3 axis)
{
    vec3 axis1 = normalize(axis);
    vec3 axis2 = vec3(1.0, 0.0, 0.0);
    if (dot(axis1, axis2) > 0.75) {
        axis2 = vec3(0.0, 1.0, 0.0);
    }
    vec3 axis3 = normalize(cross(axis1, axis2));
    axis2 = cross(axis3, axis1);

    mat3 m = mat3(  axis2.x,    axis3.x,    axis1.x,
                    axis2.y,    axis3.y,    axis1.y,
                    axis2.z,    axis3.z,    axis1.z);
    m = transpose(m);
    return m;
}

void emitPoint(mat3 orientation, vec3 p, vec3 n)
{
    worldSpacePosition = transformation * vec4(orientation * p, 1.0);
    cameraSpacePosition = camera * worldSpacePosition;
    gl_Position = projection * cameraSpacePosition;
    normal = (projection * camera * transformation * vec4(orientation * n, 0.0)).xyz;
    EmitVertex();
}

void main()
{
    vec3 vector = gl_in[0].gl_Position.xyz;

    float l = length(vector);
    if (l > 0){ // Hein ?
        mat3 m = orientationMatrix(vector);

        float l1 = l * 7/8;
        float r2 = 0.1;
        float r1 = 0.05;

        vec2 n1 = vec2(1, 0);
        for (int i = 0; i < N; i++)
        {
            float a = (i + 1) * 2 * PI / N;
            vec2 n2 = vec2(cos(a), sin(a));

            vec3 p1 = vec3(r1 * n1, 0);
            vec3 p2 = vec3(r1 * n2, 0);

            emitPoint(m, vec3(0, 0, 0), vec3(0, 0, 1));
            emitPoint(m, p1, vec3(0, 0, 1));
            emitPoint(m, p2, vec3(0, 0, 1));
            EndPrimitive();

            vec3 p3 = vec3(p1.xy, l1);
            vec3 p4 = vec3(p2.xy, l1);

            emitPoint(m, p1, vec3(n1, 0));
            emitPoint(m, p3, vec3(n1, 0));
            emitPoint(m, p4, vec3(n2, 0));
            EndPrimitive();

            emitPoint(m, p1, vec3(n1, 0));
            emitPoint(m, p4, vec3(n2, 0));
            emitPoint(m, p2, vec3(n2, 0));
            EndPrimitive();

            vec3 p5 = vec3(r2 * n1, l1);
            vec3 p6 = vec3(r2 * n2, l1);

            emitPoint(m, p3, vec3(0, 0, -1));
            emitPoint(m, p5, vec3(0, 0, -1));
            emitPoint(m, p6, vec3(0, 0, -1));
            EndPrimitive();

            emitPoint(m, p3, vec3(0, 0, -1));
            emitPoint(m, p6, vec3(0, 0, -1));
            emitPoint(m, p4, vec3(0, 0, -1));
            EndPrimitive();

            emitPoint(m, vec3(0, 0, l), vec3(0, 0, 1));
            emitPoint(m, p6, vec3(n2, 0));
            emitPoint(m, p5, vec3(n1, 0));
            EndPrimitive();

            n1 = n2;
        }
    }
}

