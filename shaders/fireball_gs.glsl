#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

layout(triangles) in;
layout(triangle_strip, max_vertices = 64) out;

out vec3 normal;

void divide2(vec3 p0, vec3 p1, vec3 p2);
void emitVertex(vec3 vertex);

void main() {
    divide2(gl_in[0].gl_Position.xyz, gl_in[1].gl_Position.xyz, gl_in[2].gl_Position.xyz);
}

/*
void divide(int depth, vec3 p0, vec3 p1, vec3 p2) {
    if (depth == 0) {
        emitVertex(p0);
        emitVertex(p1);
        emitVertex(p2);

        EndPrimitive();
    } else {
        vec3 p01 = (p0 + p1) / 2;
        vec3 p12 = (p1 + p2) / 2;
        vec3 p20 = (p2 + p0) / 2;

        //--depth;

        divide(depth - 1, p0, p01, p20);
        divide(depth - 1, p01, p1, p12);
        divide(depth - 1, p20, p12, p2);
        divide(depth - 1, p01, p12, p20);
    }
}
*/

void divide0(vec3 p0, vec3 p1, vec3 p2) {
    emitVertex(p0);
    emitVertex(p1);
    emitVertex(p2);
    EndPrimitive();
}

void divide1(vec3 p0, vec3 p1, vec3 p2) {
    vec3 p01 = (p0 + p1);
    vec3 p12 = (p1 + p2);
    vec3 p20 = (p2 + p0);

    divide0(p0, p01, p20);
    divide0(p01, p1, p12);
    divide0(p20, p12, p2);
    divide0(p01, p12, p20);
}

void divide2(vec3 p0, vec3 p1, vec3 p2) {
    vec3 p01 = (p0 + p1) / 2;
    vec3 p12 = (p1 + p2) / 2;
    vec3 p20 = (p2 + p0) / 2;

    divide1(p0, p01, p20);
    divide1(p01, p1, p12);
    divide1(p20, p12, p2);
    divide1(p01, p12, p20);
}

void emitVertex(vec3 vertex) {
    gl_Position = projection * camera * transformation * vec4(normalize(vertex), 1.0);
    normal = normalize(transformation * vec4(vertex, 0.0)).xyz;
    EmitVertex();
}
