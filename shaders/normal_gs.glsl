#version 400 core

uniform mat4 projection;
uniform mat4 camera;
uniform mat4 transformation;

layout(points) in;
layout(line_strip, max_vertices = 2) out;

in vec3 normal[];

void main()
{
    mat4 mvp = projection * camera * transformation;

    vec3 position = gl_in[0].gl_Position.xyz;
    gl_Position = mvp * vec4(position, 1.0);
    EmitVertex();

    vec3 normal = vec4(normal[0], 0.0).xyz;
    gl_Position = mvp * vec4(position + normal, 1.0);
    EmitVertex();

    EndPrimitive();
}
