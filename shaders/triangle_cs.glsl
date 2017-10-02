#version 400 core

layout (vertices = 3) out;

void main(void)
{
    // Only if I am invocation 0...
    if (gl_InvocationID == 0)
    {
        gl_TessLevelInner[0] = 5.0;

        gl_TessLevelOuter[0] = 5.0;
        gl_TessLevelOuter[1] = 5.0;
        gl_TessLevelOuter[2] = 5.0;
    }
    // Everybody copies their input to their output.
    gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;
}
