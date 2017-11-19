#version 430 core

layout (local_size_x = 32, local_size_y = 32) in;
layout (binding = 0, r8) uniform image2D img_input;
layout (binding = 1, r8) uniform image2D img_output;

void main(void)
{
    /*
    uvec3 gl_LocalInvocationID : index of the shader invocation within the local work group
    uvec3 gl_WorkGroupSize : local work group size
    uvec3 gl_NumWorkGroups : number of work groups
    uvec3 gl_WorkGroupID : the index of the current work group within the global set

    uvec3 gl_GlobalInvocationID = gl_WorkGroupID * gl_WorkGroupSize + gl_LocalInvocationID
    uint gl_LocalInvocationIndex =
        gl_LocalInvocationID.z * gl_WorkGroupSize.x * gl_WorkGroupSize.y +
        gl_LocalInvocationID.y * gl_WorkGroupSize.x +
        gl_LocalInvocationID.x
    */
    vec4 texel;
    ivec2 p = ivec2(gl_GlobalInvocationID.xy);
    texel = imageLoad(img_input, p);
    texel = vec4(1.0) - texel;
    imageStore(img_output, p, texel);
}
