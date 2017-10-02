#version 400 core

layout(location = 0) out float fragColour;

void main() {
    fragColour = gl_FragCoord.z;
}
