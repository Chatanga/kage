// http://madebyevan.com/shaders/grid/
// License: CC0 (http://creativecommons.org/publicdomain/zero/1.0/)

#version 400 core

uniform vec3 cameraPosition;

varying vec3 position;

#include "shared/constants.glsl"
#include "shared/lighting.glsl"

void main() {
    // Pick a coordinate to visualize in a grid
    vec2 coord = position.xy + cameraPosition.xy;

    // Compute anti-aliased world-space grid lines
    vec2 grid = abs(fract(coord - 0.5) - 0.5) / fwidth(coord);
    float line = min(grid.x, grid.y);

    // Just visualize the grid lines directly
    vec4 color = vec4(vec3(1.0 - min(line, 1.0)), 1.0 - min(line / 4.0, 1.0));

    // Add fog.
    float fogDistance = length(vec3(position.x, position.y, cameraPosition.z));
    gl_FragColor = applyFog(color, fogDistance * 10);
}
