#version 400 core

uniform sampler2D sampler;

in vec2 texCoord;

// varying vec3 position;

void main()
{
    vec4 baseColor = vec4(0, 0, 0, 1);

    vec2 size = textureSize(sampler, 0);
    float e = gl_FragCoord.z / 10; // Crude

    float distAlphaMask = texture(sampler, texCoord).r;

    bool OUTLINE = false;
    vec4 OUTLINE_COLOR = vec4(1, 1, 1, 1);
    float OUTLINE_MIN_VALUE0 = 0.40;
    float OUTLINE_MIN_VALUE1 = 0.45;
    float OUTLINE_MAX_VALUE0 = 0.50;
    float OUTLINE_MAX_VALUE1 = 0.55;
    if (OUTLINE && (distAlphaMask >= OUTLINE_MIN_VALUE0) && (distAlphaMask <= OUTLINE_MAX_VALUE1)) {
        float oFactor = 1.0;
        if (distAlphaMask <= OUTLINE_MIN_VALUE1) {
            oFactor = smoothstep(OUTLINE_MIN_VALUE0, OUTLINE_MIN_VALUE1, distAlphaMask);
        } else {
            oFactor = smoothstep(OUTLINE_MAX_VALUE1, OUTLINE_MAX_VALUE0, distAlphaMask);
        }
        baseColor = mix(baseColor, OUTLINE_COLOR, oFactor);
    }

    bool SOFT_EDGES = true;
    float SOFT_EDGE_MIN = 0.50 - e;
    float SOFT_EDGE_MAX = 0.50 + e;
    if (SOFT_EDGES) {
        baseColor.a *= smoothstep(SOFT_EDGE_MIN, SOFT_EDGE_MAX, distAlphaMask);
    } else {
        baseColor.a = distAlphaMask >= 0.5 ? 1 : 0;
    }

    bool OUTER_GLOW = false;
    vec2 GLOW_UV_OFFSET = vec2(0.0008, 0.0008);
    vec4 OUTER_GLOW_COLOR = vec4(0, 0, 0, 0.75);
    float OUTER_GLOW_MIN_DVALUE = 0.49;
    float OUTER_GLOW_MAX_DVALUE = 0.51;
    float mskUsed = 0.5;
    if (OUTER_GLOW) {
        float glowTexelMask = texture(sampler, texCoord + GLOW_UV_OFFSET).r;
        vec4 glowc = OUTER_GLOW_COLOR;
        float a = smoothstep(OUTER_GLOW_MIN_DVALUE, OUTER_GLOW_MAX_DVALUE, glowTexelMask);
        a = min(1 - baseColor.a, a);
        //glowc.a *= smoothstep(OUTER_GLOW_MIN_DVALUE, OUTER_GLOW_MAX_DVALUE, glowTexelMask);
        //baseColor = mix(glowc, baseColor, mskUsed);
        baseColor = glowc * a + baseColor * (1 - a);
    }

    gl_FragColor = baseColor;
}

