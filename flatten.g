#version 450

#if defined(fragmentFlatten)
layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

void fragmentFlatten() {
    outColor = texture(texSampler, fragTexCoord); // vec4(fragColor,1.0);
}
#endif

#if defined(fragmentDebug)
layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) in vec4 test;

layout(location = 0) out float outColor;

void fragmentDebug() {
    outColor = test.z;
}
#endif

#if defined(vertexFlatten)
layout(binding = 0) uniform Matrix {mat4 buf[];} inMat;

layout(location = 0) in vec4 inPosition;
layout(location = 1) in vec4 inOrdClr;
layout(location = 2) in uvec4 inRefer;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void vertexFlatten() {
    if (gl_VertexIndex >= 4) gl_Position = inMat.buf[2] * inPosition;
    else gl_Position = inMat.buf[2] * inMat.buf[3] * inPosition;
    if (gl_VertexIndex >= 4) fragColor = vec3(0.0,0.0,1.0); // blue
    else fragColor = vec3(1.0,0.0,0.0); // red
    fragTexCoord = inOrdClr.xy;
}
#endif

#if defined(vertexDebug)
layout(binding = 0) uniform Matrix {mat4 buf[];} inMat;

layout(location = 0) in vec4 inPosition;
layout(location = 1) in vec4 inOrdClr;
layout(location = 2) in uvec4 inRefer;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;
layout(location = 2) out vec4 test;

void vertexDebug() {
    gl_Position = inMat.buf[2] * inPosition;
    fragColor = inOrdClr.xyz;
    fragTexCoord = inOrdClr.xy;
    test = inMat.buf[3] * vec4(-0.5,-0.5,0.2,1.0);
}
#endif
