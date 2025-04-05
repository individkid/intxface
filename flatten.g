#version 450

#if defined(fragmentFlatten)
layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

void fragmentFlatten() {
    outColor = texture(texSampler, fragTexCoord);
}
#endif

#if defined(fragmentDebug)
layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

void fragmentDebug() {
    outColor = texture(texSampler, fragTexCoord);
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
    gl_Position = inMat.buf[2] * inMat.buf[1] * inMat.buf[0] * vec4(inPosition.xyz, 1.0);
    fragColor = inOrdClr.xyz;
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

void vertexDebug() {
    gl_Position = inMat.buf[2] * inMat.buf[1] * inMat.buf[0] * vec4(inPosition.xyz, 1.0);
    fragColor = inOrdClr.xyz;
    fragTexCoord = inOrdClr.xy;
}
#endif
