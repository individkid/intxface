#version 450

#if defined(vertexPractice) || defined(vertexCompute)
layout(binding = 0) uniform Matrix {mat4 buf[];} inMat;

// Use storage buffer and gl_VertexIndex instead of vertex binding.
layout(location = 0) in vec2 inVec;
layout(location = 1) in uint inRef;

layout(location = 0) out vec3 fragColor;

#if defined(vertexPractice)
void vertexPractice() {
#elif defined(vertexCompute)
void vertexCompute() {
#endif
    // TODO change to inMat.buf[uni.pro] * inMat.buf[uni.all] * inMat.buf[tri.tri[gl_VertexID/3].pol] * vec4(vtx[tri.tri[gl_VertexID/3].vtx[gl_VertexID%3]].vtx,1.0)
    gl_Position = inMat.buf[2] * inMat.buf[1] * inMat.buf[0] * vec4(inVec, 0.0, 1.0);
    switch (gl_VertexIndex%3) {
    case (0): fragColor = vec3(1.0,0.0,0.0); break;
    case (1): fragColor = vec3(0.0,1.0,0.0); break;
    case (2): fragColor = vec3(0.0,0.0,1.0); break;}
}
#endif

#if defined(fragmentPractice) || defined(fragmentCompute)
layout(location = 0) in vec3 fragColor;
layout(location = 0) out vec4 outColor;
#if defined(fragmentCompute)
layout(binding = 0) uniform Matrix {mat4 mat[];} mat;
struct Pierce {vec4 fix; vec4 nml; uint vld; uint idx; uint pol; uint pad;}
layout(binding = 1) buffer PierceBuffer {Pierce buf[]} outVec;
#endif

#if defined(fragmentPractice)
void fragmentPractice() {
#elif defined(fragmentCompute)
void fragmentCompute() {
#endif
    outColor = vec4(fragColor, 1.0);
}
#endif
