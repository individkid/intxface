#version 450

#ifdef vertexPractice
layout(binding = 0) uniform Matrix {mat4 mat[];} mat;

// Use storage buffer and gl_VertexIndex instead of vertex binding.
layout(location = 0) in vec2 inVec;
layout(location = 1) in uint inRef;

layout(location = 0) out vec3 fragColor;

void vertexPractice() {
    // TODO change to mat.mat[uni.pro] * mat.mat[uni.all] * mat.mat[tri.tri[gl_VertexID/3].pol] * vec4(vtx[tri.tri[gl_VertexID/3].vtx[gl_VertexID%3]].vtx,1.0)
    gl_Position = mat.mat[2] * mat.mat[1] * mat.mat[0] * vec4(inVec, 0.0, 1.0);
    switch (gl_VertexIndex%3) {
    case (0): fragColor = vec3(1.0,0.0,0.0); break;
    case (1): fragColor = vec3(0.0,1.0,0.0); break;
    case (2): fragColor = vec3(0.0,0.0,1.0); break;}
}
#endif

#ifdef fragmentPractice
layout(location = 0) in vec3 fragColor;

layout(location = 0) out vec4 outColor;

void fragmentPractice() {
    outColor = vec4(fragColor, 1.0);
}
#endif
