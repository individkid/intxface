#version 450

#ifdef vertexPractice
layout(binding = 0) uniform Replica {
    mat4 model;
    mat4 view;
    mat4 proj;
} uni;

layout(binding = 1) buffer readonly Matrix {mat4 mat[];} mat;

// Use storage buffer and gl_VertexIndex instead of vertex binding.
layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in uint inIndex;

layout(location = 0) out vec3 fragColor;

void vertexPractice() {
    // TODO change to mat.mat[uni.pro] * mat.mat[uni.all] * mat.mat[tri.tri[gl_VertexID/3].pol] * vec4(vtx[tri.tri[gl_VertexID/3].vtx[gl_VertexID%3]].vtx,1.0)
    gl_Position = uni.proj * uni.view * mat.mat[0] * vec4(inPosition, 0.0, 1.0);
    fragColor = inColor;
}
#endif

#ifdef fragmentPractice
layout(location = 0) in vec3 fragColor;

layout(location = 0) out vec4 outColor;

void fragmentPractice() {
    outColor = vec4(fragColor, 1.0);
}
#endif
