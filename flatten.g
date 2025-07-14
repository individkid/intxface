#version 450

#if defined(fragmentCode)
void fragmentCode() {
}
#endif

#if defined(fragmentTest)
layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

void fragmentTest() {
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

#if defined(vertexCode)
struct Uniform {
    int all; // which subject to use
    int one; // which element to use
    int pro; // which projection to use
    int win; // which window to use
    int lon; // horizontal axis of cursor
    int lat; // vertical axis of cursor
    int idx; // which plane to manipulate
    int use; // which basis to use
    int tri; // base of triangles
    int num; // base of numerics
    int vtx; // base of vertices
    int mat; // base of matrices
};
struct Matrix {
    mat4 buf;
};
struct Basis {
    mat3 buf[3];
};
struct Triangle {
    ivec4 vtx; // points of triangle
    int num; // plane of points
    int pol; // polytope triangle is in
    int tex; // texture selector
    int rot; // texture rotation
};
struct Numeric {
    vec4 vec; // distances above basis
    ivec4 bas; // basis selector
};
struct Vertex {
    vec4 vec; // intersection of planes
    vec4 ord; // coordinate or color
    ivec4 ref; // backreference to planes
};
layout (binding = 0) uniform Uniforms {
    Uniform buf[];
} inUni;
layout (binding = 1) uniform Matrixs {
    Matrix buf[];
} inMat;
layout (binding = 2) uniform Basiss {
    Basis buf[];
} inBas;
layout (binding = 3) buffer Triangles {
    Triangle buf[];
} inTri;
layout (binding = 4) buffer Numerics {
    Numeric buf[];
} inNum;
layout (binding = 5) buffer Vertexs {
    Vertex buf[];
} inVer;
void vertexCode() {
}
#endif

#if defined(vertexTest)
layout(binding = 0) uniform Matrix {mat4 buf[];} inMat;

layout(location = 0) in vec4 inPosition;
layout(location = 1) in vec4 inOrdClr;
layout(location = 2) in uvec4 inRefer;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void vertexTest() {
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
