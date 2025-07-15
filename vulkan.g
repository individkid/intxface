#version 450

#if defined(fragmentDisplay) || defined(fragmentCosplay) || defined(fragmentCopoint) || defined(fragmentCoplane) || defined(fragmentPierce) || defined(fragmentDepth)
layout (location = 0) in vec4 fragOrd;
layout (location = 1) flat in uint fragIdx;
layout (location = 2) in vec4 fragVec;
layout (location = 3) flat in uvec4 fragRef;
#endif
#if defined(fragmentDisplay)
layout (location = 0) out vec4 outColor;
void fragmentDisplay() {
    outColor = fragOrd;
}
#endif
#if defined(fragmentCosplay)
layout (location = 0) out vec4 outColor;
void fragmentCosplay() {
    outColor = fragOrd;
}
#endif
#if defined(fragmentCopoint)
struct Numeric {
    vec4 vec; // distances above basis
    uvec4 bas; // basis selector
};
layout (binding = 4) writeonly restrict buffer Numerics {
    Numeric buf[];
} outNum;
void fragmentCopoint() {
    outNum.buf[fragIdx].vec = fragVec;
    outNum.buf[fragIdx].bas = fragRef;
}
#endif
#if defined(fragmentCoplane)
struct Vertex {
    vec4 vec; // intersection of planes
    vec4 ord; // coordinate or color
    uvec4 ref; // backreference to planes
};
layout (binding = 5) writeonly restrict buffer Vertexs {
    Vertex buf[];
} outVer;
void fragmentCoplane() {
    outVer.buf[fragIdx].vec = fragVec;
    outVer.buf[fragIdx].ord = fragOrd;
    outVer.buf[fragIdx].ref = fragRef;
}
#endif
#if defined(fragmentPierce)
layout (location = 0) out uint outColor;
void fragmentPierce() {
    outColor = fragIdx;
}
#endif
#if defined(fragmentDepth)
layout (location = 0) out float outColor;
void fragmentDepth() {
    outColor = gl_FragCoord.z;
}
#endif

#if defined(fragmentTest)
layout (binding = 1) uniform sampler2D texSampler;

layout (location = 0) in vec3 fragColor;
layout (location = 1) in vec2 fragTexCoord;

layout (location = 0) out vec4 outColor;

void fragmentTest() {
    outColor = texture(texSampler, fragTexCoord); // vec4(fragColor,1.0);
}
#endif

#if defined(fragmentDebug)
layout (binding = 1) uniform sampler2D texSampler;

layout (location = 0) in vec3 fragColor;
layout (location = 1) in vec2 fragTexCoord;
layout (location = 2) in vec4 test;

layout (location = 0) out float outColor;

void fragmentDebug() {
    outColor = test.z;
}
#endif

#if defined(vertexDisplay) || defined(vertexCosplay) || defined(vertexCopoint) || defined(vertexCoplane) || defined(vertexPierce) || defined(vertexDepth)
struct Uniform {
    uint all; // which subject to use
    uint one; // which element to use
    uint idx; // which plane to manipulate
    uint use; // which basis to use
    uint tri; // base of triangles
    uint num; // base of numerics
    uint vtx; // base of vertices
    uint mat; // base of matrices
};
struct Matrix {
    mat4 buf;
};
struct Basis {
    float buf[48];
};
struct Triangle {
    uvec4 vtx; // points of triangle
    uint num; // plane of points
    uint pol; // polytope triangle is in
    uint tex; // texture selector
    uint rot; // texture rotation
};
struct Numeric {
    vec4 vec; // distances above basis
    uvec4 bas; // basis selector
};
struct Vertex {
    vec4 vec; // intersection of planes
    vec4 ord; // coordinate or color
    uvec4 ref; // backreference to planes
};
layout (binding = 0) uniform Uniforms {
    Uniform buf;
} inUni;
layout (binding = 1) buffer Matrixs {
    Matrix buf[];
} inMat;
layout (binding = 2) buffer Basiss {
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
layout (location = 0) out vec4 fragOrd;
layout (location = 1) out uint fragIdx;
layout (location = 2) out vec4 fragVec;
layout (location = 3) out uvec4 fragRef;
void index(out uint tri, out uint cnr, out uint vtx, out uint pol, out uint num, out uint all, out uint idx, out uint one)
{
    tri = gl_VertexIndex/3-inUni.buf.tri;
    cnr = gl_VertexIndex%3;
    vtx = inTri.buf[tri].vtx[cnr]-inUni.buf.vtx;
    pol = inTri.buf[tri].pol-inUni.buf.mat;
    num = inTri.buf[tri].num-inUni.buf.num;
    all = inUni.buf.all-inUni.buf.mat;
    idx = inUni.buf.idx-inUni.buf.num;
    one = inUni.buf.one-inUni.buf.mat;
}
void display(uint tri, uint idx, uint num, uint one, uint pol, uint all, uint vtx, vec4 vec)
{
    if (idx == num) gl_Position = inMat.buf[one].buf * inMat.buf[pol].buf * inMat.buf[all].buf * vec;
    else gl_Position = inMat.buf[pol].buf * inMat.buf[all].buf * vec;
    fragOrd = inVer.buf[vtx].ord;
    fragIdx = tri;
}
vec4 intersect(Numeric num0, Numeric num1, Numeric num2)
{
    return vec4(0.0,0.0,0.0,1.0);
}
#endif
#if defined(vertexDisplay)
void vertexDisplay() {
    uint tri,cnr,vtx,pol,num,all,idx,one;
    index(tri,cnr,vtx,pol,num,all,idx,one);
    vec4 vec = inVer.buf[vtx].vec;
    display(tri,idx,num,one,pol,all,vtx,vec);
}
#endif
#if defined(vertexCosplay)
void vertexCosplay() {
    uint tri,cnr,vtx,pol,num,all,idx,one;
    index(tri,cnr,vtx,pol,num,all,idx,one);
    Numeric num[3]; for (int i = 0; i < 3; i++)
    num[i] = inNum.buf[inVer.buf[vtx].ref[i]];
    vec4 vec = intersect(num[0],num[1],num[2]);
    display(tri,idx,num,one,pol,all,vtx,vec);
}
#endif

#if defined(vertexTest)
layout (binding = 0) uniform Matrix {mat4 buf[];} inMat;

layout (location = 0) in vec4 inPosition;
layout (location = 1) in vec4 inOrdClr;
layout (location = 2) in uvec4 inRefer;

layout (location = 0) out vec3 fragColor;
layout (location = 1) out vec2 fragTexCoord;

void vertexTest() {
    if (gl_VertexIndex >= 4) gl_Position = inMat.buf[2] * inPosition;
    else gl_Position = inMat.buf[2] * inMat.buf[3] * inPosition;
    if (gl_VertexIndex >= 4) fragColor = vec3(0.0,0.0,1.0); // blue
    else fragColor = vec3(1.0,0.0,0.0); // red
    fragTexCoord = inOrdClr.xy;
}
#endif

#if defined(vertexDebug)
layout (binding = 0) uniform Matrix {mat4 buf[];} inMat;

layout (location = 0) in vec4 inPosition;
layout (location = 1) in vec4 inOrdClr;
layout (location = 2) in uvec4 inRefer;

layout (location = 0) out vec3 fragColor;
layout (location = 1) out vec2 fragTexCoord;
layout (location = 2) out vec4 test;

void vertexDebug() {
    gl_Position = inMat.buf[2] * inPosition;
    fragColor = inOrdClr.xyz;
    fragTexCoord = inOrdClr.xy;
    test = inMat.buf[3] * vec4(-0.5,-0.5,0.2,1.0);
}
#endif
