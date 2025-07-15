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
    vec4 buf[9];
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
void index(out uint tri, out uint cnr, out uint vtx, out uint pol, out uint num, out uint all, out uint idx, out uint one, out uint use)
{
    tri = gl_VertexIndex/3-inUni.buf.tri;
    cnr = gl_VertexIndex%3;
    vtx = inTri.buf[tri].vtx[cnr]-inUni.buf.vtx;
    pol = inTri.buf[tri].pol-inUni.buf.mat;
    num = inTri.buf[tri].num-inUni.buf.num;
    all = inUni.buf.all-inUni.buf.mat;
    idx = inUni.buf.idx-inUni.buf.num;
    one = inUni.buf.one-inUni.buf.mat;
    use = inUni.buf.use;
}
void display(uint tri, uint idx, uint num, uint one, uint pol, uint all, uint vtx, vec4 vec)
{
    if (idx == num) gl_Position = inMat.buf[one].buf * inMat.buf[pol].buf * inMat.buf[all].buf * vec;
    else gl_Position = inMat.buf[pol].buf * inMat.buf[all].buf * vec;
    fragOrd = inVer.buf[vtx].ord;
    fragIdx = tri;
}
vec4 cross(out float acu, vec4 vec0, vec4 vec1)
{
    // acu is magnitude of cross over product of leg magnitudes
    vec3 lft = vec0.xyz; vec3 rgt = vec1.xyz;
    vec3 vec = cross(lft,rgt);
    vec4 res = vec4(vec,1.0);
    acu = length(res)/(length(vec0)*length(vec1));
    return res;
}
vec4 proj(out float acu, vec4 vec0, vec4 vec1, vec4 num[3])
{
    // ratio of dot of segment with cross onto segment
    // acu is cross acu times projection magnitude squared over segment magnitude squared
    acu = 0.0; vec4 res; for (int i = 0; i < 3; i++) {
    float tmp; vec4 vec = cross(tmp,num[(i+1)%3],num[(i+2)%3]);
    vec4 dif0 = vec0-num[i]; vec4 dif1 = vec1-num[i];
    float num = dot(vec,dif0); float den = dot(vec,dif1);
    tmp = tmp * (num+den)/length(vec);
    if (tmp > acu) {acu = tmp; res = (vec0*num+vec1*den)/(num+den);}}
    return res;
}
vec4 sect(out float acu, vec4 num0[3], vec4 num1[3], vec4 num2[3])
{
    // ratio of project of segment of num0/num1 onto num2
    // acu is product of projection acus
    return vec4(0.0,0.0,0.0,1.0);
}
vec4 intersect(vec4 num0[3], vec4 num1[3], vec4 num2[3])
{
    // return sect with best acu
    return vec4(0.0,0.0,0.0,1.0);
}
void expand(out vec4 res[3], uint vtx, uint ref, uint use)
{
    uint bas = inNum.buf[inVer.buf[vtx].ref[ref]].bas[0];
    vec4 vec = inNum.buf[inVer.buf[vtx].ref[ref]].vec;
    for (int i = 0; i < 3; i++) res[i] = inBas.buf[use].buf[bas*3+i]*vec[i];
}
#endif
#if defined(vertexDisplay)
void vertexDisplay() {
    uint tri,cnr,vtx,pol,num,all,idx,one,use;
    index(tri,cnr,vtx,pol,num,all,idx,one,use);
    vec4 vec = inVer.buf[vtx].vec;
    display(tri,idx,num,one,pol,all,vtx,vec);
}
#endif
#if defined(vertexCosplay)
void vertexCosplay() {
    uint tri,cnr,vtx,pol,num,all,idx,one,use;
    index(tri,cnr,vtx,pol,num,all,idx,one,use);
    vec4 num[3/*plane*/][3/*tangent*/];
    for (int i = 0; i < 3; i++) expand(num[i],vtx,i,use);
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
