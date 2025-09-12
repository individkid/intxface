#version 450

#if defined(fragmentDisplay) || defined(fragmentPierce) || defined(fragmentDepth) || defined(fragmentTest) || defined(fragmentDebug)
layout (location = 0) in vec4 fragOrd;
layout (location = 1) flat in uint fragIdx;
layout (location = 2) in vec4 fragVec;
layout (location = 3) flat in uvec4 fragRef;
layout (location = 4) flat in uint fragTex;
layout (location = 5) in vec3 fragColor;
layout (location = 6) in vec2 fragTexCoord;
layout (location = 7) in vec4 test;
/*ResrcBinding = {
    {"Resrc","RelateRes","Int","6"},
    {"Resrc","ImageRes","Int","7"},
}*/
layout (binding = 7) uniform sampler2D texSampler;
#endif
#if defined(fragmentDisplay)
layout (location = 0) out vec4 outColor;
void fragmentDisplay()
{
    switch (fragTex) {default:
    break; case (0): outColor = fragOrd;}
}
#endif
#if defined(fragmentPierce)
layout (location = 0) out uint outColor;
void fragmentPierce()
{
    outColor = fragIdx;
}
#endif
#if defined(fragmentDepth)
layout (location = 0) out float outColor;
void fragmentDepth()
{
    outColor = gl_FragCoord.z;
}
#endif
#if defined(fragmentTest)
// layout (binding = 1) buffer MyDataBuffer {
//     uint values[]; // Or a struct, array of structs, etc.
// } myData;
layout (location = 0) out vec4 outColor;
void fragmentTest()
{
    outColor = texture(texSampler, fragTexCoord); // vec4(fragColor,1.0);
}
#endif
#if defined(fragmentDebug)
layout (location = 0) out float outColor;
void fragmentDebug()
{
    outColor = test.z;
}
#endif

#if defined(vertexDisplay) || defined(vertexPierce) || defined(vertexDepth) || defined(vertexTest) || defined(vertexDebug)
// TODO use versors (which leg feet plane is constructed from) to decide which permutation to use
// TODO think of more approximate and efficient way to calculate acc-uracy
vec4 cross(out float acu, vec4 vec0, vec4 vec1)
{
    // acu is magnitude of cross over product of leg magnitudes
    vec3 lft = vec0.xyz; vec3 rgt = vec1.xyz;
    vec3 vec = cross(lft,rgt);
    vec4 res = vec4(vec,1.0);
    float num = dot(res,res);
    float den0 = dot(vec0,vec0);
    float den1 = dot(vec1,vec1);
    acu = num/max(max(den0,den1),1.0);
    return res;
}
vec4 proj(out float acu, vec4 vec0, vec4 vec1, vec4 num[3])
{
    // ratio of dot of segment with cross onto segment
    // acu is segment dot difference over cross length
    acu = 0.0; vec4 res; for (uint i = 0; i < 3; i++) {
    float tmp; vec4 vec = cross(tmp,num[(i+1)%3],num[(i+2)%3]);
    vec4 dif0 = vec0-num[i]; vec4 dif1 = vec1-num[i];
    float num = dot(vec,dif0); float den = dot(vec,dif1);
    // dot with cross is perpendicular distance from plane
    tmp = tmp * (num-den)*(num-den)/max(dot(vec,vec),1.0);
    if (tmp > acu) {acu = tmp;
    float ratio = num/(num-den);
    // by similar triangles, ratio between perpendicular distances
    // is same as ratio between distances to intersection
    res = vec0 + ratio*(vec1-vec0);}}
    return res;
}
vec4 sect(out float acu, vec4 num0[3], vec4 num1[3], vec4 num2[3])
{
    // ratio of project of segment of num0/num1 onto num2
    // acu is product of projection acu
    acu = 0.0; vec4 res; for (uint i = 0; i < 3; i++) {
    float tmp0; vec4 vec0 = proj(tmp0,num0[i],num0[(i+1)%3],num1);
    float tmp1; vec4 vec1 = proj(tmp1,num0[i],num0[(i+2)%3],num1);
    float tmp2; vec4 vec = proj(tmp2,vec0,vec1,num2);
    float tmp = tmp0*tmp1*tmp2;
    if (tmp > acu) {acu = tmp; res = vec;}}
    return res;
}
vec4 intersect(vec4 num0[3], vec4 num1[3], vec4 num2[3])
{
    // return sect with best acu
    float tmp0; vec4 vec0 = sect(tmp0,num0,num1,num2);
    float tmp1; vec4 vec1 = sect(tmp1,num1,num2,num0);
    float tmp; vec4 vec = sect(tmp,num2,num0,num1);
    float tmq0; vec4 uec0 = sect(tmq0,num1,num0,num2);
    float tmq1; vec4 uec1 = sect(tmq1,num2,num1,num0);
    float tmq; vec4 uec = sect(tmq,num0,num2,num1);
    if (tmp0 > tmp1 && tmp0 > tmp && tmp0 > tmq0 && tmp0 > tmq1 && tmp0 > tmq) return vec0;
    if (tmp1 > tmp0 && tmp1 > tmp && tmp1 > tmq0 && tmp1 > tmq1 && tmp1 > tmq) return vec1;
    if (tmp > tmp0 && tmp > tmp1 && tmp > tmq0 && tmp > tmq1 && tmp > tmq) return vec;
    if (tmq0 > tmq1 && tmq0 > tmq && tmq0 > tmp0 && tmq0 > tmp1 && tmq0 > tmp) return uec0;
    if (tmq1 > tmq0 && tmq1 > tmq && tmq1 > tmp0 && tmq1 > tmp1 && tmq1 > tmp) return uec1;
    /*if (tmq > tmq0 && tmq > tmq1 && tmq > tmp0 && tmq > tmp1 && tmq > tmp)*/ return uec;
}
struct Uniform {
    uint all; // which subject to use
    uint one; // which element to use
    uint idx; // which plane to manipulate
    uint use; // which basis to use

    uint tri; // base of triangles
    uint num; // base of numerics
    uint vtx; // base of vertices
    uint mat; // base of matrices

    uint bas; // base of basises
    uint mod; // vertices or planes
    uint pad[2];
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

/*ResrcBinding = {
    {"Resrc","UniformRes","Int","0"},
    {"Resrc","MatrixRes","Int","1"},
    {"Resrc","BasisRes","Int","2"},
    {"Resrc","TriangleRes","Int","3"},
    {"Resrc","NumericRes","Int","4"},
    {"Resrc","VertexRes","Int","5"},
}*/
layout (binding = 0) readonly uniform Uniforms {
    Uniform buf;
} inUni;
layout (binding = 1) readonly uniform Matrixs {
    Matrix buf[4]; // uniforms cannot be variable size
} inMat;
layout (binding = 2) readonly buffer Basiss {
    Basis buf[];
} inBas;
layout (binding = 3) readonly buffer Triangles {
    Triangle buf[];
} inTri;
layout (binding = 4) readonly buffer Numerics {
    Numeric buf[];
} inNum;
layout (binding = 5) readonly buffer Vertexs {
    Vertex buf[];
} inVer;

layout (location = 0) out vec4 fragOrd;
layout (location = 1) out uint fragIdx;
layout (location = 2) out vec4 fragVec;
layout (location = 3) out uvec4 fragRef;
layout (location = 4) out uint fragTex;
layout (location = 5) out vec3 fragColor;
layout (location = 6) out vec2 fragTexCoord;
layout (location = 7) out vec4 test;

layout (location = 0) in vec4 inPosition;
layout (location = 1) in vec4 inOrdClr;
layout (location = 2) in uvec4 inRefer;

void index(out uint tri, out uint cnr, out uint vtx, out uint pol, out uint num, out uint tex, out uint all, out uint idx, out uint one, out uint use)
{
    tri = gl_VertexIndex/3-inUni.buf.tri; // triangle index
    cnr = gl_VertexIndex%3; // corner index
    vtx = inTri.buf[tri].vtx[cnr]-inUni.buf.vtx; // vertex index for corner of triangle
    pol = inTri.buf[tri].pol-inUni.buf.mat; // matrix index for polytope vertex is in
    num = inTri.buf[tri].num-inUni.buf.num; // plane index for plane vertex is on
    tex = inTri.buf[tri].tex; // decoration type
    all = inUni.buf.all-inUni.buf.mat; // matrix index for everything
    idx = inUni.buf.idx-inUni.buf.num; // plane index for manipulated plane
    one = inUni.buf.one-inUni.buf.mat; // matrix index for manipulated plane
    use = inUni.buf.use; // basis index for plane feet
}
void display(uint tri, uint idx, uint num, uint tex, uint one, uint pol, uint all, uint vtx, vec4 vec)
{
    if (idx == num) gl_Position = inMat.buf[one].buf * inMat.buf[pol].buf * inMat.buf[all].buf * vec;
    else gl_Position = inMat.buf[pol].buf * inMat.buf[all].buf * vec;
    fragOrd = inVer.buf[vtx].ord;
    fragIdx = tri;
    fragTex = tex;
}
void expand(out vec4 res[3], uint ref, uint use)
{
    uint bas = inNum.buf[ref].bas[0];
    vec4 vec = inNum.buf[ref].vec;
    for (uint i = 0; i < 3; i++) {
    res[i] = inBas.buf[use].buf[bas*3+i];
    res[i][use] *= vec[i];}
}
void vertex()
{
    uint tri,cnr,vtx,pol,num,tex,all,idx,one,use;
    index(tri,cnr,vtx,pol,num,tex,all,idx,one,use);
    vec4 vec; if (inUni.buf.mod == 1) {
    vec4 num[3/*plane*/][3/*tangent*/];
    for (uint i = 0; i < 3; i++)
    expand(num[i],inVer.buf[vtx].ref[i],use);
    vec = intersect(num[0],num[1],num[2]);
    } else vec = inVer.buf[vtx].vec;
    display(tri,idx,num,tex,one,pol,all,vtx,vec);
}
#endif
#if defined(vertexDisplay)
void vertexDisplay()
{
    vertex();
}
#endif
#if defined(vertexPierce)
void vertexPierce()
{
    vertex();
}
#endif
#if defined(vertexDepth)
void vertexDepth()
{
    vertex();
}
#endif
#if defined(vertexTest)
void vertexTest()
{
    if (gl_VertexIndex >= 4) gl_Position = inMat.buf[2].buf * inPosition;
    else gl_Position = inMat.buf[2].buf * inMat.buf[3].buf * inPosition;
    if (gl_VertexIndex >= 4) fragColor = vec3(0.0,0.0,1.0); // blue
    else fragColor = vec3(1.0,0.0,0.0); // red
    fragTexCoord = inOrdClr.xy;
}
#endif
#if defined(vertexDebug)
void vertexDebug()
{
    gl_Position = inMat.buf[2].buf * inPosition;
    fragColor = inOrdClr.xyz;
    fragTexCoord = inOrdClr.xy;
    test = inMat.buf[3].buf * vec4(-0.5,-0.5,0.2,1.0);
}
#endif
