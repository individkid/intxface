#version 450

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
    uint mod; // fill constant
    uint wid; // width of image
    uint hei; // height of image
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
/*enum Decor { // fragment shader graph operation
    MoveDec, // move only
    DotDec, // dot product
    CrossDec, // cross product
    PlusDec, // numwise sum
    MinusDec, // numwise difference
    TimesDec, // numwise product
    OverDec, // numwise quotient
    PlaneDec, // construct plane
    AboveDec, // jump condition
    WriteDec, // write output
};
enum Swizzle { // fragment shader operand selector
    PointSwz, // comparison point
    PlaneSwz, // constructed plane
    Corner0Swz, // construct point
    Corner1Swz, // construct point
    Conrer2Swz, // construct point
    BufferSwz, // located feedback
    ColorSwz, // color suggestion
    CoordSwz, // fragment location
    IndexSwz, // primitive index
    TextureSwz, // texture color
    RelateSwz, // closeby indices
    NormalSwz, // surface normal
    UniformSwz, // uniform value
};*/
struct Decorate {
    /*Decor*/uint dec; // operation
    uint nxt; // next graph node unless compare fail
    uint dim; // always 4
    /*Swizzle*/uint src[4]; // which input or buffer array
    uint idx[4]; // which input or buffer element
    /*Swizzle*/uint acc; // which accumulator
};

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

// vertexFill/Const/Fetch/Vertex/Coplane fragment/Relate/Pierce/Color

#if defined(fragmentRelate) || defined(fragmentPierce) || defined(fragmentColor)
layout (location = 0) in vec4 fragOrd;
layout (location = 1) flat in vec4 fragVec;
layout (location = 2) flat in uvec4 fragRef;
layout (location = 3) flat in uint fragIdx;
layout (location = 4) flat in uint fragTex;
layout (location = 5) in vec3 fragColor;
layout (location = 6) in vec2 fragTexCoord;
#endif
#if defined(vertexFill) || defined(vertexConst) || defined(vertexFetch) || defined(vertexVertex) || defined(vertexCoplane)
layout (location = 0) out vec4 fragOrd; // interpolated vertex
layout (location = 1) out vec4 fragVec; // calculated normal
layout (location = 2) out uvec4 fragRef; // facet identifiers
layout (location = 3) out uint fragIdx; // primitive identifier
layout (location = 4) out uint fragTex; // decoration selector
layout (location = 5) out vec3 fragColor; // interpolated color
layout (location = 6) out vec2 fragTexCoord; // primitive space
#endif

layout (binding = 0) readonly uniform Uniforms {
    Uniform buf;
} inUni;

#if defined(vertexConst) || defined(vertexFetch) || defined(vertexVertex) || defined(vertexCoplane)
layout (binding = 1) readonly uniform Matrixs {
    Matrix buf[4]; // uniforms cannot be variable size
} inMat;
#endif

#if defined(vertexVertex) || defined(vertexCoplane)
layout (binding = 3) readonly buffer Triangles {
    Triangle buf[];
} inTri;
layout (binding = 5) readonly buffer Vertexs {
    Vertex buf[];
} inVer;
#endif

#if defined(vertexCoplane)
layout (binding = 2) readonly buffer Basiss {
    Basis buf[];
} inBas;
layout (binding = 4) readonly buffer Numerics {
    Numeric buf[];
} inNum;
#endif

#if defined(vertexVertex) || defined(vertexCoplane)
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
#endif

#if defined(vertexFill) || defined(vertexConst)
    const vec4 vertices[] = {
        {-0.5f, -0.5f, 0.20f, 1.0f},
        {0.5f, -0.5f, 0.40f, 1.0f},
        {0.5f, 0.5f, 0.60f, 1.0f},
        {-0.5f, 0.5f, 0.40f, 1.0f},
        
        {-0.5f, -0.5f, 0.50f, 1.0f},
        {0.5f, -0.5f, 0.50f, 1.0f},
        {0.5f, 0.5f, 0.50f, 1.0f},
        {-0.5f, 0.5f, 0.50f, 1.0f},
    };
    const vec4 coord[] = {
        {1.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 1.0f, 0.0f, 0.0f},
        {1.0f, 1.0f, 0.0f, 0.0f},
        //
        {1.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 1.0f, 0.0f, 0.0f},
        {1.0f, 1.0f, 0.0f, 0.0f},
        //
    };
    const uint indices[] = {
        0, 1, 2, 2, 3, 0,
        4, 5, 6, 6, 7, 4,
    };
    const vec4 extremes[] = {
        {-2.0f, -2.0f, 0.50f, 1.0f},
        {2.0f, -2.0f, 0.50f, 1.0f},
        {2.0f, 2.0f, 0.50f, 1.0f},
        {-2.0f, 2.0f, 0.50f, 1.0f},
    };
#endif

#if defined(vertexConst) || defined(vertexFetch)
// TODO figure out way to identify polytope from fetch data, and use it to decide which matrix(s) to use
void share(vec4 myPosition, vec4 myCoordinate, uvec4 myColor, uint lim)
{
    if (gl_VertexIndex >= lim) gl_Position = inMat.buf[2].buf * myPosition;
    else gl_Position = inMat.buf[2].buf * inMat.buf[3].buf * myPosition;
    fragTexCoord = myCoordinate.xy;
    if (gl_VertexIndex >= lim) fragColor = vec3(0.0,0.0,1.0); // blue
    else fragColor = vec3(1.0,0.0,0.0); // red
    fragIdx = (gl_VertexIndex >= lim ? 1 : 0);
}
#endif

#if defined(vertexFetch)
layout (location = 0) in vec4 inPosition;
layout (location = 1) in vec4 inOrdClr;
layout (location = 2) in uvec4 inRefer;
void fetch()
{
    share(inPosition,inOrdClr,inRefer,4);
}
#endif

#if defined(vertexConst)
void backoff()
{
    share(vertices[indices[gl_VertexIndex]],coord[indices[gl_VertexIndex]],uvec4(0,0,0,0),6);
}
#endif

#if defined(vertexVertex)
void vertex()
{
    uint tri,cnr,vtx,pol,num,tex,all,idx,one,use;
    index(tri,cnr,vtx,pol,num,tex,all,idx,one,use);
    vec4 vec = inVer.buf[vtx].vec;
    display(tri,idx,num,tex,one,pol,all,vtx,vec);
}
#endif

#if defined(vertexCoplane)
void expand(out vec4 res[3], uint ref, uint use)
{
    uint bas = inNum.buf[ref].bas[0]; // which direction is above
    vec4 vec = inNum.buf[ref].vec; // distances above feet
    for (uint i = 0; i < 3; i++) { // per foot
    res[i] = inBas.buf[use].buf[bas*3+i];
    res[i][bas] += vec[i];}
}
void coplane()
{
    uint tri,cnr,vtx,pol,num,tex,all,idx,one,use;
    index(tri,cnr,vtx,pol,num,tex,all,idx,one,use);
    vec4 exp[3/*plane*/][3/*tangent*/];
    for (uint i = 0; i < 3; i++)
    expand(exp[i],inVer.buf[vtx].ref[i],use);
    vec4 vec = intersect(exp[0],exp[1],exp[2]);
    display(tri,idx,num,tex,one,pol,all,vtx,vec);
}
#endif

#if defined(vertexFill)
void fullscreen()
{
    gl_Position = extremes[indices[gl_VertexIndex]];
}
#endif

#if defined(vertexConst)
void vertexConst()
{
    backoff();
}
#endif
#if defined(vertexFill)
void vertexFill()
{
    fullscreen();
}
#endif
#if defined(vertexCoplane)
void vertexCoplane()
{
    coplane();
}
#endif
#if defined(vertexVertex)
void vertexVertex()
{
    vertex();
}
#endif
#if defined(vertexFetch)
void vertexFetch()
{
    fetch();
}
#endif

#if defined(fragmentColor)
layout (binding = 6) readonly buffer Relates {
    uint buf[];
} inRel;
layout (binding = 7) uniform sampler2D texSampler;
layout (binding = 8) readonly buffer Decorates {
    Decorate buf[];
} inDec;
layout (location = 0) out vec4 outColor;
void fragmentColor()
{
    if (fragIdx == 1) outColor = vec4(fragColor,1.0);
    else outColor = texture(texSampler, fragTexCoord);
}
#endif
#if defined(fragmentPierce)
layout (location = 0) out float outColor;
void fragmentPierce()
{
    outColor = gl_FragCoord.z;
}
#endif
#if defined(fragmentRelate)
layout (location = 0) out uint outColor;
void fragmentRelate()
{
    outColor = inUni.buf.mod;
}
#endif
