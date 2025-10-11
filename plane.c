#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "stlx.h"
#include "type.h"
#include "fmtx.h"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/select.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <errno.h>

struct Center **center = 0; // only for planeSwitch
int centers = 0; // only for planeSwitch
int external = 0; // pipe to planeSelect
int selwake = 0; // pipe to planeSelect
// TODO add openEvent to face.c to use eventfd for wake without wasted buffer
int console = 0; // pipe to planeConsole
int conwake = 0; // pipe to planeConsole
int timwake = 0; // pipe to planeTime
int cpywake = 0; // pipe to planeMachine
int tstwake = 0; // pipe to planeTest
struct Argument argument = {0}; // constant from commandline
void *internal = 0; // queue of center; protect with pipeSem
void *response = 0; // queue of center; protect with pipeSem
void *strin = 0; // queue of string; protect with stdioSem
void *strout = 0; // queue of string; protect with stdioSem
void *chrq = 0; // temporary queue to convert chars to str
void *ableq = 0; // map from thread to vector mask
void *timeq = 0; // queue of wakeup times
void *wakeq = 0; // queue of wakeup threads
int sub0 = 0; int idx0 = 0; void **dat0 = 0; // protect with dataSem
int sub1 = 0; int idx1 = 0; void **dat1 = 0; // protect with dataSem
void *copySem = 0;
void *pipeSem = 0;
void *stdioSem = 0;
void *timeSem = 0;
void *dataSem = 0;
void *evalSem = 0; int jknfo = 0;
uftype callCopy = 0;
nftype callBack = 0;
vftype callFork = 0;
zftype callInfo = 0;
zftype callJnfo = 0;
zftype callKnfo = 0;
oftype callCmnd = 0;
aftype callGlfw = 0;
float start = 0.0;
float times[Threads] = {0};

DECLARE_DEQUE(struct Center *,Centerq)
DECLARE_DEQUE(char *,Strq)
DECLARE_DEQUE(char, Chrq)
DECLARE_DEQUE(float, Timeq)
DECLARE_DEQUE(enum Thread, Wakeq)
DECLARE_DEQUE(int, Ableq)

int planeWots(int *ref, int val)
{
    int ret = *ref&val; *ref |= val; return ret;
}
int planeWotc(int *ref, int val)
{
    int ret = *ref&val; *ref &= ~val; return ret;
}
int planeWcfg(int *ref, int val)
{
    int ret = *ref; *ref = val; return ret;
}
int planeRcfg(int *ref, int val)
{
    return *ref;
}
int planeRmw(int *ref, int val)
{
    int ret = *ref; *ref = *ref + val; return ret; 
}
int planeRdwr(int *ref, int val)
{
    int ret = *ref; *ref = val; return ret;
}
int planeRaz(int *ref, int val)
{
    *ref = 0; return 0;
}

// cursor decoration
unsigned char *moveCursor(int dim, int e, int t, int r, int b, int l)
{
    int hot = dim/2;
    int box = 1;
    int siz = dim * dim * 4;
    unsigned char *pixels = malloc(siz);
    memset(pixels, 0x00, siz);
    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        // top and bottom
        if (k == 0 || k == dim-1) pixels[k*dim*4+j*4+i] = 0xff;
        // left and right
        if (j == 0 || j == dim-1) pixels[k*dim*4+j*4+i] = 0xff;
        // close box
        if (k == hot-(box+1) && j >= hot-box && j <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == hot-(box+1) && k >= hot-box && k <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        if (k == hot+(box+1) && j >= hot-box && j <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == hot+(box+1) && k >= hot-box && k <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        // open box
        if (e && k >= hot-box && k <= hot+box && j >= hot-box && j <= hot+box) pixels[k*dim*4+j*4+i] = 0xff;
        // cross marks
        if (t && k < hot-box && j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (r && j > hot+box && k == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (b && k > hot+box && j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (l && j < hot-box && k == hot) pixels[k*dim*4+j*4+i] = 0xff;}
    return pixels;
}
unsigned char *rotateCursor(int e)
{
    int dim = 11;
    int hot = dim/2;
    int siz = dim * dim * 4;
    unsigned char *pixels = malloc(siz);
    memset(pixels, 0x00, siz);
    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        int diffx = j-hot;
        int diffy = k-hot;
        int exact = hot*hot;
        int square = diffx*diffx + diffy*diffy;
        int center = k >= hot-1 && k <= hot+1 && j >= hot-1 && j <= hot+1;
        if (square < exact+5 && !center) pixels[k*dim*4+j*4+i] = 0xff;
        if (e && center) pixels[k*dim*4+j*4+i] = 0xff;}
    return pixels;
}
unsigned char *translateCursor(int e)
{
    int dim = 11;
    int hot = dim/2;
    int siz = dim * dim * 4;
    unsigned char *pixels = malloc(siz);
    memset(pixels, 0x00, siz);
    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        int diffx = (j>hot?j-hot:hot-j);
        int diffy = (k>hot?k-hot:hot-k);
        int sum = diffx + diffy;
        int center = k >= hot-1 && k <= hot+1 && j >= hot-1 && j <= hot+1;
        if (!center && sum < hot+1) pixels[k*dim*4+j*4+i] = 0xff;
        if (e && center) pixels[k*dim*4+j*4+i] = 0xff;}
    return pixels;
}
unsigned char *refineCursor()
{
    int dim = 11;
    int hot = dim/2;
    int siz = dim * dim * 4;
    unsigned char *pixels = malloc(siz);
    memset(pixels, 0x00, siz);
    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        int diffx = j-hot;
        int diffy = k-hot;
        if (diffx == diffy) pixels[k*dim*4+j*4+i] = 0xff;
        if (diffx == -diffy) pixels[k*dim*4+j*4+i] = 0xff;
        if (j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if (k == hot) pixels[k*dim*4+j*4+i] = 0xff;}
    return pixels;
}
unsigned char *sculptCursor(int e)
{
    int dim = 11;
    int hot = dim/2;
    int siz = dim * dim * 4;
    unsigned char *pixels = malloc(siz);
    memset(pixels, 0x00, siz);
    for (int k = 0; k < dim; k++) for (int j = 0; j < dim; j++) for (int i = 0; i < 4; i++) {
        int center = k >= hot-2 && k <= hot+2 && j >= hot-2 && j <= hot+2;
        if ((e || !center) && j == hot) pixels[k*dim*4+j*4+i] = 0xff;
        if ((e || !center) && k == hot) pixels[k*dim*4+j*4+i] = 0xff;}
    return pixels;
}

// matrix manipulation
float *vectorThree(float *vec, enum Configure left, enum Configure base, enum Configure deep)
{
    vec[0] = callInfo(left,0,planeRcfg);
    vec[1] = callInfo(base,0,planeRcfg);
    vec[2] = callInfo(deep,0,planeRcfg);
    return vec;
}
float *vectorTwo(float *vec, enum Configure left, enum Configure base)
{
    vec[0] = callInfo(left,0,planeRcfg);
    vec[1] = callInfo(base,0,planeRcfg);
    return vec;
}
void physicalToScreen(float *xptr, float *yptr)
{
    int width, height, xphys, yphys;
    width = callInfo(MonitorWidth,0,planeRcfg); height = callInfo(MonitorHeight,0,planeRcfg);
    xphys = callInfo(PhysicalWidth,0,planeRcfg); yphys = callInfo(PhysicalHeight,0,planeRcfg);
    *xptr *= width/xphys; *yptr *= height/yphys;
}
void physicalFromScreen(float *xptr, float *yptr)
{
    int width, height, xphys, yphys;
    width = callInfo(MonitorWidth,0,planeRcfg); height = callInfo(MonitorHeight,0,planeRcfg);
    xphys = callInfo(PhysicalWidth,0,planeRcfg); yphys = callInfo(PhysicalHeight,0,planeRcfg);
    *xptr *= xphys/width; *yptr *= yphys/height;
}
void screenToWindow(float *xptr, float *yptr)
{
    int width, height, left, base;
    width = callInfo(WindowWidth,0,planeRcfg); height = callInfo(WindowHeight,0,planeRcfg);
    left = callInfo(WindowLeft,0,planeRcfg); base = callInfo(WindowBase,0,planeRcfg);
    *xptr -= left; *yptr -= base; *xptr /= width; *yptr /= height;
}
void screenFromWindow(float *xptr, float *yptr)
{
    int width, height, left, base;
    width = callInfo(WindowWidth,0,planeRcfg); height = callInfo(WindowHeight,0,planeRcfg);
    left = callInfo(WindowLeft,0,planeRcfg); base = callInfo(WindowBase,0,planeRcfg);
    *xptr *= width; *yptr *= height; *xptr += left; *yptr += base;
}

// Transform functions find 4 independent vectors to invert, and 4 to multiply;
float *planeTransform(float *mat, float *src0, float *dst0, float *src1, float *dst1,
    float *src2, float *dst2, float *src3, float *dst3)
{
    float src[16]; float dst[16]; float inv[16];
    copyvec(src,src0,4); copyvec(src+4,src1,4); copyvec(src+8,src2,4); copyvec(src+12,src3,4);
    copyvec(dst,dst0,4); copyvec(dst+4,dst1,4); copyvec(dst+8,dst2,4); copyvec(dst+12,dst3,4);
    invmat(copymat(inv,src,4),4);
    return jumpmat(mat,timesmat(dst,inv,4),4);
}
float *planeSolve(float *mat, float *domain, float *range, int dim)
{
    float inv[dim*dim];
    if (invmat(copymat(inv,domain,dim),dim) == 0) {
    fprintf(stderr,"domain\n");
    for (int r = 0; r < dim; r++) {for (int c = 0; c < dim; c++) fprintf(stderr," %d",(int)*matrc(domain,r,c,dim)); fprintf(stderr,"\n");}
    fprintf(stderr,"range\n");
    for (int r = 0; r < dim; r++) {for (int c = 0; c < dim; c++) fprintf(stderr," %d",(int)*matrc(range,r,c,dim)); fprintf(stderr,"\n");}
    exit(-1);}
    return timesmat(copymat(mat,range,dim),inv,dim);
}
// Rotate functions find 2 fixed and 2 rotated, put all but 1 rotated in the 1.0 space,
// and put 1 rotated in the 0.0 space by subtracting one of the fixed.
float *planeRotateFocalMouse(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // tip by angle org fix cur; line through fix, perpendicular to plane containing org fix cur, is fixed.
    float fix0[4]; copyvec(fix0,fix,3); fix0[3] = 1.0;
    float neg0[4]; scalevec(copyvec(neg0,fix0,4),-1.0,4);
    float u[3]; copyvec(u,org,2); u[2] = -1.0; normvec(plusvec(copyvec(u,u,3),neg0,3),3);
    float v[3]; copyvec(v,cur,2); v[2] = -1.0; normvec(plusvec(copyvec(v,v,3),neg0,3),3);
    float w[3]; crossvec(copyvec(w,u,3),v);
    float fix1[4]; plusvec(copyvec(fix1,w,3),fix0,3); fix1[3] = 1.0;
    float src0[4]; plusvec(copyvec(src0,u,3),fix0,3); src0[3] = 1.0;
    float dst0[4]; plusvec(copyvec(dst0,v,3),fix0,3); dst0[3] = 1.0;
    float src1[4]; plusvec(crossvec(copyvec(src1,w,3),u),fix0,3); src1[3] = 1.0;
    float dst1[4]; plusvec(crossvec(copyvec(dst1,w,3),v),fix0,3); dst1[3] = 1.0;
    return planeTransform(mat,fix0,fix0,fix1,fix1,src0,dst0,plusvec(src1,neg0,4),plusvec(dst1,neg0,4));
}
float *planeRotateCursorRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // rotate by cur[2]-org[2] angle, keeping line from fix to cur fixed.
    float ang = cur[2]-org[2];
    float s0 = sin(ang), t0 = cos(ang);
    float s1 = -s0, t1 = t0; // exchange and negate to rotate 90 degrees
    float fix0[4]; copyvec(fix0,fix,3); fix0[3] = 1.0;
    float neg0[4]; scalevec(copyvec(neg0,fix0,4),-1.0,4);
    float fix1[4]; copyvec(fix1,cur,2); fix1[2] = -1.0; fix1[3] = 1.0;
    float i[3]; normvec(plusvec(copyvec(i,neg0,3),fix1,3),3);
    float j[3]; normvec(orthovec(anyvec(copyvec(j,i,3),3),i,3),3);
    float k[3]; crossvec(copyvec(k,i,3),j);
    float j0[3], k0[3]; scalevec(copyvec(j0,j,3),t0,3); scalevec(copyvec(k0,k,3),s0,3);
    float j1[3], k1[3]; scalevec(copyvec(j1,j,3),s1,3); scalevec(copyvec(k1,k,3),t1,3);
    float rot0[4]; plusvec(copyvec(rot0,j0,3),k0,3); rot0[3] = 1.0;
    float rot1[4]; plusvec(copyvec(rot1,j1,3),k1,3); rot1[3] = 1.0;
    float src0[4]; copyvec(src0,j,3); src0[3] = 1.0;
    float src1[4]; copyvec(src1,k,3); src1[3] = 1.0;
    return planeTransform(mat,fix0,fix0,fix1,fix1,src0,rot0,plusvec(src1,neg0,4),plusvec(rot1,neg0,4));
}
float *planeSlideOrthoMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
    float u[2]; scalevec(copyvec(u,org,2),-1.0,2);
    float v[2]; plusvec(copyvec(v,cur,2),u,2);
    float h0[4], h1[4]; unitvec(h0,3,0); h0[3] = 0.0; plusvec(copyvec(h1,h0,4),v,2);
    float i0[4], i1[4]; unitvec(i0,3,0); i0[3] = 1.0; plusvec(copyvec(i1,i0,4),v,2);
    float j0[4], j1[4]; unitvec(j0,3,1); j0[3] = 1.0; plusvec(copyvec(j1,j0,4),v,2);
    float k0[4], k1[4]; unitvec(k0,3,2); k0[3] = 1.0; plusvec(copyvec(k1,k0,4),v,2);
    return planeTransform(mat,h0,h1,i0,i1,j0,j1,k0,k1);
}
typedef float *(*planeXform)(float *mat, float *fix, float *nrm, float *org, float *cur);
float *planeMatrix(float *mat)
{
    planeXform fnc = 0; int tmp; int cfg = callInfo(ManipFixed,0,planeRcfg);
    tmp = ((1<<Slide)|(1<<Ortho)|(1<<Mouse)); if ((cfg&tmp)==tmp) fnc = planeSlideOrthoMouse;
    tmp = ((1<<Rotate)|(1<<Focal)|(1<<Mouse)); if ((cfg&tmp)==tmp) fnc = planeRotateFocalMouse;
    tmp = ((1<<Rotate)|(1<<Cursor)|(1<<Roller)); if ((cfg&tmp)==tmp) fnc = planeRotateCursorRoller;
    if (!fnc) return 0; float fix[3]; float nrm[3]; float org[2]; float cur[2];
    return fnc(identmat(mat,4),
    vectorThree(fix,FixedLeft,FixedBase,FixedDeep),
    vectorThree(nrm,NormalLeft,NormalBase,NormalDeep),
    vectorTwo(org,ClickLeft,ClickBase),
    vectorTwo(cur,ManipLeft,ManipBase));
}
float *planeWindow(float *mat)
{
    identmat(mat,4);
    return mat;
    // TODO
    float domain[16]; float range[16];
    float focal = callInfo(FocalLength,0,planeRcfg); float depth = callInfo(FocalDepth,0,planeRcfg);
    *matrc(domain,0,0,4) = callInfo(WindowLeft,0,planeRcfg);
    *matrc(domain,1,0,4) = callInfo(WindowBase,0,planeRcfg);
    *matrc(domain,2,0,4) = 0.0;
    *matrc(domain,3,0,4) = 1.0;
    *matrc(domain,0,1,4) = callInfo(WindowLeft,0,planeRcfg)+callInfo(WindowWidth,0,planeRcfg);
    *matrc(domain,1,1,4) = callInfo(WindowBase,0,planeRcfg)+callInfo(WindowHeight,0,planeRcfg);
    *matrc(domain,2,1,4) = 0.0;
    *matrc(domain,3,1,4) = 1.0;
    *matrc(domain,0,2,4) = callInfo(WindowLeft,0,planeRcfg);
    *matrc(domain,1,2,4) = callInfo(WindowBase,0,planeRcfg)+callInfo(WindowHeight,0,planeRcfg);
    *matrc(domain,2,2,4) = depth;
    *matrc(domain,3,2,4) = 1.0;
    *matrc(domain,0,3,4) = callInfo(WindowLeft,0,planeRcfg)+callInfo(WindowWidth,0,planeRcfg);
    *matrc(domain,1,3,4) = callInfo(WindowBase,0,planeRcfg);
    *matrc(domain,2,3,4) = depth;
    *matrc(domain,3,3,4) = 1.0;
    *matrc(range,0,0,4) = -1.0; *matrc(range,1,0,4) = -1.0; *matrc(range,2,0,4) = 0.0; *matrc(range,3,0,4) = 1.0;
    *matrc(range,0,1,4) = 1.0; *matrc(range,1,1,4) = 1.0; *matrc(range,2,1,4) = 0.0; *matrc(range,3,1,4) = 1.0;
    *matrc(range,0,2,4) = -1.0; *matrc(range,1,2,4) = 1.0; *matrc(range,2,2,4) = depth; *matrc(range,3,2,4) = (focal+depth)/focal;
    *matrc(range,0,3,4) = 1.0; *matrc(range,1,3,4) = -1.0; *matrc(range,2,3,4) = depth; *matrc(range,3,3,4) = (focal+depth)/focal;
    return planeSolve(mat,domain,range,4);
}
void planeDebug(float *debug)
{
    switch (callInfo(RegisterPlan,0,planeRcfg)) {
    default: ERROR();
    break; case (Bringup): {
    identmat(debug,4);
    float time = processTime();
    float src0[] = {-0.5f, -0.5f, 0.20f, 1.0f};
    float dst0[] = {-0.5f, -0.5f, 0.40f+0.20f*sinf(time*8.0f), 1.0f};
    float src1[] = {0.5f, -0.5f, 0.40f, 1.0f};
    float dst1[] = {0.5f, -0.5f, 0.40f, 1.0f};
    float src2[] = {0.5f, -0.5f, 0.40f, 0.0f};
    float dst2[] = {0.5f, -0.5f, 0.40f, 0.0f};
    float src3[] = {-0.5f, 0.5f, 0.40f, 1.0f};
    float dst3[] = {-0.5f, 0.5f, 0.40f, 1.0f};
    planeTransform(debug, src0, dst0, src1, dst1, src2, dst2, src3, dst3);}
    break; case (Builtin): {
    // TODO call machineIval or callInfo for which transformation or projection to use
    }}
}

// resource accessors
void centerSize(int idx)
{
    if (waitSafe(copySem) != 0) ERROR();
    if (idx < 0) ERROR();
    if (idx >= centers) {int size = idx+1; center = realloc(center,size*sizeof(struct Center *));
    for (int i = centers; i < size; i++) center[i] = 0; centers = size;}
    if (postSafe(copySem) != 1) ERROR();
}
// TODO add sanity check by allowing only a few centerPull without corresponsing centerPlace
struct Center *centerPull(int idx)
{
    centerSize(idx);
    if (waitSafe(copySem) != 0) ERROR();
    struct Center *ret = center[idx]; center[idx] = 0;
    if (postSafe(copySem) != 1) ERROR();
    return ret;
}
void centerPlace(struct Center *ptr, int idx)
{
    centerSize(idx);
    if (waitSafe(copySem) != 0) ERROR();
    freeCenter(center[idx]); allocCenter(&center[idx],0); center[idx] = ptr;
    if (postSafe(copySem) != 1) ERROR();
}
int centerCheck(int idx)
{
    centerSize(idx);
    if (waitSafe(copySem) != 0) ERROR();
    int ret = (center[idx] != 0);
    if (postSafe(copySem) != 1) ERROR();
    return ret;
}
int centerMod(struct Center *ptr)
{
    switch (ptr->mem) {default: ERROR();
    break; case (Indexz): return sizeof(int32_t);
    break; case (Bringupz): return sizeof(struct Vertex);
    break; case (Imagez): return sizeof(struct Image);
    break; case (Uniformz): return sizeof(struct Uniform);
    break; case (Matrixz): return sizeof(struct Matrix);
    break; case (Trianglez): return sizeof(struct Triangle);
    break; case (Numericz): return sizeof(struct Numeric);
    break; case (Vertexz): return sizeof(struct Vertex);
    break; case (Basisz): return sizeof(struct Basis);
    break; case (Peekz): return sizeof(struct Pierce);
    break; case (Pokez): return sizeof(struct Pierce);
    break; case (Drawz): return sizeof(struct Draw);
    break; case (Instrz): return sizeof(struct Ins);
    // Stringz
    break; case (Machinez): return sizeof(struct Machine);
    break; case (Expressz): return sizeof(struct Express);
    break; case (Kernelz): return sizeof(struct Kernel);}
    return 0;
}
void kernelClear(struct Kernel *ker)
{
    float mat[16];
    timesmat(timesmat(timesmat(timesmat(copymat(mat,ker->manip.mat,4),
        ker->pulse.mat,4),ker->self.mat,4),ker->other.mat,4),ker->comp.mat,4);
    copymat(ker->comp.mat,mat,4);
    identmat(ker->manip.mat,4); identmat(ker->pulse.mat,4);
    identmat(ker->self.mat,4); identmat(ker->other.mat,4);
}

// machine extensions
struct Center *machineCenter(int sig, int *arg, int lim, int idx, int sub)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    struct Center *srcPtr = centerPull(src);
    if (srcSub < 0 || srcSub >= srcPtr->siz) ERROR();
    return srcPtr;
}
struct Kernel *machineKernel(struct Center *ptr, int sig, int *arg, int lim, int idx, int sub)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    if (srcSub < 0 || srcSub >= ptr->siz) ERROR();
    if (ptr->mem != Kernelz) ERROR();
    return &ptr->ker[srcSub];
}
struct Matrix *machineMatrix(struct Center *ptr, int sig, int *arg, int lim, int idx, int sub)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    if (srcSub < 0 || srcSub >= ptr->siz) ERROR();
    if (ptr->mem != Matrixz) ERROR();
    return &ptr->mat[srcSub];
}
void machinePlace(struct Center *ptr, int sig, int *arg, int lim, int idx, int sub)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    if (srcSub < 0 || srcSub >= ptr->siz) ERROR();
    centerPlace(ptr,src);
}
void machineMerge(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,MergeArgs,MergeSrc,0);
    struct Center *dst = machineCenter(sig,arg,MergeArgs,MergeDst,0);
    if (src->mem != dst->mem) ERROR();
    // TODO merge memory from Src to Dst according to idx and siz
}
void machineClick(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,ClickArgs,ClickSrc,ClickSrcSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,ClickArgs,ClickSrc,ClickSrcSub);
    struct Center *dst = machineCenter(sig,arg,ClickArgs,ClickDst,ClickDstSub);
    struct Kernel *kernel = machineKernel(dst,sig,arg,ClickArgs,ClickDst,ClickDstSub);
    copymat(kernel->copy.mat,matrix->mat,4);
    machinePlace(dst,sig,arg,ClickArgs,ClickDst,ClickDstSub);
    machinePlace(src,sig,arg,ClickArgs,ClickSrc,ClickSrcSub);
}
void machineManip(int sig, int *arg)
{
    struct Center *center = machineCenter(sig,arg,ManipArgs,ManipDst,ManipDstSub);
    struct Kernel *kernel = machineKernel(center,sig,arg,ManipArgs,ManipDst,ManipDstSub);
    // manipulate manip
    float mat[16]; float inv[16]; planeMatrix(mat);
    jumpmat(kernel->manip.mat,jumpmat(invmat(copymat(inv,kernel->copy.mat,4),4),mat,4),4);
    copymat(kernel->copy.mat,mat,4);
    machinePlace(center,sig,arg,ManipArgs,ManipDst,ManipDstSub);
}
void machinePulse(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,PulseArgs,PulseSrc,PulseSrcSub);
    struct Kernel *kernel = machineKernel(src,sig,arg,PulseArgs,PulseSrc,PulseSrcSub);
    struct Center *dst = machineCenter(sig,arg,PulseArgs,PulseDst,PulseDstSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,PulseArgs,PulseDst,PulseDstSub);
    // clear manip to pulse
    jumpmat(kernel->pulse.mat,kernel->manip.mat,4); identmat(kernel->manip.mat,4);
    // compose pulse, self, other, comp to matrix
    timesmat(timesmat(timesmat(copymat(matrix->mat,kernel->pulse.mat,4),
        kernel->self.mat,4),kernel->other.mat,4),kernel->comp.mat,4);
    // record count to detect dropped bounce backs
    dst->slf = ++kernel->count; // TODO add this functionality to file.c
    machinePlace(dst,sig,arg,PulseArgs,PulseDst,PulseDstSub);
    machinePlace(src,sig,arg,PulseArgs,PulseSrc,PulseSrcSub);
}
void machineSelf(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
    struct Center *dst = machineCenter(sig,arg,SelfArgs,SelfDst,SelfDstSub);
    struct Kernel *kernel = machineKernel(dst,sig,arg,SelfArgs,SelfDst,SelfDstSub);
    if (dst->slf != kernel->count) {kernel->count = 0;
    callJnfo(RegisterMask,1<<SelfMsk,planeWots);}
    else {kernel->count--;
    // move portion of pulse to self to make matrix equal to self times other times comp
    // self1=matrix/(other*comp); pulse1=pulse0*self0/self1
    // pulse1*self1=pulse0*self0/self1*self1=pulse0*self0
    float self[16]; float pulse[16]; float inv[16]; float mat[16];
    timesmat(copymat(self,matrix->mat,4),
        invmat(timesmat(copymat(inv,kernel->other.mat,4),kernel->comp.mat,4),4),4);
    timesmat(timesmat(copymat(pulse,kernel->pulse.mat,4),
        kernel->self.mat,4),invmat(copymat(inv,self,4),4),4);
    copymat(kernel->pulse.mat,pulse,4); copymat(kernel->self.mat,self,4);
    // if count is zero, clear self and other to comp
    if (kernel->count == 0) kernelClear(kernel);}
    machinePlace(dst,sig,arg,SelfArgs,SelfDst,SelfDstSub);
    machinePlace(src,sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
}
void machineOther(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,OtherArgs,OtherSrc,OtherSrcSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,OtherArgs,OtherSrc,OtherSrcSub);
    struct Center *dst = machineCenter(sig,arg,OtherArgs,OtherDst,OtherDstSub);
    struct Kernel *kernel = machineKernel(dst,sig,arg,OtherArgs,OtherDst,OtherDstSub);
    if (dst->slf) {callJnfo(RegisterMask,1<<NslfMsk,planeWots);}
    // change other to make matrix equal to self times other times comp
    // matrix=self*other1*comp; other1=(1/self)*matrix/comp
    else {float other[16]; float self[16]; float inv[16];
    timesmat(timesmat(invmat(copymat(other,kernel->self.mat,4),4),
        matrix->mat,4),invmat(copymat(inv,kernel->comp.mat,4),4),4);
    copymat(kernel->other.mat,other,4);
    // if count is zero, clear self and other to comp
    if (kernel->count == 0) kernelClear(kernel);}
    machinePlace(dst,sig,arg,OtherArgs,OtherDst,OtherDstSub);
    machinePlace(src,sig,arg,OtherArgs,OtherSrc,OtherSrcSub);
}
void machineComp(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,CompArgs,CompSrc,CompSrcSub);
    struct Kernel *kernel = machineKernel(src,sig,arg,CompArgs,CompSrc,CompSrcSub);
    struct Center *dst = machineCenter(sig,arg,CompArgs,CompDst,CompDstSub);
    struct Matrix *matrix = machineMatrix(dst,sig,arg,CompArgs,CompDst,CompDstSub);
    // compose for draw
    timesmat(timesmat(timesmat(timesmat(copymat(matrix->mat,kernel->manip.mat,4),
        kernel->pulse.mat,4),kernel->self.mat,4),kernel->other.mat,4),kernel->comp.mat,4);
    machinePlace(dst,sig,arg,CompArgs,CompDst,CompDstSub);
    machinePlace(src,sig,arg,CompArgs,CompSrc,CompSrcSub);
}
void machineTest(int sig, int *arg)
{
    struct Center *center = machineCenter(sig,arg,TestArgs,TestDst,TestDstSub);
    struct Matrix *matrix = machineMatrix(center,sig,arg,TestArgs,TestDst,TestDstSub);
    float debug[16]; planeDebug(debug);
    copymat(matrix->mat,debug,4);
    machinePlace(center,sig,arg,TestArgs,TestDst,TestDstSub);
}
void machineBopy(int sig, int *arg)
{
    int count = arg[BopyCount];
    if (sig != BopyArgs) ERROR();
    int src = arg[BopySrc]; int srcSub = arg[BopySrcSub];
    int dst = arg[BopyDst]; int dstSub = arg[BopyDstSub];
    struct Center *srcPtr = centerPull(src);
    struct Center *dstPtr = centerPull(dst);
    if (srcSub < 0 || srcSub+count >= srcPtr->siz) ERROR();
    if (dstSub < 0 || dstSub+count >= dstPtr->siz) ERROR();
    if (srcPtr->mem != dstPtr->mem) ERROR();
    for (int i = 0; i < count; i++) switch (srcPtr->mem) {default: ERROR();
    case (Indexz): dstPtr->ind[dstSub+i] = srcPtr->ind[srcSub+i]; break;
    case (Trianglez): copyTriangle(&dstPtr->tri[dstSub+i],&srcPtr->tri[srcSub+i]); break;
    case (Numericz): copyNumeric(&dstPtr->num[dstSub+i],&srcPtr->num[srcSub+i]); break;
    case (Vertexz): copyVertex(&dstPtr->vtx[dstSub+i],&srcPtr->vtx[srcSub+i]); break;
    case (Basisz): copyBasis(&dstPtr->bas[dstSub+i],&srcPtr->bas[srcSub+i]); break;
    case (Matrixz): copyMatrix(&dstPtr->mat[dstSub+i],&srcPtr->mat[srcSub+i]); break;
    case (Imagez): copyImage(&dstPtr->img[dstSub+i],&srcPtr->img[srcSub+i]); break;
    // TODO Pokez Peekz
    case (Drawz): copyDraw(&dstPtr->drw[dstSub+i],&srcPtr->drw[srcSub+i]); break;
    case (Stringz): assignStr(&dstPtr->str[dstSub+i],srcPtr->str[srcSub+i]); break;
    case (Machinez): copyMachine(&dstPtr->mch[dstSub+i],&srcPtr->mch[srcSub+i]); break;
    case (Kernelz): copyKernel(&dstPtr->ker[dstSub+i],&srcPtr->ker[srcSub+i]); break;}
    machinePlace(srcPtr,sig,arg,BopyArgs,BopySrc,BopySrcSub);
    machinePlace(dstPtr,sig,arg,BopyArgs,BopyDst,BopyDstSub);
}
void planePass(struct Center *ptr, int sub);
void planeFail(struct Center *ptr, int sub);
void machineCopy(int sig, int *arg)
{
    if (sig != CopyArgs) ERROR();
    int src = arg[CopySrc];
    struct Center *ptr = centerPull(src);
    struct Fnc fnc = {0,planePass,0,planeFail,0};
    callCopy(ptr,src,fnc,0,0);
}
void machineDopy(int sig, int *arg)
{
    if (sig != DopyArgs) ERROR();
    int src = arg[DopySrc];
    int dst = arg[DopyDst];
    centerPlace(centerPull(src),dst);
}
void machinePopy(int sig, int *arg)
{
    if (sig != PopyArgs) ERROR();
    int dst = arg[PopyDst];
    if (waitSafe(pipeSem) != 0) ERROR();
    struct Center *ptr = maybeCenterq(0,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    centerPlace(ptr,dst);
}
void machineQopy(int sig, int *arg)
{
    if (sig != QopyArgs) ERROR();
    int src = arg[QopySrc];
    struct Center *ptr = centerPull(src);
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(ptr,response);    
    if (postSafe(pipeSem) != 1) ERROR();
}
void machineStage(enum Configure cfg, int idx)
{
    if (cfg != ArgumentInp && cfg != ArgumentOut) {
    struct Center *ptr = centerPull(idx);
    switch (cfg) {default: ERROR();
    case (CenterMem): callJnfo(cfg,ptr->mem,planeWcfg); break;
    case (CenterSiz): callJnfo(cfg,ptr->siz,planeWcfg); break;
    case (CenterIdx): callJnfo(cfg,ptr->idx,planeWcfg); break;
    case (CenterSlf): callJnfo(cfg,ptr->slf,planeWcfg); break;}
    centerPlace(ptr,idx);} else switch (cfg) {default: ERROR();
    case (ArgumentInp): callJnfo(cfg,argument.inp,planeWcfg); break;
    case (ArgumentOut): callJnfo(cfg,argument.out,planeWcfg); break;}
}
void machineTsage(enum Configure cfg, int idx)
{
    if (cfg != ArgumentInp && cfg != ArgumentOut) {
    struct Center *ptr = centerPull(idx);
    switch (cfg) {default: ERROR();
    case (CenterMem): ptr->mem = callInfo(cfg,0,planeRcfg); break; // TODO deallocat and zero out siz
    case (CenterSiz): ptr->siz = callInfo(cfg,0,planeRcfg); break; // TODO reallocate with new siz
    case (CenterIdx): ptr->idx = callInfo(cfg,0,planeRcfg); break;
    case (CenterSlf): ptr->slf = callInfo(cfg,0,planeRcfg); break;}
    centerPlace(ptr,idx);} else switch (cfg) {default: ERROR();
    case (ArgumentInp): argument.inp = callInfo(cfg,0,planeRcfg); break;
    case (ArgumentOut): argument.out = callInfo(cfg,0,planeRcfg); break;}
}
void machineEval(struct Express *exp, int idx)
{
    struct Center *ptr = centerPull(idx);
    if (!ptr) allocCenter(&ptr,1);
    int val = identType("Center");
    if (waitSafe(dataSem) != 0) ERROR();
    datxStr(dat0,""); writeCenter(ptr,sub1); datxInsert(*dat0,*dat1,val);
    if (postSafe(dataSem) != 1) ERROR();
    if (waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int val0 = datxEval(&dat,exp,val);
    if (postSafe(evalSem) != 1) ERROR();
    if (val0 != val) ERROR();
    if (waitSafe(dataSem) != 0) ERROR();
    assignDat(dat0,dat); free(dat); readCenter(ptr,sub0);
    if (postSafe(dataSem) != 1) ERROR();
    centerPlace(ptr,idx);
}
int machineIval(struct Express *exp)
{
    if (waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int typ = datxEval(&dat,exp,identType("Int"));
    if (postSafe(evalSem) != 1) ERROR();
    if (typ != identType("Int")) ERROR();
    if (waitSafe(dataSem) != 0) ERROR();
    assignDat(dat0,dat); free(dat); int val = readInt(idx0);
    if (postSafe(dataSem) != 1) ERROR();
    return val;
}
void machineVoid(struct Express *exp)
{
    if (waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int typ = datxEval(&dat,exp,-1); free(dat);
    int typ0 = identType("Dat"); if (typ == -1) typ = typ0; if (typ != typ0) ERROR();
    if (postSafe(evalSem) != 1) ERROR();
}
int machineEscape(struct Center *current, int level, int next)
{
    int inc = (level > 0 ? 1 : (level == 0 ? 0 : -1)); level *= inc;
    for (next += inc; level > 0; next += inc) {
    if (next < 0 || next >= current->siz) break;
    struct Machine *mptr = &current->mch[next];
    if (mptr->xfr == Nest) level += mptr->lvl*inc;}
    return next;
}
void machineArg(int *arg, int sig, struct Express *exp)
{
    for (int i = 0; i < sig; i++) arg[i] = machineIval(&exp[i]);
}
#define MACHINE(XFR) { \
    int arg[mptr->sig]; \
    machineArg(arg,mptr->sig,mptr->arg); \
    machine ## XFR(mptr->sig,arg);}
void machineSwitch(struct Machine *mptr)
{
    if (!mptr) ERROR();
    switch (mptr->xfr) {default: ERROR();
    case (Stage): for (int i = 0; i < mptr->siz; i++) machineStage(mptr->sav[i],machineIval(mptr->idx)); break;
    case (Tsage): for (int i = 0; i < mptr->siz; i++) machineTsage(mptr->sav[i],machineIval(mptr->idx)); break;
    case (Force): for (int i = 0; i < mptr->num; i++) callJnfo(mptr->cfg[i],machineIval(&mptr->val[i]),planeWcfg); break;
    case (Eval): machineEval(&mptr->fnc[0],machineIval(mptr->res)); break;
    case (Merge): MACHINE(Merge) break;
    case (Click): MACHINE(Click) break;
    case (Manip): MACHINE(Manip) break;
    case (Pulse): MACHINE(Pulse) break;
    case (Self): MACHINE(Self) break;
    case (Other): MACHINE(Other) break;
    case (Comp): MACHINE(Comp) break;
    case (Test): MACHINE(Test) break;
    case (Bopy): MACHINE(Bopy) break;
    case (Copy): MACHINE(Copy) break;
    case (Dopy): MACHINE(Dopy) break;
    case (Popy): MACHINE(Popy) break;
    case (Qopy): MACHINE(Qopy) break;}
}

// response callbacks
void planeCheck(struct Center *ptr, int sub) {
    if (ptr->mem != Peekz) ERROR();
    for (int i = 0; i < ptr->siz; i++)
    printf("check: %f %f\n",ptr->eek[i].val,processTime());
    centerPlace(ptr,sub);
}
void planePass(struct Center *ptr, int sub)
{
    if (sub >= 0) {
    centerPlace(ptr,sub);
    callJnfo(RegisterPass,sub,planeWcfg);
    callJnfo(RegisterMask,(1<<PassMsk),planeWots);}
}
void planeFail(struct Center *ptr, int sub)
{
    if (sub >= 0) {
    centerPlace(ptr,sub);
    callJnfo(RegisterFail,sub,planeWcfg);
    callJnfo(RegisterMask,(1<<FailMsk),planeWots);}
}
// a response that does not place center needs goon, and thus fnow
// also blocking not ok in ThreadState, since ThreadState wakes waiters after freeing resources
// blocking in main thread must use glfw to allow gpu queue, and thus ThreadState, to progress
void planeGlfw(struct Center *ptr, int sub) {
    callGlfw();
}
void planeSelf(struct Center *ptr, int sub) {
    if (sub >= 0) {
    if (--ptr->slf == 0) centerPlace(ptr,sub);
    callJnfo(RegisterPass,sub,planeWcfg);
    callJnfo(RegisterMask,(1<<PassMsk),planeWots);}
}
void planeWait(struct Center *ptr, int sub) {
    if (centerCheck(0) && waitRead(0.0,(1<<tstwake)) == tstwake) readInt(tstwake);
}
void planeForce(struct Center *ptr, int sub) {
    ERROR();
}
int planeGoon(struct Center *ptr, int sub) {
    return centerCheck(0);
}

// thread callbacks
void planeMachine(enum Thread tag, int idx)
{
    while (1) {
    int index = callInfo(MachineIndex,0,planeRcfg);
    if (index < 0) break;
    struct Center *current = centerPull(index);
    if (current->mem != Machinez) ERROR();
    int last = callInfo(MachineLast,0,planeRcfg)-1;
    for (int next = last+1; next != last; last = ++next) {
    if (next < 0 || next >= current->siz) ERROR();
    struct Machine *mptr = &current->mch[next];
    {char *opr = 0; showMachine(mptr,&opr);
    printf("%s\n",opr); free(opr);}
    switch (mptr->xfr) {default: machineSwitch(mptr); break;
    case (Jump): next = machineEscape(current,machineIval(&mptr->exp[0]),next) - 1; break;
    case (Goto): next = next + machineIval(&mptr->exp[0]) - 1; break;
    case (Nest): break;}}
    centerPlace(current,index);
    int sub = waitRead(0.0,(1<<cpywake));
    if (sub == cpywake && readInt(cpywake) < 0) break;}
}
void planeSelect(enum Thread tag, int idx)
{
    while (1) {
    int sub = waitRead(0.0,(1<<external)|(1<<selwake));
    if (sub == selwake) {
    if (readInt(selwake) < 0) break;
    if (waitSafe(pipeSem) != 0) ERROR();
    struct Center *center = maybeCenterq(0,response);
    if (postSafe(pipeSem) != 1) ERROR();
    if (center) {
    writeCenter(center,external); freeCenter(center); allocCenter(&center,0);}}
    else if (sub == external) {
    struct Center *center = 0;
    allocCenter(&center,1);
    readCenter(center,external);
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(center,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    callJnfo(RegisterMask,(1<<SlctMsk),planeWots);}
    else ERROR();}
}
void planeConsole(enum Thread tag, int idx)
{
    while (1) {
    int sub = waitRead(0.0,(1<<console)|(1<<conwake));
    if (sub == conwake) {
    if (readInt(conwake) < 0) break;
    if (waitSafe(stdioSem) != 0) ERROR();
    char *str = maybeStrq(0,strout);
    if (postSafe(stdioSem) != 1) ERROR();
    if (str) {writeStr(str,console); free(str);}}
    else if (sub == console) {
    char chr = readChr(console);
    pushChrq(chr,chrq);
    if (chr == '\n') {char *str = malloc(sizeChrq(chrq)+1); char *ptr = str;
    while (sizeChrq(chrq)) {*(ptr++) = frontChrq(chrq); popChrq(chrq);} *(ptr++) = 0;
    if (waitSafe(stdioSem) != 0) ERROR();
    pushStrq(str,strin);
    int size = sizeStrq(strin);
    if (postSafe(stdioSem) != 1) ERROR();
    callJnfo(RegisterMask,(1<<CnslMsk),planeWots);
    callJnfo(RegisterStrq,size,planeWcfg);}}
    else ERROR();}
}
void planeTime(enum Thread tag, int idx)
{
    // wait for smallest requested time, send interrupt first time it is exceeded
    float time = 0.0; // time requested
    int wake = 0; // expression to evaluate
    float delta = 0.0; // delay or 0.0 for forever
    int size = 0; // whether time is changed
    int init = 0; // whether time is valid
    while (1) {
    if (waitSafe(timeSem) != 0) ERROR();
    size = sizeTimeq(timeq);
    if (size != sizeWakeq(wakeq)) ERROR();
    if (!init && size != 0) {init = 1;
    time = frontTimeq(timeq); wake = frontWakeq(wakeq);
    dropTimeq(timeq); dropWakeq(wakeq);}
    if (postSafe(timeSem) != 1) ERROR();
    if (init) delta = time-(float)processTime(); // how long to wait
    else delta = 0.0; // wait forever
    if (init && (delta == 0.0 || delta <= 0.0 || delta < 0.0)) delta = -1.0; // wait not at all
    int sub = waitRead(delta,(1<<timwake));
    if (sub == timwake && readInt(timwake) < 0) break;
    if (init && (float)processTime() >= time) {init = 0;
    callJnfo(RegisterEval,wake,planeWcfg);
    callJnfo(RegisterMask,(1<<TimeMsk),planeWots);}}
}
void planeTest(enum Thread tag, int idx)
{
    int count = 0; float time = 0.0; int tested = 0; int pull = 0;
    struct Fnc fnc = {0,planePass,0,planeFail,0};
    struct Fnc fun = {0,planePass,planeWait,0,planeGoon};
    struct Fnc chk = {0,planeCheck,planeWait,0,planeGoon};
    int inds = 12;
    int arg[] = {0,1,2, 3,4,5, 6, 7,8,9, 10};
    int val[] = {
    /*DerIns ChainRes*//*req.idx*/0,/*req.siz*/inds,/*req.base*/MicroTest,
    /*DerIns DrawRes*//*req.idx*/0,/*req.siz*/inds,/*req.base*/MicroTest,
    /*IDeeIns PipeRes*//*ins.idx*/MicroTest,
    /*DerIns ChainRes*//*req.idx*/0,/*req.siz*/inds,/*req.base*/MicroTest,
    /*IDeeIns PipeRes*//*ins.idx*/MicroTest};
    if (sizeof(arg) != sizeof(val)) ERROR();
    int brg[] = {0,1,2, 3};
    int bal[] = {
    /*DerIns DrawRes*//*req.idx*/0,/*req.siz*/inds,/*req.base*/MicroDebug,
    /*IDeeIns PipeRes*//*ins.idx*/MicroDebug};
    if (sizeof(brg) != sizeof(bal)) ERROR();
    int crg[] = {0,1,2, 3};
    int cal[] = {
    /*DerIns DrawRes*//*req.idx*/0,/*req.siz*/inds,/*req.base*/MicroPierce,
    /*IDeeIns PipeRes*//*ins.idx*/MicroPierce};
    if (sizeof(crg) != sizeof(cal)) ERROR();
    while (1) {
    struct Center *tmp = centerPull(Matrixz);
    if (tmp && tmp->siz == 4) {
    centerPlace(tmp,Matrixz); break;}
    centerPlace(tmp,Matrixz);
    planeWait(0,0);}
    while (centerCheck(0)) {
    int save = pull+Memorys; struct Center *mat = centerPull(save); if (!mat) {planeWait(0,0); continue;}
    freeCenter(mat); pull = (pull+1)%4;
    mat->mem = Matrixz; mat->idx = 3; mat->siz = 1; allocMatrix(&mat->mat,mat->siz);
    float debug[16]; planeDebug(debug);
    memcpy(&mat->mat[0],debug,sizeof(struct Matrix));
    callCopy(mat,save,fnc,0,0);
    if (time == 0.0) time = processTime();
    if (processTime()-time > 0.1) {time = processTime(); count += 1;}
    if (count == tested) {/*int idx = 0;
    copy->push(MicroPierce,0,crg,sizeof(crg)/sizeof(int),idx,0,0,fun,SmartState());*/
    if (0) // TODO when PierceRes is done
    {int save = pull+Memorys; struct Center *drw = centerPull(save); if (!drw) {planeWait(0,0); continue;}
    freeCenter(drw); pull = (pull+1)%4;
    drw->mem = Drawz; drw->idx = 0; drw->siz = 1; allocDraw(&drw->drw,drw->siz);
    drw->drw[0].con.tag = MicroCon;
    drw->drw[0].con.mic = MicroPierce;
    drw->drw[0].siz = sizeof(crg)/sizeof(int);
    drw->drw[0].sze = sizeof(cal)/sizeof(int);
    allocInt(&drw->drw[0].arg,drw->drw[0].siz);
    allocInt(&drw->drw[0].val,drw->drw[0].sze);
    for (int i = 0; i < drw->drw[0].siz; i++) drw->drw[0].arg[i] = crg[i];
    for (int i = 0; i < drw->drw[0].sze; i++) drw->drw[0].val[i] = cal[i];
    callCopy(drw,save,fun,0,0);}/*TODO issue RelateRes to copy from PierceRes*//*int idx = 0;
    copy->push(MicroTest,0,arg,sizeof(arg)/sizeof(int),idx,0,0,fun,SmartState());*/
    {int save = pull+Memorys; struct Center *drw = centerPull(save); if (!drw) {planeWait(0,0); continue;}
    freeCenter(drw); pull = (pull+1)%4;
    drw->mem = Drawz; drw->idx = 0; drw->siz = 1; allocDraw(&drw->drw,drw->siz);
    drw->drw[0].con.tag = MicroCon;
    drw->drw[0].con.mic = MicroTest;
    drw->drw[0].siz = sizeof(arg)/sizeof(int);
    drw->drw[0].sze = sizeof(val)/sizeof(int);
    allocInt(&drw->drw[0].arg,drw->drw[0].siz);
    allocInt(&drw->drw[0].val,drw->drw[0].sze);
    for (int i = 0; i < drw->drw[0].siz; i++) drw->drw[0].arg[i] = arg[i];
    for (int i = 0; i < drw->drw[0].sze; i++) drw->drw[0].val[i] = val[i];
    callCopy(drw,save,fun,0,0);}}
    else if (count%8 == 1 || count%8 == 5) {/*int idx = 0;
    copy->push(MicroDebug,0,brg,sizeof(brg)/sizeof(int),idx,0,0,fun,SmartState());*/
    int save = pull+Memorys; struct Center *drw = centerPull(save); if (!drw) {planeWait(0,0); continue;}
    freeCenter(drw); pull = (pull+1)%4;
    drw->mem = Drawz; drw->idx = 0; drw->siz = 1; allocDraw(&drw->drw,drw->siz);
    drw->drw[0].con.tag = MicroCon;
    drw->drw[0].con.mic = MicroDebug;
    drw->drw[0].siz = sizeof(brg)/sizeof(int);
    drw->drw[0].sze = sizeof(bal)/sizeof(int);
    allocInt(&drw->drw[0].arg,drw->drw[0].siz);
    allocInt(&drw->drw[0].val,drw->drw[0].sze);
    for (int i = 0; i < drw->drw[0].siz; i++) drw->drw[0].arg[i] = brg[i];
    for (int i = 0; i < drw->drw[0].sze; i++) drw->drw[0].val[i] = bal[i];
    callCopy(drw,save,fun,0,0);}
    else if (count%8 == 2 || count%8 == 6) {
    int width = callInfo(WindowWidth,0,planeRcfg);
    int height = callInfo(WindowHeight,0,planeRcfg);
    int save = pull+Memorys; struct Center *eek = centerPull(save); if (!eek) {planeWait(0,0); continue;}
    freeCenter(eek); pull = (pull+1)%4;
    eek->mem = Peekz; eek->idx = 0; eek->siz = 1; allocPierce(&eek->eek,eek->siz);
    eek->eek[0].wid = 0.5*width; eek->eek[0].hei = 0.5*height; eek->eek[0].val = 1.0;
    callCopy(eek,save,chk,0,0);}
    tested = count;}
}

// phase callbacks
void planeClose(enum Thread tag, int idx)
{
    callJnfo(RegisterOpen,(1<<tag),planeWotc);
}
void planeJoin(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): closeIdent(external); closeIdent(selwake);
    break; case (StdioThd): closeIdent(console); closeIdent(conwake);
    break; case (CopyThd): closeIdent(cpywake);
    break; case (TimeThd): closeIdent(timwake);
    break; case (TestThd): closeIdent(tstwake);}
}
void planeWake(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): writeInt(0,selwake);
    break; case (StdioThd): writeInt(0,conwake);
    break; case (CopyThd): writeInt(0,cpywake);
    break; case (TimeThd): writeInt(0,timwake);
    break; case (TestThd): writeInt(0,tstwake);}
}

// register callbacks
void registerOpen(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterOpen) ERROR();
    if ((act & (1<<PipeThd)) && !(sav & (1<<PipeThd))) {
        if ((external = argument.idx = rdwrInit(argument.inp,argument.out)) < 0) ERROR();
        if ((selwake = openPipe()) < 0) ERROR();
        callFork(PipeThd,0,planeSelect,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<PipeThd)) && (sav & (1<<PipeThd))) {
        writeInt(-1,selwake);}
    if ((act & (1<<StdioThd)) && !(sav & (1<<StdioThd))) {
        if ((console = rdwrInit(STDIN_FILENO,STDOUT_FILENO)) < 0) ERROR();
        if ((conwake = openPipe()) < 0) ERROR();
        callFork(StdioThd,0,planeConsole,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<StdioThd)) && (sav & (1<<StdioThd))) {
        writeInt(-1,conwake);}
    if ((act & (1<<CopyThd)) && !(sav & (1<<CopyThd))) {
        if ((cpywake = openPipe()) < 0) ERROR();
        callFork(CopyThd,0,planeMachine,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<CopyThd)) && (sav & (1<<CopyThd))) {
        callKnfo(MachineIndex,-1,planeWcfg);
        writeInt(-1,cpywake);}
    if ((act & (1<<TimeThd)) && !(sav & (1<<TimeThd))) {
        if ((timwake = openPipe()) < 0) ERROR();
        callFork(TimeThd,0,planeTime,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<TimeThd)) && (sav & (1<<TimeThd))) {
        writeInt(-1,timwake);}
    if ((act & (1<<TestThd)) && !(sav & (1<<TestThd))) {
        if ((tstwake = openPipe()) < 0) ERROR();
        callFork(TestThd,0,planeTest,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<TestThd)) && (sav & (1<<TestThd))) {
        centerPlace(0,0);
        writeInt(-1,tstwake);}
}
void registerWake(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterWake) ERROR();
    for (int i = ffs(val)-1; val; i = ffs(val&=~(1<<i))-1) planeWake(i,0);
}
void registerMask(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterMask) ERROR();
    int open = callKnfo(RegisterOpen,0,planeRcfg);
    int wake = 0;
    for (int i = ffs(val)-1; val; i = ffs(val&=~(1<<i))-1)
    wake |= (sizeAbleq(ableq) > i ? *ptrAbleq(i,ableq) : 0);
    callKnfo(RegisterWake,open&wake,planeRaz);
}
void registerAble(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterAble) ERROR();
    int mask = val >> Threads; // mask of events
    int wake = val & ((1<<Threads)-1); // mask of threads
    for (int i = ffs(mask)-1; mask; i = ffs(mask&=~(1<<i))-1) {
    while (sizeAbleq(ableq) <= i) pushAbleq(0,ableq);
    // map event to set of threads to wake it up
    *ptrAbleq(i,ableq) = wake;}
}
void registerTime(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterTime) ERROR();
    int lwr = val & 0xff;
    int upr = val >> 8;
    if (lwr >= Threads) lwr = Threads;
    if (waitSafe(timeSem) != 0) ERROR();
    if (times[lwr] < start) times[lwr] = processTime();
    float time = times[lwr] + (float)upr/1000.0; times[lwr] = time;
    enum Thread wake = lwr;
    if (sizeTimeq(timeq) && backTimeq(timeq) > time) {
    pushTimeq(backTimeq(timeq),timeq);
    int idx = sizeTimeq(timeq)-2;
    while (idx > 0 && *ptrTimeq(idx,timeq) > time) {idx--;
    *ptrTimeq(idx+1,timeq) = *ptrTimeq(idx,timeq);
    *ptrWakeq(idx+1,wakeq) = *ptrWakeq(idx,wakeq);}
    *ptrTimeq(idx,timeq) = time;
    *ptrWakeq(idx,wakeq) = wake;} else {
    pushTimeq(time,timeq);
    pushWakeq(wake,wakeq);}
    if (postSafe(timeSem) != 1) ERROR();
    writeInt(0,timwake);
}
void registerEval(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterEval) ERROR();
    int idx = callKnfo(RegisterExpr,0,planeRcfg);
    struct Center *ptr = centerPull(idx);
    if (ptr && ptr->mem == Expressz && val < ptr->siz) {
    {char *exp = 0; showExpress(&ptr->exp[val],&exp);
    printf("%s\n",exp); free(exp);}
    if (waitSafe(evalSem) != 0) ERROR();
    jknfo = 1;
    if (postSafe(evalSem) != 1) ERROR();    
    machineVoid(&ptr->exp[val]);}
    centerPlace(ptr,idx);
}

// expression callbacks
const char *planeGetstr()
{
    if (waitSafe(stdioSem) != 0) ERROR();
    if (sizeStrq(strin) == 0) ERROR();
    char *str = frontStrq(strin); popStrq(strin);
    if (postSafe(stdioSem) != 1) ERROR();
    return str;
}
void planePutstr(const char *src)
{
    if (waitSafe(stdioSem) != 0) ERROR();
    char *str = malloc(strlen(src)+1);
    strcpy(str,src); pushStrq(str,strout);
    if (postSafe(stdioSem) != 1) ERROR();
}
void planeSetcfg(int val, int sub)
{
    if (jknfo) callKnfo((enum Configure)sub,val,planeWcfg);
    else callJnfo((enum Configure)sub,val,planeWcfg);
}
int planeRetcfg(int sub)
{
    return callInfo((enum Configure)sub,0,planeRcfg);
}
int planeField(void **dst, const void *src, const void *fld, int idx, int sub, int stp, int ftp)
{
    if (stp == identType("Center")) {
    struct Center *tmp = 0;
    allocCenter(&tmp,1);
    if (waitSafe(dataSem) != 0) ERROR();
    assignDat(dat0,src); readCenter(tmp,idx0);
    if (postSafe(dataSem) != 1) ERROR();
    switch (idx) {default: ERROR();
    break; case(5):
    if (ftp != identType("Int")) ERROR();
    if (sub < 0 || sub >= tmp->siz) ERROR(); // TODO reallocate when siz field changes
    if (waitSafe(dataSem) != 0) ERROR();
    assignDat(dat0,fld); tmp->val[sub] = readInt(idx0);}
    datxVoid(dat0,0); writeCenter(tmp,idx0); assignDat(dst,*dat0);
    if (postSafe(dataSem) != 1) ERROR();
    allocCenter(&tmp,0);
    return stp;}
    return -1;
}
int planeExtract(void **fld, const void *src, int idx, int sub, int typ)
{
    // TODO
}
int planeImmed(void **dat, const char *str)
{
    struct Center *tmp = 0; int len = 0;
    allocCenter(&tmp,1);
    len = 0; if (hideCenter(tmp,str,&len)) {
    if (waitSafe(dataSem) != 0) ERROR();
    datxVoid(dat0,0); writeCenter(tmp,idx0); assignDat(dat,*dat0);
    if (postSafe(dataSem) != 1) ERROR();
    allocCenter(&tmp,0);
    return identType("Center");}
    allocCenter(&tmp,0);
    return -1;
}

void initSafe()
{
    if (!(copySem = allocSafe(1))) ERROR(); // protect array of Center
    if (!(pipeSem = allocSafe(1))) ERROR(); // protect planeSelect queues
    if (!(stdioSem = allocSafe(1))) ERROR(); // protect planeConsole queues
    if (!(timeSem = allocSafe(1))) ERROR(); // protect planeTime queue
    if (!(dataSem = allocSafe(1))) ERROR(); // protect data conversion
    if (!(evalSem = allocSafe(1))) ERROR(); // protect data evaluation
    sub0 = datxSub(); idx0 = puntInit(sub0,sub0,datxReadFp,datxWriteFp); dat0 = datxDat(sub0);
    sub1 = datxSub(); idx1 = puntInit(sub1,sub1,datxReadFp,datxWriteFp); dat1 = datxDat(sub1);
    internal = allocCenterq(); response = allocCenterq();
    strout = allocStrq(); strin = allocStrq(); chrq = allocChrq();
    timeq = allocTimeq(); wakeq = allocWakeq(); ableq = allocAbleq();
    callBack(RegisterOpen,registerOpen);
    callBack(RegisterWake,registerWake);
    callBack(RegisterMask,registerMask);
    callBack(RegisterAble,registerAble);
    callBack(RegisterTime,registerTime);
    callBack(RegisterEval,registerEval);
    datxFnptr(planeRetcfg,planeSetcfg,planeGetstr,planePutstr,planeField,planeExtract,planeImmed);
    start = processTime();
}
void initBoot()
{
    // Bootstrap is after Cmnd so it can elaborate on Cmnd and Cmnd can configure Bootstrap
    int size = 0; int cmnds = 0;
    for (int i = 0; callCmnd(i); i++) {size++; cmnds++;}
    for (int i = 0; Bootstrap__Int__Str(i); i++) size++;
    const char **boot = malloc(size*sizeof(const char *)); size = 0;
    for (int i = 0; callCmnd(i); i++) boot[size++] = callCmnd(i);
    for (int i = 0; Bootstrap__Int__Str(i); i++) boot[size++] = Bootstrap__Int__Str(i);
    for (int i = 0; i < size; i++) {
    int asiz = 0; int csiz = 0; int msiz = 0; int esiz = 0; int ssiz = 0;
    struct Argument arg = {0}; struct Center cntr = {0};
    struct Machine mchn = {0}; struct Express expr = {0}; char *str = 0;
    if (hideArgument(&arg, boot[i], &asiz)) {
    copyArgument(&argument,&arg); freeArgument(&arg);
    if (i < cmnds) callInfo(RegisterShow,1,planeWots);}
    else if (hideCenter(&cntr, boot[i], &csiz)) {struct Center *ptr = 0;
    allocCenter(&ptr,1); copyCenter(ptr,&cntr); freeCenter(&cntr); centerPlace(ptr,centers);
    if (i < cmnds) callInfo(RegisterShow,2,planeWots);}
    else if (hideMachine(&mchn, boot[i], &msiz)) {
    machineSwitch(&mchn); freeMachine(&mchn);
    if (i < cmnds) callInfo(RegisterShow,4,planeWots);}
    else if (hideExpress(&expr, boot[i], &esiz)) {
    machineVoid(&expr); freeExpress(&expr);
    if (i < cmnds) callInfo(RegisterShow,8,planeWots);}
    else if (hideStr(&str,boot[i],&ssiz)) {
    planePutstr(str); freeStr(&str,1);
    if (i < cmnds) callInfo(RegisterShow,16,planeWots);}
    else {fprintf(stderr,"Argument:%d Center:%d Machine:%d Str:%d unmatched:%s\n",asiz,csiz,msiz,ssiz,boot[i]); exit(-1);}}
}
void initPlan()
{
    switch (callInfo(RegisterPlan,0,planeRcfg)) {
    default: ERROR();
    break; case (Bringup): // no commandline arguments
    callJnfo(RegisterPoll,1,planeWcfg);
    callJnfo(MachineIndex,Machinez,planeWcfg);
    callJnfo(RegisterExpr,Expressz,planeWcfg);
    callJnfo(RegisterTime,500<<8,planeWcfg);
    callJnfo(RegisterAble,(((1<<FnceMsk)<<Threads)|(1<<TestThd)),planeWcfg);
    callJnfo(RegisterAble,(((1<<TimeMsk)<<Threads)|(1<<CopyThd)),planeWcfg);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<TestThd),planeWots);
    callJnfo(RegisterOpen,(1<<CopyThd),planeWots);
    callJnfo(RegisterOpen,(1<<TimeThd),planeWots);
    break; case (Builtin): // choose what to test from commandline
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<TestThd),planeWots);
    callJnfo(RegisterOpen,(1<<CopyThd),planeWots);
    callJnfo(RegisterOpen,(1<<TimeThd),planeWots);
    break; case (Regress): // choose how to interpret centers from pipe
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<CopyThd),planeWots);
    callJnfo(RegisterOpen,(1<<PipeThd),planeWots);
    break; case (Release): // use builtin machine to handle user and pipe
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<CopyThd),planeWots);
    callJnfo(RegisterOpen,(1<<PipeThd),planeWots);}
}
void initTest()
{
    const struct Vertex vertices[] = {
        {{-0.5f, -0.5f, 0.20f, 1.0f}, {1.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, -0.5f, 0.40f, 1.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, 0.5f, 0.60f, 1.0f}, {0.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{-0.5f, 0.5f, 0.40f, 1.0f}, {1.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        //
        {{-0.5f, -0.5f, 0.50f, 1.0f}, {1.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, -0.5f, 0.50f, 1.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, 0.5f, 0.50f, 1.0f}, {0.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{-0.5f, 0.5f, 0.50f, 1.0f}, {1.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        //
    };
    const uint16_t indices[] = {
        0, 1, 2, 2, 3, 0,
        4, 5, 6, 6, 7, 4,
    };
    switch (callInfo(RegisterPlan,0,planeRcfg)) {
    default: ERROR();
    break; case (Bringup): {
    struct Fnc fnc = {0,planeSelf,planeGlfw,0,planeGoon};
    struct Fnc fun = {0,planePass,planeGlfw,0,planeGoon};
    int frames = callInfo(ConstantFrames,0,planeRcfg);
    struct Center *ptr = centerPull(Drawz); freeCenter(ptr);
    ptr->mem = Drawz; ptr->siz = 1/*+2*//*Micros*//*+frames*//*+frames*/;
    allocDraw(&ptr->drw,ptr->siz);
    ptr->drw[0].con.tag = ResrcCon;
    ptr->drw[0].con.res = SwapRes;
    callCopy(ptr,Drawz,fun,0,0);
    while (!centerCheck(Drawz)) usleep(1000);
    ptr = centerPull(Drawz); freeCenter(ptr);
    ptr->mem = Drawz; ptr->siz = 3/*Micros*/+frames/*+frames*/; // TODO arg for each ResrcLoc of PierceRes
    allocDraw(&ptr->drw,ptr->siz);
    for (int i = 0; i < 3/*Micros*/; i++) {
    ptr->drw[i].con.tag = ResrcCon;
    ptr->drw[i].con.res = PipeRes;
    int val[] = {/*IDerIns*/i,/*Micro*/i};
    ptr->drw[i].sze = sizeof(val)/sizeof(int);
    allocInt(&ptr->drw[i].val,ptr->drw[i].sze);
    for (int j = 0; j < ptr->drw[i].sze; j++) {
    ptr->drw[i].val[j] = val[j];}}
    for (int i = 0; i < frames; i++) {
    ptr->drw[3/*Micros*/+i].con.tag = ResrcCon;
    ptr->drw[3/*Micros*/+i].con.res = ChainRes;}
    if (0) for (int i = 0; i < frames; i++) { // TODO arg for each ResrcLoc of PierceRes
    ptr->drw[3/*Micros*/+frames+i].con.tag = ResrcCon;
    ptr->drw[3/*Micros*/+frames+i].con.res = PierceRes;
    int val[] = {/*IDerIns*/i,callInfo(WindowWidth,0,planeRcfg),callInfo(WindowHeight,0,planeRcfg)};
    ptr->drw[3/*Micros*/+frames+i].sze = sizeof(val)/sizeof(int);
    allocInt(&ptr->drw[3/*Micros*/+frames+i].val,ptr->drw[3/*Micros*/+frames+i].sze);
    for (int j = 0; j < ptr->drw[3/*Micros*/+frames+i].sze; j++) {
    ptr->drw[3/*Micros*/+frames+i].val[j] = val[j];}}
    callCopy(ptr,Drawz,fun,0,0);
    while (!centerCheck(Drawz)) usleep(1000);
    int width = callInfo(WindowWidth,0,planeRcfg);
    int height = callInfo(WindowHeight,0,planeRcfg);
    struct Center *uni = centerPull(Uniformz); freeCenter(uni);
    uni->mem = Uniformz; uni->siz = 1; allocUniform(&uni->uni,uni->siz);
    uni->uni[0].wid = width; uni->uni[0].hei = height;
    callCopy(uni,Uniformz,fun,0,0);
    struct Center *vtx = centerPull(Bringupz); freeCenter(vtx);
    vtx->mem = Bringupz; vtx->siz = sizeof(vertices)/sizeof(struct Vertex); allocVertex(&vtx->ver,vtx->siz);
    for (int i = 0; i < vtx->siz; i++) memcpy(&vtx->ver[i],&vertices[i],sizeof(struct Vertex));
    callCopy(vtx,Bringupz,fun,0,0);
    struct Center *ind = centerPull(Indexz); freeCenter(ind);
    ind->mem = Indexz; ind->siz = sizeof(indices)/sizeof(int32_t); allocInt32(&ind->ind,ind->siz);
    memcpy(ind->ind,indices,sizeof(indices)); // note that two int16_t are packed into each int32_t; don't care
    callCopy(ind,Indexz,fun,0,0);
    struct Center *img = centerPull(Imagez); freeCenter(img);
    img->mem = Imagez; img->idx = 0; img->siz = 1; allocImage(&img->img,img->siz);
    fmtxStbi(&img->img[0].dat,&img->img[0].wid,&img->img[0].hei,&img->img[0].cha,"texture.jpg");
    callCopy(img,Imagez,fun,0,0);
    struct Center *oke = centerPull(Pokez); freeCenter(oke);
    oke->mem = Pokez; oke->siz = 1; allocPierce(&oke->oke,oke->siz);
    oke->oke[0].wid = width/2; oke->oke[0].hei = height/2; oke->oke[0].val = 1.5;
    callCopy(oke,Pokez,fun,0,0);
    struct Center *eek = centerPull(Peekz); freeCenter(eek);
    eek->mem = Peekz; eek->idx = 0; eek->siz = 1; allocPierce(&eek->eek,eek->siz);
    eek->eek[0].wid = width/2; eek->eek[0].hei = height/2; eek->eek[0].val = 1.0;
    callCopy(eek,Peekz,fun,0,0);
    struct Center *mat = centerPull(Matrixz); freeCenter(mat);
    mat->mem = Matrixz; mat->slf = frames; mat->siz = 4; allocMatrix(&mat->mat,mat->siz);
    float ident[16]; identmat(ident,4);
    float proj[16]; identmat(proj,4);
    *matrc(proj,3,2,4) = 0.83; // b; // row major; row number 3; column number 2
    *matrc(proj,3,3,4) = 0.58; // a; // w = a + bz
    memcpy(&mat->mat[0],ident,sizeof(struct Matrix));
    memcpy(&mat->mat[1],ident,sizeof(struct Matrix));
    memcpy(&mat->mat[2],proj,sizeof(struct Matrix));
    memcpy(&mat->mat[3],ident,sizeof(struct Matrix));
    for (int i = 0; i < frames; i++) callCopy(mat,Matrixz,fnc,1,0);
    } break; case (Builtin): {
    } break; case (Regress): case (Release): {
    }}
}

void planeInit(uftype copy, nftype call, vftype fork, zftype info, zftype jnfo, zftype knfo, oftype cmnd, aftype glfw)
{
    callCopy = copy;
    callBack = call;
    callFork = fork;
    callInfo = info;
    callJnfo = jnfo;
    callKnfo = knfo;
    callCmnd = cmnd;
    callGlfw = glfw;
    initSafe();
    initBoot();
    initPlan();
    initTest();
}
int planeLoop()
{
    switch (callInfo(RegisterPlan,0,planeRcfg)) {default: break;
    break; case (Bringup):
    if ((processTime()-start)*1000 < 2000) return 1;
    break; case (Builtin): case (Regress): case (Release):
    if (callInfo(RegisterExit,0,planeRcfg) == 0) return 1;}
    return 0;
}
void planeDone()
{
    switch (callInfo(RegisterPlan,0,planeRcfg)) {
    default: ERROR();
    break; case (Bringup):
    callJnfo(RegisterOpen,(1<<TimeThd),planeWotc);
    callJnfo(RegisterOpen,(1<<CopyThd),planeWotc);
    callJnfo(RegisterOpen,(1<<TestThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);
    break; case (Builtin):
    callJnfo(RegisterOpen,(1<<TimeThd),planeWotc);
    callJnfo(RegisterOpen,(1<<CopyThd),planeWotc);
    callJnfo(RegisterOpen,(1<<TestThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);
    break; case (Regress):
    callJnfo(RegisterOpen,(1<<PipeThd),planeWotc);
    callJnfo(RegisterOpen,(1<<CopyThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);
    break; case (Release):
    callJnfo(RegisterOpen,(1<<PipeThd),planeWotc);
    callJnfo(RegisterOpen,(1<<CopyThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);}
    // TODO after other destructors on the heap
    // TODO also free heap pointers from face datx fmtx and local globals
    /*datxFnptr(0,0,0,0,0,0,0);
    callBack(RegisterTime,0);
    callBack(RegisterAble,0);
    callBack(RegisterMask,0);
    callBack(RegisterWake,0);
    callBack(RegisterOpen,0);
    freeAbleq(ableq); freeWakeq(wakeq); freeTimeq(timeq);
    freeChrq(chrq); freeStrq(strin); freeStrq(strout);
    freeCenterq(response); freeCenterq(internal);
    closeIdent(idx1); closeIdent(idx0); datxNon();
    if (sem_destroy(&evalSem) != 0) ERROR();
    if (sem_destroy(&dataSem) != 0) ERROR();
    if (sem_destroy(&timeSem) != 0) ERROR();
    if (sem_destroy(&stdioSem) != 0) ERROR();
    if (sem_destroy(&pipeSem) != 0) ERROR();
    if (sem_destroy(&copySem) != 0) ERROR();*/
}
