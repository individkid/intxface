#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "stlx.h"
#include "type.h"
#include "fmtx.h"
#include "sugx.h"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

struct Center **center = 0; // only for planeSwitch
int centers = 0; // only for planeSwitch
void *copySem = 0; // protect centers
int external = 0; // pipe to planeExternal
int extdone = 0; // done for planeExternal
void *internal = 0; // queue of center
void *response = 0; // queue of center
void *pipeSem = 0; // protect internal and response
int console = 0; // pipe to planeConsole
int condone = 0; // done for planeConsole
void *strin = 0; // queue of string
void *strout = 0; // queue of string
void *stdioSem = 0; // protect strin and strout
void *maskq = 0; // map from event to thread mask
void *ableq = 0; // map from thread to vector mask
void *timeq = 0; // queue of wakeup times
void *wakeq = 0; // queue of wakeup threads
void *timeSem = 0; // protect wakeup queues
void **wakeSem[Threads] = {0};
int keepSem[Threads] = {0};
int sizeSem[Threads] = {0};
int *machine = 0;
void *safeSem = 0; // protect machine and wakeSem
float times[Threads] = {0};
// initialized before threads so safe
struct Argument argument = {0}; // constant from commandline
void *chrq = 0; // temporary queue to convert chars to str
void *evalSem = 0;
uftype callCopy = 0;
nftype callBack = 0;
vftype callFork = 0;
zftype callInfo = 0;
zftype callJnfo = 0;
zftype callKnfo = 0;
bftype callHnfo = 0;
oftype callCmnd = 0;
aftype callWait = 0;
float start = 0.0;

DECLARE_DEQUE(struct Center *,Centerq)
DECLARE_DEQUE(char *,Strq)
DECLARE_DEQUE(char, Chrq)
DECLARE_DEQUE(float, Timeq)
DECLARE_DEQUE(enum Thread, Wakeq)
DECLARE_DEQUE(int, Ableq)
DECLARE_DEQUE(int, Maskq)

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
void safeInit(enum Thread thd, int siz, int val)
{
    waitSafe(safeSem);
    if (siz <= sizeSem[thd]) {postSafe(safeSem); return;}
    void **temp = malloc(sizeof(void*)*siz);
    for (int i = 0; i < sizeSem[thd]; i++) temp[i] = wakeSem[thd][i];
    for (int i = sizeSem[thd]; i < siz; i++) temp[i] = allocSafe(val);
    free(wakeSem[thd]); wakeSem[thd] = temp;
    if (thd == MachThd) {
    int *temq = malloc(sizeof(int)*siz);
    for (int i = 0; i < sizeSem[thd]; i++) temq[i] = machine[i];
    for (int i = sizeSem[thd]; i < siz; i++) temq[i] = -1;
    free(machine); machine = temq;}
    keepSem[thd] = siz;
    sizeSem[thd] = siz;
    postSafe(safeSem);
}
void *safeSafe(enum Thread thd, int idx)
{
    waitSafe(safeSem);
    if (thd < 0 || thd >= Threads) ERROR();
    if (idx < 0 || idx >= sizeSem[thd]) ERROR();
    if (wakeSem[thd] == 0 || wakeSem[thd][idx] == 0) ERROR();
    void *ret = wakeSem[thd][idx];
    postSafe(safeSem);
    return ret;
}
void safeJoin(enum Thread thd, int idx)
{
    waitSafe(safeSem);
    void *ptr = safeSafe(thd,idx);
    freeSafe(ptr); wakeSem[thd][idx] = 0; keepSem[thd] -= 1;
    if (keepSem[thd] == 0) {sizeSem[thd] = 0; free(wakeSem[thd]); wakeSem[thd] = 0;}
    postSafe(safeSem);
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
    float width = callInfo(UniformWid,0,planeRcfg);
    float height = callInfo(UniformHei,0,planeRcfg);
    *matrc(mat,3,2,4) = 0.83; // b; // row major; row number 3; column number 2
    *matrc(mat,3,3,4) = 0.58; // a; // w = a + bz
    *matrc(mat,0,0,4) = height/width; // y'=y x'=x*height/width
    return mat;
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
struct Center *centerPull(int idx)
{
    centerSize(idx);
    if (waitSafe(copySem) != 0) ERROR();
    struct Center *ret = center[idx]; center[idx] = 0;
    if (postSafe(copySem) != 1) ERROR();
    return ret;
}
void planePutstr(const char *str);
void centerPlace(struct Center *ptr, int idx)
{
    centerSize(idx);
    if (waitSafe(copySem) != 0) ERROR();
    freeCenter(center[idx]); allocCenter(&center[idx],0); center[idx] = ptr;
    if (postSafe(copySem) != 1) ERROR();
}
// int _debug_ = 0;
void centerDone(struct Center *ptr, int idx)
{
    if ((callJnfo(RegisterSave,0,planeRcfg) & (1<<idx)) != 0) {
    callJnfo(RegisterWake,(1<<DropMsk),planeWots);
    freeCenter(ptr); allocCenter(&ptr,0); return;}
    centerPlace(ptr,idx);
    if (ptr && ptr->slf == 0) {
    callJnfo(RegisterWake,(1<<PassMsk),planeWots);
    /*if ((1<<idx)&~_debug_) {_debug_ |= (1<<idx);
    char *st0 = 0; showMemory((enum Memory)idx,&st0);
    fprintf(stderr,"centerDone 0x%x %s\n",_debug_,st0); free(st0);}*/
    callJnfo(RegisterPass,(1<<idx),planeWots);}
    else if (ptr) {
    callJnfo(RegisterWake,(1<<FailMsk),planeWots);
    callJnfo(RegisterFail,(1<<idx),planeWots);}
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
    break; case (Getintz): return sizeof(int32_t);
    break; case (Getoldz): return sizeof(float);
    break; case (Setintz): return sizeof(int32_t);
    break; case (Setoldz): return sizeof(float);
    break; case (Uniformz): return sizeof(struct Uniform);
    break; case (Matrixz): return sizeof(struct Matrix);
    break; case (Trianglez): return sizeof(struct Triangle);
    break; case (Numericz): return sizeof(struct Numeric);
    break; case (Vertexz): return sizeof(struct Vertex);
    break; case (Basisz): return sizeof(struct Basis);
    break; case (Drawz): return sizeof(struct Draw);
    break; case (Instrz): return sizeof(struct Inst);
    // Stringz
    break; case (Machinez): return sizeof(struct Machine);
    break; case (Expressz): return sizeof(struct Express);
    break; case (Kernelz): return sizeof(struct Kernel);}
    return 0;
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
void planeArgv(int argc, char **argv, int cmnds);
void machineArgv(int sig, int *arg)
{
    if (sig != ArgvArgs) ERROR();
    int src = arg[ArgvSrc];
    struct Center *ptr = centerPull(src);
    planeArgv(ptr->siz,ptr->str,0);
    centerPlace(ptr,src);
}
// manipulation C
// Kernel.saved T
// Kernel.local L
// Kernel.sent S
// Kernal.global G
// Matrix M
void machineComp(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,CompArgs,CompSrc,CompSrcSub);
    struct Kernel *kernel = machineKernel(src,sig,arg,CompArgs,CompSrc,CompSrcSub);
    struct Center *dst = machineCenter(sig,arg,CompArgs,CompDst,CompDstSub);
    struct Matrix *matrix = machineMatrix(dst,sig,arg,CompArgs,CompDst,CompDstSub);
    // TODO compose for draw -- T = C; M = GSLT
    machinePlace(dst,sig,arg,CompArgs,CompDst,CompDstSub);
    machinePlace(src,sig,arg,CompArgs,CompSrc,CompSrcSub);
}
void machineForm(int sig, int *arg)
{
    struct Center *center = machineCenter(sig,arg,FormArgs,FormSrc,FormSrcSub);
    struct Kernel *kernel = machineKernel(center,sig,arg,FormArgs,FormSrc,FormSrcSub);
    // TODO change manipulation matrix -- L = LTC'; T = C
    machinePlace(center,sig,arg,FormArgs,FormSrc,FormSrcSub);
}
void machineSend(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,SendArgs,SendSrc,SendSrcSub);
    struct Kernel *kernel = machineKernel(src,sig,arg,SendArgs,SendSrc,SendSrcSub);
    struct Center *dst = machineCenter(sig,arg,SendArgs,SendDst,SendDstSub);
    struct Matrix *matrix = machineMatrix(dst,sig,arg,SendArgs,SendDst,SendDstSub);
    // TODO move local to sent -- T = C; M = L; S = SL; L = I
    machinePlace(dst,sig,arg,SendArgs,SendDst,SendDstSub);
    machinePlace(src,sig,arg,SendArgs,SendSrc,SendSrcSub);
}
void machineSelf(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
    struct Center *dst = machineCenter(sig,arg,SelfArgs,SelfDst,SelfDstSub);
    struct Kernel *kernel = machineKernel(dst,sig,arg,SelfArgs,SelfDst,SelfDstSub);
    // TODO move portion of sent to global -- G = GM; S = M'S
    machinePlace(dst,sig,arg,SelfArgs,SelfDst,SelfDstSub);
    machinePlace(src,sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
}
void machineGlob(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,GlobArgs,GlobSrc,GlobSrcSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,GlobArgs,GlobSrc,GlobSrcSub);
    struct Center *dst = machineCenter(sig,arg,GlobArgs,GlobDst,GlobDstSub);
    struct Kernel *kernel = machineKernel(dst,sig,arg,GlobArgs,GlobDst,GlobDstSub);
    // TODO absorb discontinuous change -- G = GM
    machinePlace(dst,sig,arg,GlobArgs,GlobDst,GlobDstSub);
    machinePlace(src,sig,arg,GlobArgs,GlobSrc,GlobSrcSub);
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
    case (Bringupz): copyVertex(&dstPtr->ver[dstSub+i],&srcPtr->ver[srcSub+i]); break;
    case (Imagez): copyImage(&dstPtr->img[dstSub+i],&srcPtr->img[srcSub+i]); break;
    case (Getintz): dstPtr->uns[dstSub+i] = srcPtr->uns[srcSub+i]; break;
    case (Setintz): dstPtr->uns[dstSub+i] = srcPtr->uns[srcSub+i]; break;
    case (Getoldz): dstPtr->old[dstSub+i] = srcPtr->old[srcSub+i]; break;
    case (Setoldz): dstPtr->old[dstSub+i] = srcPtr->old[srcSub+i]; break;
    case (Uniformz): copyUniform(&dstPtr->uni[dstSub+i],&srcPtr->uni[srcSub+i]); break;
    case (Matrixz): copyMatrix(&dstPtr->mat[dstSub+i],&srcPtr->mat[srcSub+i]); break;
    case (Trianglez): copyTriangle(&dstPtr->tri[dstSub+i],&srcPtr->tri[srcSub+i]); break;
    case (Numericz): copyNumeric(&dstPtr->num[dstSub+i],&srcPtr->num[srcSub+i]); break;
    case (Vertexz): copyVertex(&dstPtr->vtx[dstSub+i],&srcPtr->vtx[srcSub+i]); break;
    case (Basisz): copyBasis(&dstPtr->bas[dstSub+i],&srcPtr->bas[srcSub+i]); break;
    case (Drawz): copyDraw(&dstPtr->drw[dstSub+i],&srcPtr->drw[srcSub+i]); break;
    case (Instrz): copyInst(&dstPtr->ins[dstSub+i],&srcPtr->ins[srcSub+i]); break;
    case (Stringz): assignStr(&dstPtr->str[dstSub+i],srcPtr->str[srcSub+i]); break;
    case (Machinez): copyMachine(&dstPtr->mch[dstSub+i],&srcPtr->mch[srcSub+i]); break;
    case (Expressz): copyExpress(&dstPtr->exp[dstSub+i],&srcPtr->exp[srcSub+i]); break;
    case (Kernelz): copyKernel(&dstPtr->ker[dstSub+i],&srcPtr->ker[srcSub+i]); break;
    case (Configurez): dstPtr->cfg[dstSub+i] = srcPtr->cfg[srcSub+i];
    dstPtr->val[dstSub+i] = srcPtr->val[srcSub+i]; break;}
    machinePlace(srcPtr,sig,arg,BopyArgs,BopySrc,BopySrcSub);
    machinePlace(dstPtr,sig,arg,BopyArgs,BopyDst,BopyDstSub);
}
void machineCopy(int sig, int *arg)
{
    if (sig != CopyArgs) ERROR();
    int src = arg[CopySrc];
    struct Center *ptr = centerPull(src);
    callCopy(ptr,src,RetRsp,0,0);
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
    switch (cfg) {default: {
    centerSize(idx);
    if (waitSafe(copySem) != 0) ERROR();
    struct Center *ptr = center[idx];
    switch (cfg) {default: ERROR();
    case (CenterPtr): callJnfo(cfg,(ptr!=0),planeWcfg); break;
    case (CenterMem): callJnfo(cfg,ptr->mem,planeWcfg); break;
    case (CenterSiz): callJnfo(cfg,ptr->siz,planeWcfg); break;
    case (CenterIdx): callJnfo(cfg,ptr->idx,planeWcfg); break;
    case (CenterSlf): callJnfo(cfg,ptr->slf,planeWcfg); break;}
    if (postSafe(copySem) != 1) ERROR();}
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
    if (waitSafe(evalSem) != 0) ERROR();
    writeCenter(ptr,datxClr(0)); freeCenter(ptr); allocCenter(&ptr,0);
    void *dat0 = 0; datxStr(&dat0,"_");
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPECenter);
    free(dat0); free(dat1);
    void *dat = 0; int typ = datxEval(&dat,exp,TYPECenter);
    if (typ != TYPECenter) ERROR();
    allocCenter(&ptr,1);
    readCenter(ptr,datxPut(0,dat));
    free(dat);
    if (postSafe(evalSem) != 1) ERROR();
    centerPlace(ptr,idx);
}
int machineIval(struct Express *exp)
{
    if (waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int typ = datxEval(&dat,exp,TYPEInt);
    if (typ != identType("Int")) ERROR();
    int val = readInt(datxPut(0,dat));
    free(dat);
    if (postSafe(evalSem) != 1) ERROR();
    return val;
}
void machineVoid(struct Express *exp)
{
    if (!callHnfo() && waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int typ = datxEval(&dat,exp,-1); free(dat);
    if (!callHnfo() && postSafe(evalSem) != 1) ERROR();
}
int machineEscape(struct Center *current, int level, int next)
{
    int inc = (level > 0 ? 1 : (level == 0 ? 0 : -1)); level *= inc;
    while (1) {
    next += inc;
    if (next < 0 || next >= current->siz) break;
    struct Machine *mptr = &current->mch[next];
    if (mptr->xfr == Nest) level += mptr->lvl*inc;
    if (level <= 0) break;}
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
    case (Eval): machineEval(&mptr->fnc[0],machineIval(&mptr->res[0])); break;
    case (Void): machineVoid(&mptr->exp[0]); break;
    case (Argv): MACHINE(Argv) break;
    case (Comp): MACHINE(Comp) break;
    case (Form): MACHINE(Form) break;
    case (Send): MACHINE(Send) break;
    case (Self): MACHINE(Self) break;
    case (Glob): MACHINE(Glob) break;
    case (Bopy): MACHINE(Bopy) break;
    case (Copy): MACHINE(Copy) break;
    case (Dopy): MACHINE(Dopy) break;
    case (Popy): MACHINE(Popy) break;
    case (Qopy): MACHINE(Qopy) break;}
}

// thread callbacks
void planeMachine(enum Thread tag, int idx)
{
    int next = 0;
    while (next >= 0) {
    int index = -1;
    struct Center *current = 0;
    if (current == 0) {
    waitSafe(safeSem);
    index = machine[idx];
    postSafe(safeSem);
    if (index < 0) break;
    current = centerPull(index);
    if (current == 0) break;
    if (current->mem != Machinez) break;
    if (current->siz <= 0) break;
    next = 0;}
    while (next >= 0 && next < current->siz) {
    struct Machine *mptr = &current->mch[next];
    /*{char *opr = 0; showMachine(mptr,&opr);
    fprintf(stderr,"%d-%.3f-%s\n",next,(float)processTime(),opr); free(opr);}*/
    int save = next;
    switch (mptr->xfr) {default: machineSwitch(mptr); next += 1; break;
    case (Goto): next += machineIval(&mptr->exp[0]); break;
    case (Jump): next = machineEscape(current,machineIval(&mptr->exp[0]),next); break;
    case (Nest): next += 1; break;}
    if (next == save) {
    if (waitSafe(safeSafe(MachThd,0)) < 0) next = -1;
    else next += 1;}}
    centerPlace(current,index); current = 0;}
}
void planeCenter(enum Thread tag, int idx)
{
    while (1) {
    if (waitSafe(safeSafe(PipeThd,idx)) < 0) break;
    if (waitSafe(pipeSem) != 0) ERROR();
    struct Center *center = maybeCenterq(0,response);
    int self = center->slf;
    if (self < 0) pushCenterq(center,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    if (center && self >= 0) {
    writeCenter(center,external); freeCenter(center); allocCenter(&center,0);}}
}
void planeExternal(enum Thread tag, int idx)
{
    while (1) {
    int sub = waitRead(0.0,(1<<external)|(1<<extdone));
    if (sub == extdone) break;
    if (sub == external) {
    struct Center *center = 0;
    allocCenter(&center,1);
    readCenter(center,external);
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(center,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    callJnfo(RegisterWake,(1<<SlctMsk),planeWots);}
    else ERROR();}
}
void planeString(enum Thread tag, int idx)
{
    while (1) {
    if (waitSafe(safeSafe(StdioThd,idx)) < 0) break;
    while (1) {
    if (waitSafe(stdioSem) != 0) ERROR();
    char *str = maybeStrq(0,strout);
    if (postSafe(stdioSem) != 1) ERROR();
    if (str == 0) break;
    writeStr(str,console);
    free(str);}}
}
void planeConsole(enum Thread tag, int idx)
{
    while (1) {
    int sub = waitRead(0.0,(1<<console)|(1<<condone));
    if (sub == condone) break;
    if (sub == console) {
    char chr = readChr(console);
    pushChrq(chr,chrq);
    if (chr == '\n') {char *str = malloc(sizeChrq(chrq)+1); char *ptr = str;
    while (sizeChrq(chrq)) {*(ptr++) = frontChrq(chrq); popChrq(chrq);} *(ptr++) = 0;
    if (waitSafe(stdioSem) != 0) ERROR();
    pushStrq(str,strin);
    int size = sizeStrq(strin);
    if (postSafe(stdioSem) != 1) ERROR();
    callJnfo(RegisterWake,(1<<CnslMsk),planeWots);
    callJnfo(RegisterStrq,size,planeWcfg);}}
    else ERROR();}
}
void planeTime(enum Thread tag, int idx)
{
    // wait for smallest requested time, send interrupt first time it is exceeded
    while (1) {
    if (waitSafe(timeSem) != 0) ERROR();
    if (sizeTimeq(timeq) == 0) {
    if (postSafe(timeSem) != 1) ERROR();
    if (timeSafe(safeSafe(TimeThd,0),0.0) < 0) break; else continue;}
    if (sizeTimeq(timeq) != sizeWakeq(wakeq)) ERROR();
    float time = frontTimeq(timeq); int wake = frontWakeq(wakeq);
    if (postSafe(timeSem) != 1) ERROR();
    float delta = time-(float)processTime(); // how long to wait
    if (timeSafe(safeSafe(TimeThd,0),delta) < 0) break;
    if ((float)processTime() >= time) {
    if (waitSafe(timeSem) != 0) ERROR();
    dropTimeq(timeq); dropWakeq(wakeq);
    if (postSafe(timeSem) != 1) ERROR();
    callJnfo(RegisterWake,(1<<TimeMsk),planeWots);}}
}
void planeTest(enum Thread tag, int idx)
{
    switch (idx) {default: ERROR();
    break; case (0): {
    int debug = 0; int count = 0; float time = 0.0; int tested = 0;
    int giv[] = {0,12}; // idx,siz
    while (timeSafe(safeSafe(TestThd,idx),0.0) >= 0) {
    struct Center *mat = centerPull(Memorys+0); if (!mat) {callWait(); continue;}
    freeCenter(mat);
    mat->mem = Matrixz; mat->idx = 2; mat->siz = 2; allocMatrix(&mat->mat,mat->siz);
    float proj[16]; planeWindow(proj);
    float dbg[16]; planeDebug(dbg);
    memcpy(&mat->mat[0],proj,sizeof(struct Matrix));
    memcpy(&mat->mat[1],dbg,sizeof(struct Matrix));
    callCopy(mat,Memorys+0,RptRsp,1,(debug?"matrix":0));
    if (time == 0.0) time = processTime();
    if (processTime()-time > 0.1) {time = processTime(); count += 1;}
    if (count == tested) {
    struct Center *drw = centerPull(Memorys+1); if (!drw) {callWait(); continue;}
    freeCenter(drw);
    drw->mem = Drawz; drw->idx = 0; drw->siz = 1; allocDraw(&drw->drw,drw->siz);
    drw->drw[0].con.tag = MicroCon;
    drw->drw[0].con.mic = MicroTest;
    drw->drw[0].siz = sizeof(giv)/sizeof(int);
    allocInt(&drw->drw[0].arg,drw->drw[0].siz);
    for (int i = 0; i < drw->drw[0].siz; i++) drw->drw[0].arg[i] = giv[i];
    callCopy(drw,Memorys+1,RetRsp,0,(debug?"test":0));}
    tested = count;}}
    break; case (1): {
    int debug = 0; int count = 0; float time = 0.0; int tested = 0;
    int hiv[] = {callInfo(UniformWid,0,planeRcfg),callInfo(UniformHei,0,planeRcfg),0,12}; // width,height,idx,siz
    while (timeSafe(safeSafe(TestThd,idx),0.0) >= 0) {
    if (time == 0.0) time = processTime();
    if (processTime()-time > 0.1) {time = processTime(); count += 1;}
    if (count == tested) {}
    else if (count%8 == 1 || count%8 == 5) {
    struct Center *drw = centerPull(Memorys+2); if (!drw) {callWait(); continue;}
    freeCenter(drw);
    drw->mem = Drawz; drw->idx = 0; drw->siz = 1; allocDraw(&drw->drw,drw->siz);
    drw->drw[0].con.tag = MicroCon;
    drw->drw[0].con.mic = MicroDebug;
    drw->drw[0].siz = sizeof(hiv)/sizeof(int);
    allocInt(&drw->drw[0].arg,drw->drw[0].siz);
    for (int i = 0; i < drw->drw[0].siz; i++) drw->drw[0].arg[i] = hiv[i];
    callCopy(drw,Memorys+2,RptRsp,0,(debug?"debug":0));}
    else if (count%8 == 2 || count%8 == 6) {
    int width = callInfo(UniformWid,0,planeRcfg);
    int height = callInfo(UniformHei,0,planeRcfg);
    struct Center *eek = centerPull(Memorys+3); if (!eek) {callWait(); continue;}
    freeCenter(eek);
    eek->mem = Getoldz; eek->idx = (int)(0.3*width)+(int)(0.3*height)*width; eek->siz = 1;
    allocOld(&eek->old,eek->siz);
    callCopy(eek,Memorys+3,RptRsp,0,(debug?"peek":0));}
    tested = count;}}}
}

// phase callbacks
void planeClose(enum Thread tag, int idx)
{
    callJnfo(RegisterOpen,(1<<tag),planeWotc);
}
void planeJoin(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): if (idx) {freeIdent(external); closeIdent(extdone);}
    break; case (StdioThd): if (idx) {freeIdent(console); closeIdent(condone);}
    break; case (MachThd): case (TimeThd): case (TestThd):}
}
void planeWake(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): case (StdioThd): case (MachThd): case (TimeThd): case (TestThd):}
    postSafe(safeSafe(tag,idx));
}

// register callbacks
void registerCall(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterCall) ERROR();
    int wake = val & 0xff; // thread to wake
    int indx = val >> 8; // machine center
    safeInit(MachThd,wake+1,0);
    waitSafe(safeSem);
    int save = machine[wake]; machine[wake] = indx;
    postSafe(safeSem);
    if (save < 0) callFork(MachThd,wake,planeMachine,planeClose,planeJoin,planeWake);
    else if (indx < 0) doneSafe(safeSafe(MachThd,wake));
    else postSafe(safeSafe(MachThd,wake));
}
void registerOpen(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterOpen) ERROR();
    if ((act & (1<<PipeThd)) && !(sav & (1<<PipeThd))) {
        extdone = openPipe();
        if ((external = argument.idx = rdwrInit(argument.inp,argument.out)) < 0) ERROR();
        safeInit(PipeThd,1,0);
        callFork(PipeThd,0,planeCenter,planeClose,planeJoin,planeWake);
        callFork(PipeThd,1,planeExternal,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<PipeThd)) && (sav & (1<<PipeThd))) {
        doneSafe(safeSafe(PipeThd,0));
        writeChr(0,extdone);}
    if ((act & (1<<StdioThd)) && !(sav & (1<<StdioThd))) {
        condone = openPipe();
        if ((console = rdwrInit(STDIN_FILENO,STDOUT_FILENO)) < 0) ERROR();
        safeInit(StdioThd,1,0);
        callFork(StdioThd,0,planeString,planeClose,planeJoin,planeWake);
        callFork(StdioThd,1,planeConsole,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<StdioThd)) && (sav & (1<<StdioThd))) {
        doneSafe(safeSafe(StdioThd,0));
        writeChr(0,condone);}
    if ((act & (1<<MachThd)) && !(sav & (1<<MachThd))) {
        callKnfo(RegisterCall,callKnfo(RegisterMain,0,planeRcfg)<<8,planeWcfg);}
    if (!(act & (1<<MachThd)) && (sav & (1<<MachThd))) {
        callKnfo(RegisterCall,-1<<8,planeWcfg);}
    if ((act & (1<<TimeThd)) && !(sav & (1<<TimeThd))) {
        safeInit(TimeThd,1,0);
        callFork(TimeThd,0,planeTime,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<TimeThd)) && (sav & (1<<TimeThd))) {
        doneSafe(safeSafe(TimeThd,0));}
    if ((act & (1<<TestThd)) && !(sav & (1<<TestThd))) {
        safeInit(TestThd,2,0);
        callFork(TestThd,0,planeTest,planeClose,planeJoin,planeWake);
        callFork(TestThd,1,planeTest,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<TestThd)) && (sav & (1<<TestThd))) {
        doneSafe(safeSafe(TestThd,0));
        doneSafe(safeSafe(TestThd,1));}
}
void registerWake(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterWake) ERROR();
    int mask = act&~sav;
    int wake = 0;
    for (int i = ffs(mask)-1; mask; i = ffs(mask&=~(1<<i))-1) {
    int able = (sizeAbleq(ableq) > i ? *ptrAbleq(i,ableq) : 0);
    wake |= able;}
    wake &= callKnfo(RegisterOpen,0,planeRcfg);
    for (int i = ffs(wake)-1; wake; i = ffs(wake&=~(1<<i))-1) {
    planeWake(i,0);}
}
void registerAble(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterAble) ERROR();
    int wake = val & 0xff; // thread to wake
    int mask = val >> 8; // mask of events
    while (sizeMaskq(maskq) <= wake) pushMaskq(0,maskq);
    int even = *ptrMaskq(wake,maskq);
    for (int i = ffs(even)-1; even; i = ffs(even&=~(1<<i))-1) {
    while (sizeAbleq(ableq) <= i) pushAbleq(0,ableq);
    *ptrAbleq(i,ableq) &= ~(1<<wake);}
    *ptrMaskq(wake,maskq) = mask;
    for (int i = ffs(mask)-1; mask; i = ffs(mask&=~(1<<i))-1) {
    while (sizeAbleq(ableq) <= i) pushAbleq(0,ableq);
    *ptrAbleq(i,ableq) |= 1<<wake;}
}
void registerTime(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterTime) ERROR();
    int lwr = val & 0xff; // time to advance
    int upr = val >> 8; // amount to advance
    if (lwr < 0 || lwr >= Threads) ERROR();
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
    postSafe(safeSafe(TimeThd,0));
}
void registerEval(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterEval) ERROR();
    int lwr = val & 0xff; // expr in center
    int upr = val >> 8; // center of expr
    struct Center *ptr = centerPull(upr);
    if (ptr && ptr->mem == Expressz && lwr >= 0 && lwr < ptr->siz) {
    /*{char *exp = 0; showExpress(&ptr->exp[lwr],&exp);
    printf("%s\n",exp); free(exp);}*/
    machineVoid(&ptr->exp[lwr]);}
    centerPlace(ptr,upr);
}
void registerUniform(enum Configure cfg, int sav, int val, int act)
{
    callKnfo(RegisterWake,(1<<UnifMsk),planeWots);
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
    if (postSafe(safeSafe(StdioThd,0)) <= 0) ERROR();
    if (postSafe(stdioSem) != 1) ERROR();
}
void planeSetcfg(int val, int sub)
{
    if (callHnfo()) callKnfo((enum Configure)sub,val,planeWcfg);
    else callJnfo((enum Configure)sub,val,planeWcfg);
}
void planeWoscfg(int val, int sub)
{
    if (callHnfo()) callKnfo((enum Configure)sub,val,planeWots);
    else callJnfo((enum Configure)sub,val,planeWots);
}
void planeWoccfg(int val, int sub)
{
    if (callHnfo()) callKnfo((enum Configure)sub,val,planeWotc);
    else callJnfo((enum Configure)sub,val,planeWotc);
}
int planeRawcfg(int val, int sub)
{
    if (callHnfo()) callKnfo((enum Configure)sub,val,planeRdwr);
    else callJnfo((enum Configure)sub,val,planeRdwr);
}
int planeRetcfg(int sub)
{
    return callInfo((enum Configure)sub,0,planeRcfg);
}
int planeTopcfg(int sub)
{
    return callJnfo((enum Configure)sub,0,planeRcfg);
}
void planeSugar(const char *str)
{
    struct Express **exp = 0;
    int dim = sugarHide(&exp,str);
    for (int i = 0; i < dim; i++) {
    machineVoid(exp[i]);
    freeExpress(exp[i]);
    allocExpress(&exp[i],0); exp[i] = 0;}
    free(exp);
}
int planeSugval(const char *str)
{
    struct Express **exp = 0;
    if (sugarHide(&exp,str) != 1) ERROR();
    int ret = machineIval(exp[0]);
    freeExpress(exp[0]);
    allocExpress(&exp[0],0); exp[0] = 0;
    free(exp);
    return ret;
}
void planeArgv(int argc, char **argv, int cmnds)
{
    for (int i = 0; i < argc; i++) {
    sugarRepl(&argv[i],'$'); // replace $() by Express
    sugarEval(planeSugar,argv[i],'!'); // evaluate !() in the embedding
    sugarFilt(&argv[i],'!');} // filter out !() before hide and process below
    for (int i = 0; i < argc; i++) {
    // fprintf(stderr,"argv--%s--\n",argv[i]);
    int asiz = 0; int csiz = 0; int msiz = 0; int esiz = 0; int ssiz = 0;
    struct Argument arg = {0}; struct Center cntr = {0}; struct Machine mchn = {0};
    struct Express expr = {0}; char *str = 0;
    if (hideArgument(&arg, argv[i], &asiz)) {
    copyArgument(&argument,&arg); freeArgument(&arg);
    if (i < cmnds) callInfo(RegisterShow,1,planeWots);}
    else if (hideCenter(&cntr, argv[i], &csiz)) {struct Center *ptr = 0;
    allocCenter(&ptr,1); copyCenter(ptr,&cntr); freeCenter(&cntr); centerPlace(ptr,centers);
    if (i < cmnds) callInfo(RegisterShow,2,planeWots);}
    else if (hideMachine(&mchn, argv[i], &msiz)) {
    machineSwitch(&mchn); freeMachine(&mchn);
    if (i < cmnds) callInfo(RegisterShow,4,planeWots);}
    else if (hideExpress(&expr, argv[i], &esiz)) {
    machineVoid(&expr); freeExpress(&expr);
    if (i < cmnds) callInfo(RegisterShow,8,planeWots);}
    else if (hideStr(&str,argv[i],&ssiz)) {
    planePutstr(str); freeStr(&str,1);
    if (i < cmnds) callInfo(RegisterShow,16,planeWots);}
    else {fprintf(stderr,"Argument:%d Center:%d Machine:%d Str:%d unmatched:%s\n",asiz,csiz,msiz,ssiz,argv[i]); exit(-1);}}
}

void initSafe()
{
    if (!(copySem = allocSafe(1))) ERROR(); // protect array of Center
    if (!(pipeSem = allocSafe(1))) ERROR(); // protect internal and response queues
    if (!(stdioSem = allocSafe(1))) ERROR(); // protect planeConsole queues
    if (!(timeSem = allocSafe(1))) ERROR(); // protect planeTime queue
    if (!(evalSem = allocSafe(1))) ERROR(); // protect data evaluation
    if (!(safeSem = allocSafe(1))) ERROR(); // protect thread semaphores
    internal = allocCenterq(); response = allocCenterq();
    strout = allocStrq(); strin = allocStrq(); chrq = allocChrq();
    timeq = allocTimeq(); wakeq = allocWakeq();
    ableq = allocAbleq(); maskq = allocMaskq();
    callBack(RegisterCall,registerCall);
    callBack(RegisterOpen,registerOpen);
    callBack(RegisterWake,registerWake);
    callBack(RegisterAble,registerAble);
    callBack(RegisterTime,registerTime);
    callBack(RegisterEval,registerEval);
    callBack(UniformAll,registerUniform);
    callBack(UniformOne,registerUniform);
    callBack(UniformIdx,registerUniform);
    callBack(UniformUse,registerUniform);
    callBack(UniformTri,registerUniform);
    callBack(UniformNum,registerUniform);
    callBack(UniformVtx,registerUniform);
    callBack(UniformMat,registerUniform);
    callBack(UniformBas,registerUniform);
    callBack(UniformMod,registerUniform);
    callBack(UniformWid,registerUniform);
    callBack(UniformHei,registerUniform);
    datxFnptr(planeRetcfg,planeTopcfg,planeSetcfg,planeWoscfg,planeWoccfg,planeRawcfg,planeGetstr,planePutstr);
    start = processTime();
}
void initBoot()
{
    // Bootstrap is after Cmnd so it can elaborate on Cmnd and Cmnd can configure Bootstrap
    int size = 0; int cmnds = 0;
    for (int i = 0; callCmnd(i); i++) {size++; cmnds++;}
    for (int i = 0; Bootstrap__Int__Str(i); i++) size++;
    const char **temp = malloc(size*sizeof(const char *)); size = 0;
    for (int i = 0; callCmnd(i); i++) temp[size++] = callCmnd(i);
    for (int i = 0; Bootstrap__Int__Str(i); i++) temp[size++] = Bootstrap__Int__Str(i);
    char **boot = malloc(size*sizeof(char *));
    for (int i = 0; i < size; i++) {
    int len = strlen(temp[i]);
    boot[i] = malloc(len+1);
    strncpy(boot[i],temp[i],len); boot[i][len] = 0;}
    planeArgv(size,boot,cmnds);
    for (int i = 0; i < size; i++) free(boot[i]); free(boot);
}
void initPlan()
{
    switch (callInfo(RegisterPlan,0,planeRcfg)) {
    default: ERROR();
    break; case (Bringup): // no commandline arguments
    callJnfo(RegisterPoll,1,planeWcfg);
    callJnfo(RegisterMain,planeSugval("@machine"),planeWcfg);
    callJnfo(RegisterAble,((((1<<TimeMsk)|(1<<PassMsk))<<8)|MachThd),planeWcfg);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<MachThd),planeWots);
    callJnfo(RegisterOpen,(1<<TimeThd),planeWots);
    callJnfo(RegisterTime,500<<8,planeWcfg);
    callJnfo(RegisterOpen,(1<<StdioThd),planeWots);
    break; case (Builtin): // choose what to test from commandline
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<MachThd),planeWots);
    break; case (Regress): // choose how to interpret centers from pipe
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<MachThd),planeWots);
    callJnfo(RegisterOpen,(1<<PipeThd),planeWots);
    break; case (Release): // use builtin machine to handle user and pipe
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<MachThd),planeWots);
    callJnfo(RegisterOpen,(1<<PipeThd),planeWots);
    callJnfo(RegisterOpen,(1<<StdioThd),planeWots);}
}
void initTest()
{
    int debug = 0;
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
    struct Center *ptr = 0;
    int frames = callInfo(ConstantFrames,0,planeRcfg);
    ptr = centerPull(Drawz); freeCenter(ptr);
    ptr->mem = Drawz; ptr->siz = 1;
    allocDraw(&ptr->drw,ptr->siz);
    ptr->drw[0].con.tag = ResrcCon;
    ptr->drw[0].con.res = SwapRes;
    callCopy(ptr,Drawz,RptRsp,0,(debug?"swap":0));
    while (!centerCheck(Drawz)) usleep(1000);
    ptr = centerPull(Drawz); freeCenter(ptr);
    ptr->mem = Drawz; ptr->siz = Micros;
    allocDraw(&ptr->drw,ptr->siz);
    for (int i = 0; i < Micros; i++) {
    ptr->drw[i].con.tag = ResrcCon;
    ptr->drw[i].con.res = PipeRes;
    int val[] = {/*IDerIns*/i,/*Micro*/i};
    ptr->drw[i].sze = sizeof(val)/sizeof(int);
    allocInt(&ptr->drw[i].val,ptr->drw[i].sze);
    for (int j = 0; j < ptr->drw[i].sze; j++) {
    ptr->drw[i].val[j] = val[j];}}
    callCopy(ptr,Drawz,MptRsp,0,(debug?"pipe":0));
    while (!centerCheck(Drawz)) usleep(1000);
    for (int i = 0; i < frames; i++) {
    ptr = centerPull(Drawz); freeCenter(ptr);
    ptr->mem = Drawz; ptr->siz = 1;
    allocDraw(&ptr->drw,ptr->siz);
    ptr->drw[0].con.tag = ResrcCon;
    ptr->drw[0].con.res = ChainRes;
    callCopy(ptr,Drawz,MptRsp,0,(debug?"chain":0));
    while (!centerCheck(Drawz)) usleep(1000);}
    // write to PierceRes with Setintz and read back the same with Getintz
    // Getintz should use OldDerIns instead of GetDerIns
    int width = callInfo(UniformWid,0,planeRcfg);
    int height = callInfo(UniformHei,0,planeRcfg); callJnfo(UniformHei,height,planeWcfg);
    struct Center *uni = centerPull(Uniformz); freeCenter(uni);
    uni->mem = Uniformz; uni->siz = 1; allocUniform(&uni->uni,uni->siz);
    uni->uni[0].wid = width; uni->uni[0].hei = height;
    callCopy(uni,Uniformz,RptRsp,0,(debug?"uniform":0));
    struct Center *vtx = centerPull(Bringupz); freeCenter(vtx);
    vtx->mem = Bringupz; vtx->siz = sizeof(vertices)/sizeof(struct Vertex); allocVertex(&vtx->ver,vtx->siz);
    for (int i = 0; i < vtx->siz; i++) memcpy(&vtx->ver[i],&vertices[i],sizeof(struct Vertex));
    callCopy(vtx,Bringupz,RptRsp,0,(debug?"bringup":0));
    struct Center *ind = centerPull(Indexz); freeCenter(ind);
    ind->mem = Indexz; ind->siz = sizeof(indices)/sizeof(int32_t); allocInt32(&ind->ind,ind->siz);
    memcpy(ind->ind,indices,sizeof(indices)); // note that two int16_t are packed into each int32_t; don't care
    callCopy(ind,Indexz,RptRsp,0,(debug?"index":0));
    struct Center *img = centerPull(Imagez); freeCenter(img);
    img->mem = Imagez; img->idx = 0; img->siz = 1; allocImage(&img->img,img->siz);
    fmtxStbi(&img->img[0].dat,&img->img[0].wid,&img->img[0].hei,&img->img[0].cha,"texture.jpg");
    callCopy(img,Imagez,RptRsp,0,(debug?"image":0));

    struct Center *eek = centerPull(Getoldz); freeCenter(eek);
    eek->mem = Getoldz; eek->idx = (int)(0.3*width)+(int)(0.3*height)*width; eek->siz = 1;
    allocOld(&eek->old,eek->siz); eek->old[0] = 1.0;
    callCopy(eek,Getoldz,RptRsp,1,(debug?"peek":0));

    for (int i = 0; i < frames; i++) {
    struct Center *mat = centerPull(Matrixz); freeCenter(mat);
    mat->mem = Matrixz; mat->slf = frames; mat->siz = 4; allocMatrix(&mat->mat,mat->siz);
    float ident[16]; identmat(ident,4);
    float proj[16]; planeWindow(proj);
    memcpy(&mat->mat[0],ident,sizeof(struct Matrix));
    memcpy(&mat->mat[1],ident,sizeof(struct Matrix));
    memcpy(&mat->mat[2],proj,sizeof(struct Matrix));
    memcpy(&mat->mat[3],ident,sizeof(struct Matrix));
    callCopy(mat,Matrixz,RptRsp,0,(debug?"initmat":0));

    while (!centerCheck(Matrixz)) callWait();}
    callJnfo(RegisterOpen,(1<<TestThd),planeWots);}
    break; case (Builtin): case (Regress): case (Release): {}}
}

void planeInit(uftype copy, nftype call, vftype fork, zftype info, zftype jnfo, zftype knfo, bftype hnfo, oftype cmnd, aftype wait)
{
    callCopy = copy;
    callBack = call;
    callFork = fork;
    callInfo = info;
    callJnfo = jnfo;
    callKnfo = knfo;
    callHnfo = hnfo;
    callCmnd = cmnd;
    callWait = wait;
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
    // doneTest();
    // donePlan();
    switch (callInfo(RegisterPlan,0,planeRcfg)) {
    default: ERROR();
    break; case (Bringup):
    callJnfo(RegisterOpen,(1<<TestThd),planeWotc);
    callJnfo(RegisterOpen,(1<<StdioThd),planeWots);
    callJnfo(RegisterOpen,(1<<TimeThd),planeWotc);
    callJnfo(RegisterOpen,(1<<MachThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);
    break; case (Builtin):
    callJnfo(RegisterOpen,(1<<TestThd),planeWotc);
    callJnfo(RegisterOpen,(1<<MachThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);
    break; case (Regress):
    callJnfo(RegisterOpen,(1<<PipeThd),planeWotc);
    callJnfo(RegisterOpen,(1<<MachThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);
    break; case (Release):
    callJnfo(RegisterOpen,(1<<StdioThd),planeWots);
    callJnfo(RegisterOpen,(1<<PipeThd),planeWotc);
    callJnfo(RegisterOpen,(1<<MachThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);}
    // TODO after other destructors on the heap
    // TODO also free heap pointers from face datx fmtx and local globals
    // doneBoot();
    // doneSafe();
    // datxFnptr(0,0,0,0,0,0,0);
    /*callBack(RegisterTime,0);
    callBack(RegisterAble,0);
    callBack(RegisterWake,0);
    callBack(RegisterOpen,0);
    freeMaskq(maskq); freeAbleq(ableq);
    freeWakeq(wakeq); freeTimeq(timeq);
    freeChrq(chrq); freeStrq(strin); freeStrq(strout);
    freeCenterq(response); freeCenterq(internal);
    closeIdent(idx1); closeIdent(idx0); datxNon();
 else safeJoin(tag,idx);
 else safeJoin(tag,idx);
 safeJoin(tag,idx);
    if (sem_destroy(&safeSem) != 0) ERROR();
    if (sem_destroy(&evalSem) != 0) ERROR();
    if (sem_destroy(&timeSem) != 0) ERROR();
    if (sem_destroy(&stdioSem) != 0) ERROR();
    if (sem_destroy(&pipeSem) != 0) ERROR();
    if (sem_destroy(&copySem) != 0) ERROR();
    */
}
