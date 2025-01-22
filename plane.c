#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "stlx.h"
#include "type.h"
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
int external = 0; // safe pipe descriptor
int wakeup = 0; // safe pipe descriptor
struct Argument argument = {0}; // constant from commandline
void *internal = 0; // queue of center; protect with pipeSem
void *response = 0; // queue of center; protect with pipeSem
void *strout = 0; // queue of string; protect with stdioSem
void *strin = 0; // queue of string; protect with stdioSem
int sub0 = 0; int idx0 = 0; void **dat0 = 0; // protect with dataSem
sem_t waitSem = {0};
sem_t copySem = {0};
sem_t pipeSem = {0};
sem_t stdioSem = {0};
sem_t dataSem = {0};
sem_t testSem = {0};
wftype callCopy = 0;
nftype callBack = 0;
vftype callFork = 0;
zftype callInfo = 0;
zftype callJnfo = 0;
zftype callKnfo = 0;
oftype callCmdl = 0;

DECLARE_DEQUE(struct Center *,Centerq)
DECLARE_DEQUE(char *,Strq)

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
typedef float *(*planeXform)(float *mat, float *fix, float *nrm, float *org, float *cur);
float *planeMatrix(float *mat)
{
    planeXform fnc = 0; int tmp; int cfg = callInfo(ManipFixed,0,planeRcfg);
    tmp = ((1<<Slide)|(1<<Ortho)|(1<<Mouse)); if ((cfg&tmp)==tmp) fnc = planeSlideOrthoMouse;
    tmp = ((1<<Rotate)|(1<<Focal)|(1<<Mouse)); if ((cfg&tmp)==tmp) fnc = planeRotateFocalMouse;
    tmp = ((1<<Rotate)|(1<<Cursor)|(1<<Roller)); if ((cfg&tmp)==tmp) fnc = planeRotateCursorRoller;
    if (!fnc) return 0; float fix[3]; float nrm[3]; float org[2]; float cur[2];
    return fnc(identmat(mat,4),
    vectorThree(fix,PierceLeft,PierceBase,PierceDeep),
    vectorThree(nrm,NormalLeft,NormalBase,NormalDeep),
    vectorTwo(org,ClickLeft,ClickBase),
    vectorTwo(cur,ManipLeft,ManipBase));
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
int debug = 0;
float *planeWindow(float *mat)
{
    // find the matrix to keep points fixed when window moves or resizes
    float xmax = 50.0; float ymax = 50.0;
    float xmin = -50.0; float ymin = -50.0;
    float xmid = (xmax+xmin)/2.0; float ymid = (ymax+ymin)/2.0;
    physicalToScreen(&xmax,&ymax); screenToWindow(&xmax,&ymax);
    physicalToScreen(&xmin,&ymin); screenToWindow(&xmin,&ymin);
    physicalToScreen(&xmid,&ymid); screenToWindow(&xmid,&ymid);
    for (int i = 0; i < 16; i++) mat[i] = 0.0;
    *matrc(mat,0,0,4) = 1.0/(xmax-xmid); *matrc(mat,1,1,4) = 1.0/(ymax-ymid);
    *matrc(mat,0,3,4) = -xmid; *matrc(mat,1,3,4) = -ymid;
    *matrc(mat,2,2,4) = 1.0; *matrc(mat,3,3,4) = 1.0;
    /*
    printf("%d\n",debug++);
    for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++)
    printf(" %6f",*matrc(mat,i,j,4));
    printf("\n");}
    printf("\n");
    */
    return identmat(mat,4);
}

void centerSize(int idx)
{
    if (sem_wait(&copySem) != 0) ERROR();
    if (idx < 0 || idx >= callInfo(CenterSize,0,planeRcfg)) ERROR();
    if (idx >= centers) {int size = idx+1; center = realloc(center,size);
    for (int i = centers; i < size; i++) center[i] = 0; centers = size;}
    if (sem_post(&copySem) != 0) ERROR();
}
// TODO add sanity check by allowing only a few centerPull without corresponsing centerPlace
struct Center *centerPull(int idx)
{
    centerSize(idx);
    if (sem_wait(&copySem) != 0) ERROR();
    struct Center *ret = center[idx]; center[idx] = 0;
    if (sem_post(&copySem) != 0) ERROR();
    return ret;
}
void centerPlace(struct Center *ptr, int idx)
{
    centerSize(idx);
    if (sem_wait(&copySem) != 0) ERROR();
    freeCenter(center[idx]); allocCenter(&center[idx],0); center[idx] = ptr;
    if (sem_post(&copySem) != 0) ERROR();
}
void centerFree(struct Response resp) {
    if (resp.ptr) {freeCenter(resp.ptr); allocCenter(&resp.ptr,0);}
}
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
// Order of matrix application is window * project * subject * object * element * vector.
// To change X such that YX changes to ZYX, change X to Y’ZYX.
// Each matrix is product of local, towrite, written, maintain.
// User manipulates local, periodically cleared to towrite, cleared to written after echo indicated by slf.
// Others manipulate maintain as if before any towrite and after any written.
// Change between one machineFunc and another requires swapping their order.
void machineManip(int sig, int *arg)
{
    struct Center *center = machineCenter(sig,arg,ManipArgs,ManipDst,ManipDstSub);
    struct Kernel *kernel = machineKernel(center,sig,arg,ManipArgs,ManipDst,ManipDstSub);
    // manipulate local
    float mat[16]; jumpmat(kernel->local.mat,planeMatrix(mat),4);
    machinePlace(center,sig,arg,ManipArgs,ManipDst,ManipDstSub);
}
void machinePulse(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,PulseArgs,PulseSrc,PulseSrcSub);
    struct Kernel *kernel = machineKernel(src,sig,arg,PulseArgs,PulseSrc,PulseSrcSub);
    struct Center *dst = machineCenter(sig,arg,PulseArgs,PulseDst,PulseDstSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,PulseArgs,PulseDst,PulseDstSub);
    // clear local to towrite
    jumpmat(kernel->towrite.mat,kernel->local.mat,4); identmat(kernel->local.mat,4);
    // compose towrite, wrritten, maintain to matrix
    timesmat(timesmat(copymat(matrix->mat,kernel->towrite.mat,4),kernel->written.mat,4),
        kernel->maintain.mat,4);
    machinePlace(dst,sig,arg,PulseArgs,PulseDst,PulseDstSub);
    machinePlace(src,sig,arg,PulseArgs,PulseSrc,PulseSrcSub);
}
void machineSelf(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
    struct Center *dst = machineCenter(sig,arg,SelfArgs,SelfDst,SelfDstSub);
    struct Kernel *kernel = machineKernel(dst,sig,arg,SelfArgs,SelfDst,SelfDstSub);
    if (!dst->slf) ERROR();
    // TODO move portion of towrite to written indicated by portion of written times maintain that matrix is
    // TODO if towrite is technically clear, clear written to maintain
    machinePlace(dst,sig,arg,SelfArgs,SelfDst,SelfDstSub);
    machinePlace(src,sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
}
void machineOther(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,OtherArgs,OtherSrc,OtherSrcSub);
    struct Matrix *matrix = machineMatrix(src,sig,arg,OtherArgs,OtherSrc,OtherSrcSub);
    struct Center *dst = machineCenter(sig,arg,OtherArgs,OtherDst,OtherDstSub);
    struct Kernel *kernel = machineKernel(dst,sig,arg,OtherArgs,OtherDst,OtherDstSub);
    if (dst->slf) ERROR();
    // TODO change written such that written times maintain is matrix
    machinePlace(dst,sig,arg,OtherArgs,OtherDst,OtherDstSub);
    machinePlace(src,sig,arg,OtherArgs,OtherSrc,OtherSrcSub);
}
void machineSwap(int sig, int *arg)
{
    struct Center *center = machineCenter(sig,arg,SwapArgs,SwapDst,SwapDstSub);
    struct Kernel *kernel = machineKernel(center,sig,arg,SwapArgs,SwapDst,SwapDstSub);
    // TODO change manipulator
    machinePlace(center,sig,arg,SwapArgs,SwapDst,SwapDstSub);
}
void machineComp(int sig, int *arg)
{
    struct Center *src = machineCenter(sig,arg,CompArgs,CompSrc,CompSrcSub);
    struct Kernel *kernel = machineKernel(src,sig,arg,CompArgs,CompSrc,CompSrcSub);
    struct Center *dst = machineCenter(sig,arg,OtherArgs,OtherDst,OtherDstSub);
    struct Matrix *matrix = machineMatrix(dst,sig,arg,OtherArgs,OtherDst,OtherDstSub);
    // compose for draw
    timesmat(timesmat(timesmat(copymat(matrix->mat,kernel->local.mat,4),kernel->towrite.mat,4),
        kernel->written.mat,4),kernel->maintain.mat,4);
    machinePlace(dst,sig,arg,CompArgs,CompDst,CompDstSub);
    machinePlace(src,sig,arg,CompArgs,CompSrc,CompSrcSub);
}
void machineBopy(int sig, int *arg)
{
    int count = arg[BopyCount];
    if (sig != BopyArgs) ERROR();
    int src = arg[BopySrc]; int srcSub = arg[BopySrcSub];
    int dst = arg[BopyDst]; int dstSub = arg[BopyDstSub];
    struct Center *srcPtr = centerPull(src);
    struct Center *dstPtr = centerPull(dst);
    if (srcSub < 0 || srcSub >= srcPtr->siz) ERROR();
    if (dstSub < 0 || dstSub >= dstPtr->siz) ERROR();
    if (srcPtr->mem == Kernelz && dstPtr->mem == Matrixz) for (int i = 0; i < count; i++)
    copyMatrix(&dstPtr->mat[dstSub],&srcPtr->ker[srcSub].compose);
    else if (srcPtr->mem == Matrixz && dstPtr->mem == Kernelz) for (int i = 0; i < count; i++)
    copyMatrix(&dstPtr->ker[dstSub].local,&srcPtr->mat[srcSub]);
    else if (srcPtr->mem == dstPtr->mem) switch (srcPtr->mem) {default: ERROR();
    case (Indexz): dstPtr->ind[dstSub] = srcPtr->ind[srcSub]; break;
    case (Trianglez): copyTriangle(&dstPtr->tri[dstSub],&srcPtr->tri[srcSub]); break;
    case (Numericz): copyNumeric(&dstPtr->num[dstSub],&srcPtr->num[srcSub]); break;
    case (Vertexz): copyVertex(&dstPtr->vtx[dstSub],&srcPtr->vtx[srcSub]); break;
    case (Basisz): copyBasis(&dstPtr->bas[dstSub],&srcPtr->bas[srcSub]); break;
    case (Matrixz): copyMatrix(&dstPtr->mat[dstSub],&srcPtr->mat[srcSub]); break;
    case (Texturez): copyTexture(&dstPtr->tex[dstSub],&srcPtr->tex[srcSub]); break;
    case (Piercez): copyPierce(&dstPtr->pie[dstSub],&srcPtr->pie[srcSub]); break;
    case (Stringz): assignStr(&dstPtr->str[dstSub],srcPtr->str[srcSub]); break;
    case (Machinez): copyMachine(&dstPtr->mch[dstSub],&srcPtr->mch[srcSub]); break;
    case (Kernelz): copyKernel(&dstPtr->ker[dstSub],&srcPtr->ker[srcSub]); break;}
    else ERROR();
    machinePlace(srcPtr,sig,arg,BopyArgs,BopySrc,BopySrcSub);
    machinePlace(dstPtr,sig,arg,BopyArgs,BopyDst,BopyDstSub);
}
void machineResp(struct Response resp)
{
    centerPlace(resp.ptr,resp.idx);
}
void machineCopy(int sig, int *arg)
{
    if (sig != CopyArgs) ERROR();
    int src = arg[CopySrc];
    struct Center *ptr = centerPull(src);
    struct Response resp = {0,1,src,ptr,machineResp};
    callCopy(resp);
}
void machineDopy(int sig, int *arg)
{
    if (sig != DopyArgs) ERROR();
    int src = arg[DopySrc];
    struct Center *ptr = centerPull(src);
    struct Response resp = {0,0,src,ptr,machineResp};
    callCopy(resp);
}
void machinePopy(int sig, int *arg)
{
    if (sig != PopyArgs) ERROR();
    int dst = arg[PopyDst];
    if (sem_wait(&pipeSem) != 0) ERROR();
    struct Center *ptr = maybeCenterq(0,internal);
    if (sem_post(&pipeSem) != 0) ERROR();
    centerPlace(ptr,dst);
}
void machineQopy(int sig, int *arg)
{
    if (sig != QopyArgs) ERROR();
    int src = arg[QopySrc];
    struct Center *ptr = centerPull(src);
    if (sem_wait(&pipeSem) != 0) ERROR();
    pushCenterq(ptr,response);    
    if (sem_post(&pipeSem) != 0) ERROR();
}
char *planeGetstr()
{
    char *str = malloc(strlen(frontStrq(strin))+1);
    strcpy(str,frontStrq(strin));
    if (sem_wait(&stdioSem) != 0) ERROR();
    popStrq(strin);
    if (sem_post(&stdioSem) != 0) ERROR();
    return str;
}
void planePutstr(const char *src)
{
    char *str = malloc(strlen(src)+1);
    strcpy(str,src);
    if (sem_wait(&stdioSem) != 0) ERROR();
    pushStrq(str,strout);
    if (sem_post(&stdioSem) != 0) ERROR();
}
void planeSetcfg(int val, int sub)
{
    callJnfo((enum Configure)sub,val,planeWcfg);
}
int planeRetcfg(int sub)
{
    return callInfo((enum Configure)sub,0,planeRcfg);
}
void machineStage(enum Configure cfg, int idx)
{
    struct Center *ptr = centerPull(idx);
    switch (cfg) {default: ERROR();
    case (CenterMem): callJnfo(cfg,ptr->mem,planeWcfg); break;
    case (CenterSiz): callJnfo(cfg,ptr->siz,planeWcfg); break;
    case (CenterIdx): callJnfo(cfg,ptr->idx,planeWcfg); break;
    case (CenterSlf): callJnfo(cfg,ptr->slf,planeWcfg); break;}
    centerPlace(ptr,idx);
}
void machineTsage(enum Configure cfg, int idx)
{
    struct Center *ptr = centerPull(idx);
    switch (cfg) {default: ERROR();
    case (CenterMem): ptr->mem = callInfo(cfg,0,planeRcfg); break;
    case (CenterSiz): ptr->siz = callInfo(cfg,0,planeRcfg); break;
    case (CenterIdx): ptr->idx = callInfo(cfg,0,planeRcfg); break;
    case (CenterSlf): ptr->slf = callInfo(cfg,0,planeRcfg); break;}
    centerPlace(ptr,idx);
}
void machineEval(struct Express *exp, int idx)
{
    void *dat = 0; struct Center *cptr = 0;
    if (datxEval(&dat,exp,identType("Center")) != identType("Center")) ERROR();
    if (sem_wait(&dataSem) != 0) ERROR();
    assignDat(dat0,dat); readCenter(cptr,sub0);
    if (sem_post(&dataSem) != 0) ERROR();
    centerPlace(cptr,idx);
}
int machineIval(struct Express *exp)
{
    void *dat = 0; int val = 0; int typ = 0;
    typ = datxEval(&dat,exp,identType("Int"));
    if (typ != identType("Int")) ERROR();
    val = *datxIntz(0,dat); free(dat);
    return val;
}
struct Machine *machineNext(struct Center *current, int next)
{
    if (next < 0 || next >= current->siz) ERROR(); return &current->mch[next];
}
int machineEscape(struct Center *current, int level, int next)
{
    int inc = (level > 0 ? 1 : (level == 0 ? 0 : -1)); level *= inc;
    for (next += inc; level > 0; next += inc) {
    struct Machine *mptr = machineNext(current,next);
    if (!mptr) break;
    if (mptr->xfr == Nest) level += mptr->lvl*inc;}
    return next;
}
void machineSwitch(struct Machine *mptr)
{
    if (!mptr) ERROR();
    switch (mptr->xfr) {default: ERROR();
    case (Stage): for (int i = 0; i < mptr->siz; i++) machineStage(mptr->sav[i],mptr->idx); break;
    case (Tsage): for (int i = 0; i < mptr->siz; i++) machineTsage(mptr->sav[i],mptr->idx); break;
    case (Force): for (int i = 0; i < mptr->num; i++) callJnfo(mptr->cfg[i],mptr->val[i],planeWcfg); break;
    case (Eval): machineEval(&mptr->fnc[0],mptr->res); break;
    case (Manip): machineManip(mptr->sig,mptr->arg); break;
    case (Pulse): machinePulse(mptr->sig,mptr->arg); break;
    case (Self): machineSelf(mptr->sig,mptr->arg); break;
    case (Other): machineOther(mptr->sig,mptr->arg); break;
    case (Swap): machineSwap(mptr->sig,mptr->arg); break;
    case (Comp): machineComp(mptr->sig,mptr->arg); break;
    case (Bopy): machineBopy(mptr->sig,mptr->arg); break;
    case (Copy): machineCopy(mptr->sig,mptr->arg); break;
    case (Dopy): machineDopy(mptr->sig,mptr->arg); break;
    case (Popy): machinePopy(mptr->sig,mptr->arg); break;
    case (Qopy): machineQopy(mptr->sig,mptr->arg); break;}
}
void planeMachine(enum Thread tag, int idx)
{
    while (1) {
    int index = callInfo(CenterIndex,0,planeRcfg);
    if (index < 0) break;
    struct Center *current = centerPull(index);
    int last = callInfo(MachineIndex,0,planeRcfg)-1;
    for (int next = last+1; next != last; last = ++next) {
    struct Machine *mptr = machineNext(current,next);
    // {char *opr = 0; showTransfer(mptr->xfr,&opr);
    // printf("planeMachine %s\n",opr); free(opr);}
    switch (mptr->xfr) {default: machineSwitch(mptr); break;
    case (Jump): next = machineEscape(current,machineIval(&mptr->exp[0]),next) - 1; break;
    case (Goto): next = next + machineIval(&mptr->exp[0]) - 1; break;
    case (Nest): break;}}
    centerPlace(current,index);
    if (sem_wait(&waitSem) != 0) ERROR();}
}

void planeSelect(enum Thread tag, int idx)
{
    if ((external = argument.idx = rdwrInit(argument.inp,argument.out)) < 0) ERROR();
    while (1) {
    int sub = waitRead(0,(1<<external)|(1<<wakeup));
    if (sub == wakeup) {
    struct Center *center = 0;
    if (!checkRead(wakeup)) break;
    readInt(wakeup);
    if (sem_wait(&pipeSem) != 0) ERROR();
    center = maybeCenterq(0,response);
    if (sem_post(&pipeSem) != 0) ERROR();
    writeCenter(center,external);
    freeCenter(center);
    allocCenter(&center,0);}
    else if (sub == external) {
    struct Center *center = 0;
    if (!checkRead(external)) break;
    allocCenter(&center,1);
    readCenter(center,external);
    if (sem_wait(&pipeSem) != 0) ERROR();
    pushCenterq(center,internal);
    if (sem_post(&pipeSem) != 0) ERROR();}
    else break;}
}

void planeConsole(enum Thread tag, int idx)
{
    char chr[2] = {0};
    int val = 0;
    int nfd = 0;
    char *str = 0;
    fd_set fds, ers;
    while (1) {
    FD_ZERO(&fds); FD_ZERO(&ers); nfd = 0;
    if (nfd <= STDIN_FILENO) nfd = STDIN_FILENO+1;
    FD_SET(STDIN_FILENO,&fds); FD_SET(STDIN_FILENO,&ers);
    val = pselect(nfd,&fds,0,&ers,0,0);
    if (val < 0 && errno == EINTR) continue;
    if (val < 0 && errno == EBADF) break;
    if (val == 0) break;
    if (val < 0) ERROR();
    val = read(STDIN_FILENO,chr,1);
    if (val == 0) break;
    if (val < 0) ERROR();
    // TODO hide string push to string queue and wakeup machine thread
    }
}

void registerClose(enum Thread tag, int idx)
{
    callJnfo(RegisterOpen,(1<<tag),planeWotc);
}
void registerOpen(enum Configure cfg, int sav, int val)
{
    if (cfg != RegisterOpen) ERROR();
    if ((val & (1<<PipeThd)) && !(sav & (1<<PipeThd))) {
        callFork(PipeThd,0,planeSelect,registerClose);}
    if (!(val & (1<<PipeThd)) && (sav & (1<<PipeThd))) {
        closeIdent(external); closeIdent(wakeup);}
    if ((val & (1<<PipeThd)) && (sav & (1<<PipeThd))) {
        writeInt(wakeup,0);}
    if ((val & (1<<StdioThd)) && !(sav & (1<<StdioThd))) {
        callFork(StdioThd,0,planeConsole,registerClose);}
    if (!(val & (1<<StdioThd)) && (sav & (1<<StdioThd))) {
        close(STDIN_FILENO); close(STDOUT_FILENO);}
    if ((val & (1<<StdioThd)) && (sav & (1<<StdioThd))) {
        /*TODO no wakeup for console thread*/}
    if ((val & (1<<CopyThd)) && !(sav & (1<<CopyThd))) {
        callFork(CopyThd,0,planeMachine,registerClose);}
    if (!(val & (1<<CopyThd)) && (sav & (1<<CopyThd))) {
        callKnfo(CenterIndex,-1,planeWcfg);
        if (sem_post(&waitSem) != 0) ERROR();}
    if ((val & (1<<CopyThd)) && (sav & (1<<CopyThd))) {
        if (sem_post(&waitSem) != 0) ERROR();}
}
void registerMask(enum Configure cfg, int sav, int val)
{
    if (cfg != RegisterMask) ERROR();
    if (callKnfo(RegisterOpen,0,planeRcfg) & (1<<CopyThd)) {
        callKnfo(RegisterOpen,(1<<CopyThd),planeWots);}
}

void initSafe()
{
    if (sem_init(&waitSem, 0, 0) != 0) ERROR();
    if (sem_init(&copySem, 0, 1) != 0) ERROR();
    if (sem_init(&pipeSem, 0, 1) != 0) ERROR();
    if (sem_init(&stdioSem, 0, 1) != 0) ERROR();
    if (sem_init(&testSem, 0, 1) != 0) ERROR();
    if (sem_init(&dataSem, 0, 1) != 0) ERROR();
    sub0 = datxSub(); idx0 = puntInit(sub0,sub0,datxReadFp,datxWriteFp); dat0 = datxDat(sub0);
    internal = allocCenterq(); response = allocCenterq();
    strout = allocStrq(); strin = allocStrq();
    callBack(RegisterOpen,registerOpen);
    callBack(RegisterMask,registerMask);
}
void planeDone()
{
    callBack(RegisterMask,0);
    callBack(RegisterOpen,0);
    freeStrq(strin); freeStrq(strout);
    freeCenterq(response); freeCenterq(internal);
    closeIdent(idx0); datxNon();
    if (sem_destroy(&dataSem) != 0) ERROR();
    if (sem_destroy(&testSem) != 0) ERROR();
    if (sem_destroy(&stdioSem) != 0) ERROR();
    if (sem_destroy(&pipeSem) != 0) ERROR();
    if (sem_destroy(&copySem) != 0) ERROR();
    if (sem_destroy(&waitSem) != 0) ERROR();
}
void initBoot()
{
    int size = 0; for (int i = 0; callCmdl(i); i++) size++;
    for (int i = 0; Bootstrap__Int__Str(i); i++) size++;
    const char **boot = malloc(size*sizeof(const char *)); size = 0;
    for (int i = 0; callCmdl(i); i++) boot[size++] = callCmdl(i);
    for (int i = 0; Bootstrap__Int__Str(i); i++) boot[size++] = Bootstrap__Int__Str(i);
    for (int i = 0; i < size; i++) {int asiz = 0; int csiz = 0; int msiz = 0;
    struct Argument arg = {0}; struct Center cntr = {0}; struct Machine mchn = {0};
    if (hideArgument(&arg, boot[i], &asiz)) {
    copyArgument(&argument,&arg); freeArgument(&arg);}
    else if (hideCenter(&cntr, boot[i], &csiz)) {struct Center *ptr = 0;
    allocCenter(&ptr,1); copyCenter(ptr,&cntr); freeCenter(&cntr); centerPlace(ptr,centers);}
    else if (hideMachine(&mchn, boot[i], &msiz)) {
    machineSwitch(&mchn); freeMachine(&mchn);}
    else {fprintf(stderr,"Argument:%d Center:%d Machine:%d unmatched:%s\n",asiz,csiz,msiz,boot[i]); exit(-1);}}
}
void initPlan()
{
    switch (callInfo(RegisterPlan,0,planeRcfg)) {default: ERROR();
    break; case (Bringup): {
    callJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    callJnfo(RegisterOpen,(1<<TestThd),planeWots);}
    }
}
void planeLoop()
{
    switch (callInfo(RegisterPlan,0,planeRcfg)) {default: ERROR();
    break; case (Bringup): {
    if (callJnfo(RegisterCount,1,planeRmw) < 1000) {callJnfo(RegisterOpen,(1<<TestThd),planeWots); return;}
    callJnfo(RegisterOpen,(1<<TestThd),planeWotc);
    callJnfo(RegisterOpen,(1<<FenceThd),planeWotc);}
    }
}
void wrapPlane();
void planeInit(wftype copy, nftype call, vftype fork, zftype info, zftype jnfo, zftype knfo, oftype cmdl)
{
    callCopy = copy;
    callBack = call;
    callFork = fork;
    callInfo = info;
    callJnfo = jnfo;
    callKnfo = knfo;
    callCmdl = cmdl;
    initSafe();
    initBoot();
    wrapPlane();
    initPlan();
}
