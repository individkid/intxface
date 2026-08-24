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

struct Extend **center = 0; // only for planeSwitch
int centers = 0; // only for planeSwitch
void *copySem = 0; // protect centers
int extdone = 0; // done for planeExternal
int external = 0; // pipes to planeExternal
int inverse[Programs] = {0}; // inverse to userIdent
void *internal = 0; // queue of center
void *response = 0; // queue of center
void *replace = 0; // queue of center
void *pipeSem = 0; // protect external inverse internal response replace
int console = 0; // pipe to planeConsole
int condone = 0; // done for planeConsole
void *strin = 0; // queue of string
void *strout = 0; // queue of string
void *stdioSem = 0; // protect strin and strout
void *maskq = 0; // map from event to thread mask
void *ableq = 0; // map from thread to vector mask
void *timeq = 0; // queue of wakeup times
void *wakeq = 0; // queue of wakeup threads
void *timep = 0; // map from thread to time
void *timeSem = 0; // protect wakeup queues
void *charq = 0; // queue of keyboard presses
void *leftq = 0; // queue of mouse presses
void *baseq = 0; // queue of mouse presses
void *angleq = 0; // queue of mouse presses
void *pressSem = 0; // protect press queues
void **wakeSem[Threads] = {0};
int sizeSem[Threads] = {0};
int *machine = 0;
int **reboot = 0;
struct Extend ***recent = 0;
int *resize = 0;
void *safeSem = 0; // protect reboot recent resize and wakeSem
// initialized before threads so safe
void *tempq = 0; // temporary queue to convert chars to str
int loopfd = 0; // pipe from one struct to another
void *loopSem = 0; // protect loopfd
void *evalSem = 0;
uftype callCopy = 0;
wftype callCont = 0;
nftype callBack = 0;
vftype callFork = 0;
zftype callGnfo = 0;
zftype callInfo = 0;
zftype callJnfo = 0;
zftype callKnfo = 0;
bftype callHnfo = 0;
oftype callCmnd = 0;
aftype callWait = 0;
aftype callWake = 0;
float start = 0.0;

DECLARE_DEQUE(struct Extend *,Centerq)
DECLARE_DEQUE(char *,Strq)
DECLARE_DEQUE(char, Chrq)
DECLARE_DEQUE(float, Timeq)
DECLARE_DEQUE(int, Intq)

DECLARE_MAP(int,float,Timep)

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
int planeGnfo(enum Configure cfg, int val, yftype fnc)
{
    callGnfo(&cfg,&val,1,fnc); return val;
}
int planeInfo(enum Configure cfg, int val, yftype fnc)
{
    callInfo(&cfg,&val,1,fnc); return val;
}
int planeJnfo(enum Configure cfg, int val, yftype fnc)
{
    callJnfo(&cfg,&val,1,fnc); return val;
}
int planeKnfo(enum Configure cfg, int val, yftype fnc)
{
    callKnfo(&cfg,&val,1,fnc); return val;
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
    int **temr = malloc(sizeof(int*)*siz);
    struct Extend ***tems = malloc(sizeof(struct Extend**)*siz);
    int *temt = malloc(sizeof(int)*siz);
    for (int i = 0; i < sizeSem[thd]; i++) {
    temq[i] = machine[i]; temr[i] = reboot[i]; tems[i] = recent[i]; temt[i] = resize[i];}
    for (int i = sizeSem[thd]; i < siz; i++) {
    temq[i] = -1; temr[i] = 0; tems[i] = 0; temt[i] = 0;}
    free(machine); free(reboot); free(recent); free(resize);
    machine = temq; reboot = temr; recent = tems; resize = temt;}
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
int safeFunc(void *arg)
{
    int *idx = (int*)arg;
    return (machine[*idx] < 0);
}
int safeGunc(void *arg)
{
    int *idx = (int*)arg;
    return (machine[*idx] >= 0);
}

// Transform functions find 4 independent vectors to invert, and 4 to multiply;
float *planeVector(float *vec, enum Configure left, enum Configure base, enum Configure deep)
{
    enum Configure cfg[3] = {left,base,deep}; int val[3] = {0,0,0};
    callInfo(cfg,val,3,planeRcfg);
    for (int i = 0; i < 3; i++) vec[i] = val[i] / 1000.0;
    return vec;
}
float *planeTransform(float *mat, float *src0, float *dst0, float *src1, float *dst1,
    float *src2, float *dst2, float *src3, float *dst3)
{
    float src[16]; float dst[16]; float inv[16];
    copyvec(src,src0,4); copyvec(src+4,src1,4); copyvec(src+8,src2,4); copyvec(src+12,src3,4);
    copyvec(dst,dst0,4); copyvec(dst+4,dst1,4); copyvec(dst+8,dst2,4); copyvec(dst+12,dst3,4);
    invmat(copymat(inv,src,4),4);
    return copymat(mat,timesmat(dst,inv,4),4);
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
    float v[4]; zerovec(v,4); v[0] = cur[0]-org[0]; v[1] = cur[1]-org[1];
    float h0[4], h1[4]; zerovec(h0,3); h0[3] = 1.0; plusvec(copyvec(h1,h0,4),v,4);
    float i0[4], i1[4]; unitvec(i0,3,0); i0[3] = 1.0; plusvec(copyvec(i1,i0,4),v,4);
    float j0[4], j1[4]; unitvec(j0,3,1); j0[3] = 1.0; plusvec(copyvec(j1,j0,4),v,4);
    float k0[4], k1[4]; unitvec(k0,3,2); k0[3] = 1.0; plusvec(copyvec(k1,k0,4),v,4);
    return planeTransform(mat,h0,h1,i0,i1,j0,j1,k0,k1);
}
typedef float *(*planeXform)(float *mat, float *fix, float *nrm, float *org, float *cur);
float *planeMatrix(float *mat)
{
    planeXform fnc = 0; int tmp; int cfg = planeInfo(ManipFixed,0,planeRcfg);
    tmp = ((1<<Slide)|(1<<Ortho)|(1<<Mouse)); if ((cfg&tmp)==tmp) fnc = planeSlideOrthoMouse;
    tmp = ((1<<Rotate)|(1<<Focal)|(1<<Mouse)); if ((cfg&tmp)==tmp) fnc = planeRotateFocalMouse;
    tmp = ((1<<Rotate)|(1<<Cursor)|(1<<Roller)); if ((cfg&tmp)==tmp) fnc = planeRotateCursorRoller;
    if (!fnc) return identmat(mat,4);
    float fix[3]; float nrm[3]; float org[3]; float cur[3];
    return fnc(identmat(mat,4),
    planeVector(fix,FixedLeft,FixedBase,FixedDeep),
    planeVector(nrm,NormalLeft,NormalBase,NormalDeep),
    planeVector(org,ClickLeft,ClickBase,ClickAngle),
    planeVector(cur,ManipLeft,ManipBase,ManipAngle));
}
float *planeWindow(float *mat)
{
    identmat(mat,4);
    enum Configure cfg[2] = {UniformWid,UniformHei};
    int val[2] = {0,0};
    callInfo(cfg,val,2,planeRcfg);
    float width = val[0];
    float height = val[1];
    *matrc(mat,3,2,4) = 0.83; // b; // row major; row number 3; column number 2
    *matrc(mat,3,3,4) = 0.58; // a; // w = a + bz
    *matrc(mat,0,0,4) = height/width; // y'=y x'=x*height/width
    return mat;
}

// resource accessors
void centerSize(int idx);
int centerFunc(void *arg);
struct Extend *centerPull(int idx, const char *log)
{
    centerSize(idx);
    if (funcSafe(copySem,centerFunc,center+idx) < 0) ERROR();
    struct Extend *ret = center[idx];
    deleteSmart(ret->log); ret->log = otherSmart(planeInfo(CenterLog,0,planeRcfg));
    center[idx] = 0;
    if (postSafe(copySem) != 1) ERROR();
    printfSmart(ret->log,"Pull %d %s",idx,log);
    if (ret->asr != PlaceAsr) ERROR(); else ret->asr = PullAsr;
    return ret;
}
struct Extend *centerPeek(int idx, const char *log)
{
    centerSize(idx);
    if (waitSafe(copySem) != 0) ERROR();
    struct Extend *ret = center[idx];
    center[idx] = 0;
    if (postSafe(copySem) != 1) ERROR();
    // if (ret) printfSmart(ret->log,"Peek %d %s",idx,log);
    if (ret != 0 && ret->asr != PlaceAsr) ERROR(); else if (ret != 0) ret->asr = PullAsr;
    return ret;
}
void centerFree(int idx, const char *log);
void centerPlace(struct Extend *ptr)
{
    if (ptr == 0) return;
    if (ptr->asr != PullAsr) ERROR(); else ptr->asr = PlaceAsr;
    // printfSmart(ptr->log,"Place %d",ptr->sub);
    centerSize(ptr->sub);
    centerFree(ptr->sub,"Place");
    if (waitSafe(copySem) != 0) ERROR();
    if (center[ptr->sub] != 0) ERROR();
    center[ptr->sub] = ptr;
    if (postSafe(copySem) != 1) ERROR();
}
void centerClear(int sub)
{
    centerSize(sub);
    centerFree(sub,"Clear");
}
void centerDone(struct Extend *ptr)
{
    if (ptr->asr != PullAsr) ERROR(); else ptr->asr = DoneAsr;
    printfSmart(ptr->log,"Done %d",ptr->sub);
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(ptr,replace);
    if (postSafe(pipeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<DoneMsk),planeWots);
}
int centerCheck(int idx)
{
    centerSize(idx);
    if (center[idx] != 0 && center[idx]->asr != PlaceAsr) ERROR();
    if (waitSafe(copySem) != 0) ERROR();
    int ret = (center[idx] != 0);
    if (postSafe(copySem) != 1) ERROR();
    return ret;
}
int centerMod(struct Extend *ptr)
{
    switch (ptr->ptr->mem) {default: ERROR();
    break; case (Indexz): return sizeof(int32_t);
    break; case (Storagez): return sizeof(int32_t);
    break; case (Bringupz): return sizeof(struct Vertex);
    break; case (Identz): return sizeof(int32_t);
    break; case (Uniformz): return sizeof(struct Uniform);
    break; case (Matrixz): return sizeof(struct Matrix);
    break; case (Trianglez): return sizeof(struct Triangle);
    break; case (Numericz): return sizeof(struct Numeric);
    break; case (Vertexz): return sizeof(struct Vertex);
    break; case (Basisz): return sizeof(struct Basis);}
    return 0;
}
struct InitCenter {
    int sdx, ddx, siz, tot;
    struct Extend *src;
    struct Extend *sav;
    struct Extend *dst;
};
enum DatxEnum centerField(int num, int fld, int sub, int typ, struct DatxField *arg);
enum DatxEnum centerElem(int num, int fld, int sub, int typ, struct DatxField *arg);
void centerInit(struct Extend *src, struct Extend *dst)
{
    dst->src = src->src;
    dst->sub = src->sub;
    dst->sav = src->sav;
    dst->rsp = src->rsp;
    dst->ret = src->ret;
    if (src->log) dst->log = otherSmart(src->log); else dst->log = 0;
}
void centerResize(struct Extend **ptr, int siz)
{
    struct InitCenter init = {0,0,siz,0,*ptr,0,0};
    allocExtend(&init.dst,1);
    centerInit(*ptr,init.dst);
    initCenter(init.dst->ptr,centerField,&init);
    if ((*ptr) != 0) deleteSmart((*ptr)->log);
    freeExtend(*ptr); allocExtend(ptr,0);
    *ptr = init.dst;
}
void centerMerge(struct Extend *src, struct Extend **dst, int sdx, int ddx, int siz)
{
    struct InitCenter init = {sdx,ddx,siz,(*dst)->ptr->siz+siz,src,*dst,0};
    allocExtend(&init.dst,1);
    centerInit(*dst,init.dst);
    initCenter(init.dst->ptr,centerElem,&init);
    if ((*dst) != 0) deleteSmart((*dst)->log);
    freeExtend(*dst); allocExtend(dst,0);
    *dst = init.dst;
}
void centerSize(int idx)
{
    if (waitSafe(copySem) != 0) ERROR();
    if (idx < 0) ERROR();
    if (idx >= centers) {int size = idx+1; center = realloc(center,size*sizeof(struct Extend *));
    for (int i = centers; i < size; i++) center[i] = 0; centers = size;}
    if (postSafe(copySem) != 1) ERROR();
}
void centerFree(int idx, const char *log)
{
    centerSize(idx);
    struct Extend *ptr = centerPeek(idx,log);
    if (ptr == 0) return;
    if (ptr->asr != PullAsr) ERROR(); else ptr->asr = Asserts;
    deleteSmart(ptr->log);
    freeExtend(ptr);
    allocExtend(&ptr,0);
}
int centerFunc(void *arg)
{
    struct Extend **center = (struct Extend **)arg;
    return (*center != 0);
}

// machine extensions
struct Extend *machineCenter(int sig, int *arg, int lim, int idx, int sub, const char *log)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    struct Extend *srcPtr = centerPull(src,log);
    if (srcPtr->sub != src) ERROR();
    if (srcSub < 0 || srcSub >= srcPtr->ptr->siz) ERROR();
    return srcPtr;
}
struct Kernel *machineKernel(struct Extend *ptr, int sig, int *arg, int lim, int idx, int sub)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    if (srcSub < 0 || srcSub >= ptr->ptr->siz) ERROR();
    if (ptr->ptr->mem != Kernelz) ERROR();
    return &ptr->ptr->ker[srcSub];
}
struct Matrix *machineMatrix(struct Extend *ptr, int sig, int *arg, int lim, int idx, int sub)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    if (srcSub < 0 || srcSub >= ptr->ptr->siz) ERROR();
    if (ptr->ptr->mem != Matrixz) ERROR();
    return &ptr->ptr->mat[srcSub];
}
struct Menu *machineMenu(struct Extend *ptr, int sig, int *arg, int lim, int idx, int sub)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    if (srcSub < 0 || srcSub >= ptr->ptr->siz) ERROR();
    if (ptr->ptr->mem != Menuz) ERROR();
    return &ptr->ptr->men[srcSub];
}
void machinePlace(struct Extend *ptr, int sig, int *arg, int lim, int idx, int sub)
{
    if (sig != lim) ERROR();
    int src = arg[idx];
    int srcSub = arg[sub];
    if (ptr->sub != src) ERROR();
    if (srcSub < 0 || srcSub >= ptr->ptr->siz) ERROR();
    centerPlace(ptr);
}

// manipulation C
// Kernel.saved T
// Kernel.local L
// Kernel.sent S
// Kernal.global G
// Matrix M
// upon pipe from other, cursor, or roller, send machineComp to gpu
// upon change between cursor and roller, or change to manipulation mode, call machineForm
// periodically send machineSend to pipe
// upon pipe back from self call machineSelf on it
// upon pipe from other call machineGlob on it
// T goes to I without changing Comp, when C is I upon Form
// L goes to I without changing Comp, upon Send
// S goes to I without changing Comp, upon last outstanding Self
void machineProj(int sig, int *arg)
{
    if (sig != ProjArgs) ERROR();
    struct Extend *dst = machineCenter(sig,arg,ProjArgs,ProjDst,ProjDstSub,"Proj");
    struct Matrix *matrix = machineMatrix(dst,sig,arg,ProjArgs,ProjDst,ProjDstSub);
    planeWindow(matrix->mat);
    machinePlace(dst,sig,arg,ProjArgs,ProjDst,ProjDstSub);
}
void machineBnry(int sig, int *arg)
{
    if (sig != ProjArgs) ERROR();
    struct Extend *lft = machineCenter(sig,arg,BnryArgs,BnryLft,BnryLftSub,"Proj");
    struct Matrix *mft = machineMatrix(lft,sig,arg,BnryArgs,BnryLft,BnryLftSub);
    struct Extend *rgt = machineCenter(sig,arg,BnryArgs,BnryRgt,BnryRgtSub,"Proj");
    struct Matrix *mgt = machineMatrix(rgt,sig,arg,BnryArgs,BnryRgt,BnryRgtSub);
    struct Extend *dst = machineCenter(sig,arg,BnryArgs,BnryDst,BnryDstSub,"Proj");
    struct Matrix *mst = machineMatrix(dst,sig,arg,BnryArgs,BnryDst,BnryDstSub);
    float *fft = mft->mat; float *fgt = mgt->mat; float *fst = mst->mat;
    planeTransform(fst,fft+0,fgt+0,fft+4,fgt+4,fft+8,fgt+8,fft+12,fgt+12);
    machinePlace(lft,sig,arg,BnryArgs,BnryLft,BnryLftSub);
    machinePlace(rgt,sig,arg,BnryArgs,BnryRgt,BnryRgtSub);
    machinePlace(dst,sig,arg,BnryArgs,BnryDst,BnryDstSub);
}
void machineComp(int sig, int *arg)
{
    if (sig != CompArgs) ERROR();
    struct Extend *src = machineCenter(sig,arg,CompArgs,CompSrc,CompSrcSub,"Comp");
    struct Kernel *kernel = machineKernel(src,sig,arg,CompArgs,CompSrc,CompSrcSub);
    struct Extend *dst = machineCenter(sig,arg,CompArgs,CompDst,CompDstSub,"Comp");
    struct Matrix *matrix = machineMatrix(dst,sig,arg,CompArgs,CompDst,CompDstSub);
    // compose for draw -- T = C; M = GSLT
    float mat[16]; copymat(kernel->saved.mat,planeMatrix(mat),4); // T = C
    timesmat(timesmat(timesmat(copymat(matrix->mat,kernel->global.mat,4),kernel->sent.mat,4),kernel->local.mat,4),kernel->saved.mat,4); // M = GSLT
    machinePlace(src,sig,arg,CompArgs,CompSrc,CompSrcSub);
    machinePlace(dst,sig,arg,CompArgs,CompDst,CompDstSub);
}
void machineForm(int sig, int *arg)
{
    if (sig != FormArgs) ERROR();
    struct Extend *center = machineCenter(sig,arg,FormArgs,FormSrc,FormSrcSub,"Form");
    struct Kernel *kernel = machineKernel(center,sig,arg,FormArgs,FormSrc,FormSrcSub);
    // change manipulation matrix -- L = LTC'; T = C
    float mat[16]; float inv[16]; invmat(copymat(inv,planeMatrix(mat),4),4);
    timesmat(timesmat(kernel->local.mat,kernel->saved.mat,4),inv,4); // L = LTC'
    copymat(kernel->saved.mat,mat,4); // T = C
    machinePlace(center,sig,arg,FormArgs,FormSrc,FormSrcSub);
}
void machineSend(int sig, int *arg)
{
    if (sig != SendArgs) ERROR();
    struct Extend *src = machineCenter(sig,arg,SendArgs,SendSrc,SendSrcSub,"Send");
    struct Kernel *kernel = machineKernel(src,sig,arg,SendArgs,SendSrc,SendSrcSub);
    struct Extend *dst = machineCenter(sig,arg,SendArgs,SendDst,SendDstSub,"Send");
    struct Matrix *matrix = machineMatrix(dst,sig,arg,SendArgs,SendDst,SendDstSub);
    // move local to sent -- T = C; M = L; S = SL; L = I
    float mat[16]; copymat(kernel->saved.mat,planeMatrix(mat),4); // T = C
    copymat(matrix->mat,kernel->local.mat,4); // M = L
    timesmat(kernel->sent.mat,kernel->local.mat,4); // S = SL
    identmat(kernel->local.mat,4); // L = I
    machinePlace(src,sig,arg,SendArgs,SendSrc,SendSrcSub);
    machinePlace(dst,sig,arg,SendArgs,SendDst,SendDstSub);
}
void machineSelf(int sig, int *arg)
{
    if (sig != SelfArgs) ERROR();
    struct Extend *src = machineCenter(sig,arg,SelfArgs,SelfSrc,SelfSrcSub,"Self");
    struct Matrix *matrix = machineMatrix(src,sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
    struct Extend *dst = machineCenter(sig,arg,SelfArgs,SelfDst,SelfDstSub,"Self");
    struct Kernel *kernel = machineKernel(dst,sig,arg,SelfArgs,SelfDst,SelfDstSub);
    // move portion of sent to global -- G = GM; S = M'S
    timesmat(kernel->global.mat,matrix->mat,4); // G = GM
    float inv[16]; jumpmat(kernel->sent.mat,invmat(copymat(inv,matrix->mat,4),4),4); // S = M'S
    machinePlace(src,sig,arg,SelfArgs,SelfSrc,SelfSrcSub);
    machinePlace(dst,sig,arg,SelfArgs,SelfDst,SelfDstSub);
}
void machineGlob(int sig, int *arg)
{
    if (sig != GlobArgs) ERROR();
    struct Extend *src = machineCenter(sig,arg,GlobArgs,GlobSrc,GlobSrcSub,"Glob");
    struct Matrix *matrix = machineMatrix(src,sig,arg,GlobArgs,GlobSrc,GlobSrcSub);
    struct Extend *dst = machineCenter(sig,arg,GlobArgs,GlobDst,GlobDstSub,"Glob");
    struct Kernel *kernel = machineKernel(dst,sig,arg,GlobArgs,GlobDst,GlobDstSub);
    // absorb discontinuous change -- G = GM
    timesmat(kernel->global.mat,matrix->mat,4); // G = GM
    machinePlace(src,sig,arg,GlobArgs,GlobSrc,GlobSrcSub);
    machinePlace(dst,sig,arg,GlobArgs,GlobDst,GlobDstSub);
}
void machineBopy(int sig, int *arg)
{
    if (sig != BopyArgs) ERROR();
    int src = arg[BopySrc];
    int alt = arg[BopyAlt];
    struct Extend *ext = centerPull(src,"Bopy");
    callCont(ext,alt,ext->log);
}
void machineExec(int idx, struct Extend *ext);
void machineCopy(int sig, int *arg)
{
    if (sig != CopyArgs) ERROR();
    int src = arg[CopySrc];
    int idx = arg[CopyThd];
    struct Extend *ext = centerPull(src,"Copy");
    machineExec(idx,ext);
    centerPlace(ext);
}
void machineDopy(int sig, int *arg)
{
    if (sig != DopyArgs) ERROR();
    int src = arg[DopySrc];
    int dst = arg[DopyDst];
    struct Extend *cpy = 0; allocExtend(&cpy,1);
    struct Extend *ptr = centerPull(src,"Dopy");
    copyExtend(cpy,ptr);
    cpy->sub = dst; cpy->log = otherSmart(ptr->log);
    centerPlace(ptr);
    centerPlace(cpy);
}
void machineMopy(int sig, int *arg)
{
    if (sig != MopyArgs) ERROR();
    int srcSub = arg[MopySrc];
    int srcOfs = arg[MopySrcSub];
    int dstSub = arg[MopyDst];
    int dstOfs = arg[MopyDstSub];
    int siz = arg[MopySiz];
    struct Extend *src = centerPull(srcSub,"Mopy");
    struct Extend *dst = centerPull(dstSub,"Mopy");
    centerMerge(src,&dst,srcOfs,dstOfs,siz);
    centerPlace(src);
    centerPlace(dst);
}
void machinePop(int sig, int chk, int dst, void *que);
void machinePopy(int sig, int *arg)
{
    machinePop(sig,PopyArgs,arg[PopyDst],internal);
}
void machineQopy(int sig, int *arg)
{
    if (sig != QopyArgs) ERROR();
    int src = arg[QopySrc];
    struct Extend *ptr = centerPull(src,"Qopy");
    if (ptr->asr != PullAsr) ERROR(); else ptr->asr = RespAsr;
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(ptr,response);
    if (postSafe(pipeSem) != 1) ERROR();
    postSafe(safeSafe(PipeThd,0));
}
void machineRopy(int sig, int *arg)
{
    machinePop(sig,RopyArgs,arg[RopyDst],replace);
}
void machineStage(enum Configure *cfg, int siz, int idx)
{
    centerSize(idx);
    struct Extend *ext = centerPeek(idx,"Stage");
    struct Center *ptr = (ext?ext->ptr:0);
    struct Metric *met = (ptr&&ptr->mem==Metricz?ptr->met:0);
    for (int i = 0; i < siz; i++) switch (cfg[i]) {default: ERROR();
    case (CenterMem): planeJnfo(cfg[i],(ptr?ptr->mem:0),planeWcfg); break;
    case (CenterSiz): planeJnfo(cfg[i],(ptr?ptr->siz:0),planeWcfg); break;
    case (CenterIdx): planeJnfo(cfg[i],(ptr?ptr->idx:0),planeWcfg); break;
    case (CenterSlf): planeJnfo(cfg[i],(ptr?ptr->slf:0),planeWcfg); break;
    case (CenterInt): planeJnfo(cfg[i],(ptr?ptr->idx:0),planeWcfg); break;
    case (CenterPtr): planeJnfo(cfg[i],(ext!=0),planeWcfg); break;
    case (CenterSrc): planeJnfo(cfg[i],(ext?ext->src:0),planeWcfg); break;
    case (CenterRsp): planeJnfo(cfg[i],(ext?ext->rsp:0),planeWcfg); break;
    case (CenterRet): planeJnfo(cfg[i],(ext?ext->ret:0),planeWcfg); break;
    case (CenterAsr): planeJnfo(cfg[i],(ext?ext->asr:0),planeWcfg); break;
    case (CenterSub): planeJnfo(cfg[i],(ext?ext->sub:0),planeWcfg); break;
    case (CenterSav): planeJnfo(cfg[i],(ext?ext->sav:0),planeWcfg); break;
    case (CenterLog): planeJnfo(cfg[i],(ext?ext->log:0),planeWcfg); break;
    case (FixedLeft): planeJnfo(cfg[i],(met?met->fix[0]:0),planeWcfg); break;
    case (FixedBase): planeJnfo(cfg[i],(met?met->fix[1]:0),planeWcfg); break;
    case (FixedDeep): planeJnfo(cfg[i],(met?met->fix[2]:0),planeWcfg); break;
    case (NormalLeft): planeJnfo(cfg[i],(met?met->nor[0]:0),planeWcfg); break;
    case (NormalBase): planeJnfo(cfg[i],(met?met->nor[1]:0),planeWcfg); break;
    case (NormalDeep): planeJnfo(cfg[i],(met?met->nor[2]:0),planeWcfg); break;
    case (SelectIdx): planeJnfo(cfg[i],(met?met->idx:0),planeWcfg); break;
    case (MetricAct): planeJnfo(cfg[i],(met?met->act:0),planeWcfg); break;}
    centerPlace(ext);
}
void machineTsage(enum Configure *cfg, int siz, int idx)
{
    struct Extend *ext = centerPull(idx,"Tsage");
    struct Center *ptr = (ext?ext->ptr:0);
    struct Metric *met = (ptr&&ptr->mem==Metricz?ptr->met:0);
    for (int i = 0; i < siz; i++) switch (cfg[i]) {default: ERROR();
    case (CenterMem): freeCenter(ptr); ptr->siz = 0; ptr->mem = planeInfo(cfg[i],0,planeRcfg); break;
    case (CenterSiz): {int siz = planeInfo(cfg[i],0,planeRcfg); if (siz != ptr->siz) centerResize(&ext,siz);} break;
    case (CenterIdx): ptr->idx = planeInfo(cfg[i],0,planeRcfg); break;
    case (CenterSlf): ptr->slf = planeInfo(cfg[i],0,planeRcfg); break;
    case (CenterInt): {int sub = planeInfo(cfg[i],0,planeRcfg); if (sub >= ptr->siz) centerResize(&ext,sub+1);} break;
    case (CenterPtr): ERROR();
    case (CenterSrc): ext->src = planeInfo(cfg[i],0,planeRcfg); break;
    case (CenterRsp): ext->rsp = planeInfo(cfg[i],0,planeRcfg); break;
    case (CenterRet): ext->ret = planeInfo(cfg[i],0,planeRcfg); break;
    case (CenterAsr): ERROR();
    case (CenterSub): ext->sub = planeInfo(cfg[i],0,planeRcfg); break;
    case (CenterSav): ext->sav = planeInfo(cfg[i],0,planeRcfg); break;
    case (CenterLog): if (ext->log) deleteSmart(ext->log); ext->log = otherSmart(planeInfo(cfg[i],0,planeRcfg)); break;
    case (FixedLeft): met->fix[0] = planeInfo(cfg[i],0,planeRcfg); break;
    case (FixedBase): met->fix[1] = planeInfo(cfg[i],0,planeRcfg); break;
    case (FixedDeep): met->fix[2] = planeInfo(cfg[i],0,planeRcfg); break;
    case (NormalLeft): met->nor[0] = planeInfo(cfg[i],0,planeRcfg); break;
    case (NormalBase): met->nor[1] = planeInfo(cfg[i],0,planeRcfg); break;
    case (NormalDeep): met->nor[2] = planeInfo(cfg[i],0,planeRcfg); break;
    case (SelectIdx): met->idx = planeInfo(cfg[i],0,planeRcfg); break;
    case (MetricAct): met->act = planeInfo(cfg[i],0,planeRcfg); break;}
    centerPlace(ext);
}
void demoMenu(struct Menu *menu);
void machineDemo(int sig, int *arg)
{
    if (sig != DemoArgs) ERROR();
    struct Extend *src = machineCenter(sig,arg,DemoArgs,DemoSrc,DemoSrcSub,"Demo");
    struct Menu *menu = machineMenu(src,sig,arg,DemoArgs,DemoSrc,DemoSrcSub);
    demoMenu(menu);
    machinePlace(src,sig,arg,DemoArgs,DemoSrc,DemoSrcSub);
}
int moveIval(struct Express *exp);
struct Extend *moveRefer(int sub); // leave to be changed in place
void moveDeref(int sub, struct Extend **ext); // compare sub to asr/sub to decide whether to move
void machineMove(struct Express *sub, struct Express *exp, int siz)
{
    if (siz > 9) ERROR();
    if (waitSafe(copySem) != 0) ERROR();
    if (waitSafe(pipeSem) != 0) ERROR();
    if (callHnfo() <= 1 && waitSafe(evalSem) != 0) ERROR();
    int num[siz]; for (int i = 0; i < siz; i++) num[i] = moveIval(&sub[i]);
    // negative num refers to a queue, positive is sub into center
    for (int i = 0; i < siz; i++) {
    struct Extend *ptr = moveRefer(num[i]);
    int empty = (ptr == 0); if (empty) {allocExtend(&ptr,1); ptr->asr = PullAsr;}
    writeExtend(ptr,datxClr(0));
    char str[3]; str[0] = '_'; str[1] = '0' + i; str[2] = 0;
    void *dat0 = 0; datxStr(&dat0,str);
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPEExtend);
    free(dat0); free(dat1);}
    // each expression does pull from num and place to exp.asr/sub
    for (int i = 0; i < siz; i++) {
    struct Extend *ptr = moveRefer(num[i]);
    writeExtend(ptr,datxClr(0));
    void *dat0 = 0; datxStr(&dat0,"_");
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPEExtend);
    free(dat0); free(dat1);
    void *dat = 0; int typ = datxEval(&dat,&exp[i],TYPEExtend);
    if (typ != TYPEExtend) ERROR();
    freeExtend(ptr); readExtend(ptr,datxPut(0,dat)); free(dat);
    moveDeref(num[i],&ptr);}
    if (callHnfo() <= 1 && postSafe(evalSem) != 1) ERROR();
    if (postSafe(pipeSem) != 1) ERROR();
    if (postSafe(copySem) != 1) ERROR();
}
void machineEval(struct Express *exp, int idx)
{
    struct Extend *ext = centerPull(idx,"Eval");
    struct Center *ptr = ext->ptr;
    if (callHnfo() <= 1 && waitSafe(evalSem) != 0) ERROR();
    writeCenter(ptr,datxClr(0));
    void *dat0 = 0; datxStr(&dat0,"_");
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPECenter);
    free(dat0); free(dat1);
    void *dat = 0; int typ = datxEval(&dat,exp,TYPECenter);
    if (typ != TYPECenter) ERROR();
    freeCenter(ptr); readCenter(ptr,datxPut(0,dat)); free(dat);
    if (callHnfo() <= 1 && postSafe(evalSem) != 1) ERROR();
    centerPlace(ext);
}
void machineVoid(struct Express *exp)
{
    if (callHnfo() <= 1 && waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int typ = datxEval(&dat,exp,-1); free(dat);
    if (callHnfo() <= 1 && postSafe(evalSem) != 1) ERROR();
}
int machineIval(struct Express *exp)
{
    if (callHnfo() <= 1 && waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int typ = datxEval(&dat,exp,TYPEInt);
    if (typ != TYPEInt) ERROR();
    int val = readInt(datxPut(0,dat));
    free(dat);
    if (callHnfo() <= 1 && postSafe(evalSem) != 1) ERROR();
    return val;
}
int machineEscape(struct Machine *mch, int siz, int level, int next)
{
    int inc = (level > 0 ? 1 : (level == 0 ? 0 : -1)); level *= inc;
    while (1) {
    next += inc;
    if (next < 0 || next >= siz) break;
    struct Machine *mptr = &mch[next];
    if (mptr->xfr == Nest) level += mptr->lvl*inc;
    if (level <= 0) break;}
    return next;
}
void machineArg(int *arg, int sig, struct Express *exp);
void machineSwitch(struct Machine *mptr)
{
    if (!mptr) ERROR();
    switch (mptr->xfr) {default: ERROR();
    case (Stage): machineStage(mptr->sav,mptr->siz,machineIval(mptr->idx)); break; // TODO remove when tests changed over to Move
    case (Tsage): machineTsage(mptr->sav,mptr->siz,machineIval(mptr->idx)); break; // TODO remove when tests changed over to Move
    case (Dump): *(int*)0=0; break;
    case (Move): machineMove(mptr->sub,mptr->fun,mptr->atm); break; // each fun takes Extend @_, and Extend's in @0 @1 @2 ... indicated by sub, and returns Extend
    case (Eval): machineEval(&mptr->fnc[0],machineIval(&mptr->res[0])); break; // takes Center in @_, returns Center
    case (Void): machineVoid(&mptr->exp[0]); break; // expression has side effects
    case (Proj): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineProj(mptr->sig,arg);} break;
    case (Bnry): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineBnry(mptr->sig,arg);} break;
    case (Comp): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineComp(mptr->sig,arg);} break;
    case (Form): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineForm(mptr->sig,arg);} break;
    case (Send): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineSend(mptr->sig,arg);} break;
    case (Self): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineSelf(mptr->sig,arg);} break;
    case (Glob): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineGlob(mptr->sig,arg);} break;
    case (Bopy): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineBopy(mptr->sig,arg);} break; // TODO remove when tests changed over to Move
    case (Copy): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineCopy(mptr->sig,arg);} break; // TODO remove when tests changed over to Move
    case (Dopy): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineDopy(mptr->sig,arg);} break; // TODO remove when tests changed over to Move
    case (Mopy): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineMopy(mptr->sig,arg);} break; // TODO remove when tests changed over to Move
    case (Popy): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machinePopy(mptr->sig,arg);} break; // TODO remove when tests changed over to Move
    case (Qopy): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineQopy(mptr->sig,arg);} break; // TODO remove when tests changed over to Move
    case (Ropy): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineRopy(mptr->sig,arg);} break; // TODO remove when tests changed over to Move
    case (Demo): {int arg[mptr->sig]; machineArg(arg,mptr->sig,mptr->arg); machineDemo(mptr->sig,arg);} break;}
}

// unprotected called by big hammer
int moveIval(struct Express *exp)
{
    void *dat = 0; int typ = datxEval(&dat,exp,TYPEInt);
    if (typ != TYPEInt) ERROR();
    int val = readInt(datxPut(0,dat));
    free(dat);
    return val;
}
void moveSize(int idx)
{
    if (idx < 0) ERROR();
    if (idx >= centers) {int size = idx+1; center = realloc(center,size*sizeof(struct Extend *));
    for (int i = centers; i < size; i++) center[i] = 0; centers = size;}
}
struct Extend *moveRefer(int sub)
{
    struct Extend *ptr = 0;
    enum Assert asr = (sub < 0 ? -asr : PlaceAsr);
    if (sub == -1) asr = PullAsr;
    switch (asr) {default: ERROR();
    break; case (PullAsr):
    break; case (PlaceAsr): moveSize(sub); ptr = center[sub];
    break; case (PipeAsr): ptr = frontCenterq(internal);
    break; case (RespAsr): ptr = frontCenterq(response);
    break; case (DoneAsr): ptr = frontCenterq(replace);}
    return ptr;
}
void moveDeref(int sub, struct Extend **ext)
{
    struct Extend *ptr = *ext;
    struct Extend *chk = moveRefer(sub);
    if (ptr->asr == PullAsr && chk == 0) chk = ptr;
    if (chk != ptr) ERROR();
    // PullAsr in sub moves from nowhere
    // PullAsr in ptr moves to nowhere
    // LoopAsr in ptr does pop/push in sub and changes ptr to sub
    // sub equal to ptr does nothing; ptr already changed in place
    // otherwise set pointer at sub to zero, deallocate poiner at ptr, and set pointer at ptr to ptr
    enum Assert asr = (sub < 0 ? -asr : PlaceAsr);
    if (sub == -1) asr = PullAsr;
    void *que = 0; switch(asr) {default: ERROR();
    break; case (PullAsr): case (PlaceAsr):
    break; case (PipeAsr): que = internal;
    break; case (RespAsr): que = response;
    break; case (DoneAsr): que = replace;}
    if (ptr->asr == LoopAsr && que == 0) ERROR();
    int equal = (ptr->asr == asr && (asr != PlaceAsr || ptr->sub == sub));
    if (ptr->asr == LoopAsr) ptr->asr = asr;
    if (equal) return;
    if (que) popCenterq(que); else if (asr == PlaceAsr) center[sub] = 0;
    switch (ptr->asr) {default: ERROR();
    break; case (PullAsr): if (ptr) deleteSmart(ptr->log); freeExtend(ptr); allocExtend(ext,0);
    break; case (PlaceAsr): moveSize(ptr->sub); if (center[ptr->sub]) deleteSmart(center[ptr->sub]->log); freeExtend(center[ptr->sub]); allocExtend(&center[ptr->sub],0); center[ptr->sub] = ptr;
    break; case (PipeAsr): pushCenterq(ptr,internal);
    break; case (RespAsr): pushCenterq(ptr,response);
    break; case (DoneAsr): pushCenterq(ptr,replace);}
}

int demoJect(struct Menu *menu)
{
    switch (menu->jec) {default: ERROR();
    break; case(Inject): return menu->inj;
    break; case(Object): return menu->obj;
    break; case(Subject): return menu->suj;}
    return 0;
}
void demoDone(struct Menu *menu)
{
    int arg[4] = {menu->ker,demoJect(menu),menu->mat,menu->sub}; machineSend(4,arg);
}
void demoCont(struct Menu *menu)
{
    int arg[2] = {menu->ker,demoJect(menu)}; machineForm(2,arg);
}
void demoPack(struct Menu *menu)
{
    int arg[2] = {menu->mat,menu->tmp}; machineDopy(2,arg);
    // TODO move menu->sub in menu->tmp to 0 in menu->tmp, resize menu->tmp to 1, and change idx in menu->tmp to menu->sub
    int cpy[2] = {menu->tmp,0}; machineCopy(2,cpy);
}
void demoSize(struct Menu *menu)
{
    int pro[2] = {menu->mat,menu->sub}; machineProj(2,pro);
    demoPack(menu);
    int drw[2] = {menu->drw,0}; machineCopy(2,drw);
}
void demoDisp(struct Menu *menu)
{
    int cmp[4] = {menu->ker,demoJect(menu),menu->mat,menu->sub}; machineComp(4,cmp);
    int mat[2] = {menu->mat,0}; machineCopy(2,mat);
    int drw[2] = {menu->drw,0}; machineCopy(2,drw); // TODO do Dopy first, so Drawz is never zero
}
void demoMenu(struct Menu *menu)
{
    switch (menu->msk) {default: ERROR();
    break; case (SlctMsk): {
    int arg[4] = {menu->mat,menu->sub,menu->ker,demoJect(menu)};
    if (menu->slf >= 0) machineGlob(4,arg); else machineSelf(4,arg);
    demoDisp(menu);}
    break; case (DoneMsk): {
    // TODO add field to menu for place to hold Metric
    // TODO add to Configure Stage Tsage to get/put Metric fields from/to Piercez mem in Center
    // TODO Getoldz is only depth; calculate other coordinates from UniformWid/Hei and Focal*
    // TODO following only if Metric is completed
    // TODO mark Metric as the opposite of complete
    switch (menu->act) {default: ERROR();
    break; case (Indicate): {
    // TODO Tsage from Metric
    menu->act = Manipulate;}
    break; case (Divisive): case (Additive): case (Subtractive): case (Operative): {
    /*TODO relay menu->act Fixed* Normal* SelectIdx to other process*/}}}
    break; case (PrssMsk): { // do Send; change menu state
    demoDone(menu);
    char key = planeInfo(PressKey,0,planeRcfg);
    planeJnfo(PressQueue,-1,planeRmw);
    switch (key) {default:
    break; case ('C'): menu->coo = (1<<Mouse)|(1<<Rotate)|(1<<Cursor); menu->act = Indicate;
    break; case ('N'): menu->coo = (1<<Mouse)|(1<<Rotate)|(1<<Normal); menu->act = Indicate;
    break; case ('O'): menu->coo = (1<<Mouse)|(1<<Rotate)|(1<<Ortho); menu->act = Indicate;
    break; case ('T'): menu->coo = (1<<Mouse)|(1<<Slide)|(1<<Ortho); menu->act = Indicate;
    break; case ('P'): menu->coo = (1<<Mouse)|(1<<Slide)|(1<<Normal); menu->act = Indicate;
    break; case ('R'): menu->ang = (1<<Roller)|(1<<Rotate)|(1<<Cursor); menu->act = Indicate;
    break; case ('U'): menu->ang = (1<<Roller)|(1<<Rotate)|(1<<Focal); menu->act = Indicate;
    break; case ('Z'): menu->ang = (1<<Roller)|(1<<Slide)|(1<<Ortho); menu->act = Indicate;
    break; case ('Q'): menu->ang = (1<<Roller)|(1<<Slide)|(1<<Normal); menu->act = Indicate;
    break; case ('F'): menu->ang = (1<<Roller)|(1<<Scale)|(1<<Pierce); menu->act = Indicate;
    break; case ('A'): menu->act = Additive;
    break; case ('S'): menu->act = Subtractive;
    break; case ('B'): menu->act = Divisive;
    break; case ('M'): menu->act = Operative;
    break; case ('W'): /*TODO Warp to last metric sent*/}}
    break; case (ProjMsk): demoSize(menu);
    break; case (EoodMsk): // TODO wait for window resize
    break; case (MoveMsk): if (menu->act == Manipulate) { // if enabled: do Form last manipulate was roller; change manipulate mode; do Comp Copy Display
    if (menu->dev == Angle) {demoCont(menu); menu->dev = Coord;}
    planeInfo(ManipFixed,(menu->dev==Coord?menu->coo:menu->ang),planeWcfg);
    demoDisp(menu);}
    break; case (ClckMsk): switch (menu->act) {default: ERROR();
    break; case (Manipulate): {demoDone(menu); menu->act = Indicate;}
    break; case (Indicate): case (Divisive): case (Additive): case (Subtractive): case (Operative): {
    planeJnfo(ClickQueue,1,planeWcfg); // discard to last click
    // TODO Dopy/Draw for pierce point, and in DoneMsk, get Fixed* Normal* SelectIdx from Getoldz Vectorz Getintz at Click*
    planeJnfo(ClickQueue,0,planeWcfg);}}
    break; case (RollMsk): if (menu->act == Manipulate) { // if enabled: do Form if last manipulate was move; change manipulate state; do Comp and Display
    if (menu->dev == Coord) {demoCont(menu); menu->dev = Angle;}
    planeInfo(ManipFixed,(menu->dev==Coord?menu->coo:menu->ang),planeWcfg);
    demoDisp(menu);}}
}

// phase callbacks
void planeClose(enum Thread tag, int idx)
{
    planeJnfo(RegisterOpen,(1<<tag),planeWotc);
}
void planeJoin(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): if (idx) {for (int i = ffs(external)-1; external; external &= ~(1<<i), i = ffs(external)-1) freeIdent(i); closeIdent(extdone);}
    break; case (StdioThd): if (idx) {freeIdent(console); closeIdent(condone);}
    break; case (MachThd): case (TimeThd): case (TestThd):}
}
void planeWake(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): case (StdioThd): case (MachThd): case (TimeThd): case (TestThd):}
    postSafe(safeSafe(tag,idx));
}

void machineArg(int *arg, int sig, struct Express *exp)
{
    for (int i = 0; i < sig; i++) arg[i] = machineIval(&exp[i]);
}
void machinePop(int sig, int chk, int dst, void *que)
{
    if (sig != chk) ERROR();
    if (waitSafe(pipeSem) != 0) ERROR();
    struct Extend *ptr = maybeCenterq(0,que);
    if (postSafe(pipeSem) != 1) ERROR();
    enum Assert asr = (que==internal?PipeAsr:DoneAsr);
    if (ptr != 0 && ptr->asr != asr) ERROR(); else if (ptr != 0) ptr->asr = PullAsr;
    if (ptr == 0) centerClear(dst);
    else {ptr->sav = ptr->sub; ptr->sub = dst; centerPlace(ptr);}
}
void planeMachine(enum Thread tag, int idx);
void machineExec(int idx, struct Extend *ext)
{
    struct Center *ptr = ext->ptr;
    switch (ptr->mem) {default: ERROR();
    case (Transferz): for (int i = 0; i < ptr->siz; i++) machineSwitch(&ptr->exe[i]); break;
    case (Machinez): for (int i = 0; i < ptr->siz; i++) machineSwitch(&ptr->mch[i]); break;
    case (Rebootz): {
    struct Extend **cent = (struct Extend **)malloc(sizeof(struct Extend *)*ptr->siz);
    int *boot = (int *)malloc(sizeof(int)*ptr->siz);
    void *repush = 0; repush = allocCenterq();
    printfSmart(ext->log,"Exec %d",ptr->siz);
    for (int i = 0; i < ptr->siz; i++) {
    // clear event before clearing the condition that the event indicates
    planeInfo(RegisterWake,1<<SlctMsk,planeWotc);
    if (waitSafe(pipeSem) != 0) ERROR();
    struct Extend *nxt = maybeCenterq(0,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    if (nxt != 0 && nxt->asr != PipeAsr) ERROR(); else if (nxt != 0) nxt->asr = PullAsr;
    if (nxt == 0 && waitSafe(safeSafe(MachThd,0)) < 0) break;
    if (nxt == 0) {i--; continue;}
    if (nxt->src != ext->src/*TODO || nxt->ptr->slf != ptr->slf*/) {
    nxt->asr = PipeAsr; pushCenterq(nxt,repush); continue;}
    printfSmart(ext->log,"Exec %d/%d:%d %05d:%s",i,ptr->siz,ptr->sub[i],numberSmart(nxt->log),nameSmart(nxt->log));
    boot[i] = ptr->sub[i]; cent[i] = nxt;}
    if (sizeCenterq(repush) > 0) {
    if (waitSafe(pipeSem) != 0) ERROR();
    joinCenterq(repush,internal);
    if (postSafe(pipeSem) != 1) ERROR();}
    if (waitSafe(pipeSem) != 0) ERROR();
    int size = sizeCenterq(internal);
    if (postSafe(pipeSem) != 1) ERROR();
    if (size) planeJnfo(RegisterWake,(1<<SlctMsk),planeWots);
    freeCenterq(repush);
    safeInit(MachThd,idx+1,0);
    if (funcSafe(safeSem,safeFunc,&idx) != 0) ERROR(); // wait for machine[idx] < 0
    free(reboot[idx]); free(recent[idx]); resize[idx] = 0;
    reboot[idx] = boot; recent[idx] = cent; resize[idx] = ptr->siz; machine[idx] = 0;
    callFork(MachThd,idx,planeMachine,planeClose,planeJoin,planeWake);
    if (postSafe(safeSem) != 1) ERROR();}
    break;}
}

// thread callbacks
void planeMachine(enum Thread tag, int idx)
{
    funcSafe(safeSem,safeGunc,&idx); // wait for machine[idx] >= 0
    int index = machine[idx];
    int *boot = reboot[idx]; reboot[idx] = 0;
    struct Extend **cent = recent[idx]; recent[idx] = 0;
    int size = resize[idx]; resize[idx] = 0;
    postSafe(safeSem);
    if (index < 0) ERROR(); if (size == 0) {size = 1;
    boot = malloc(sizeof(int)); boot[0] = -1;
    cent = malloc(sizeof(struct Extend *));
    cent[0] = centerPull(index,"Mach");}
    // if (idx > 0) {fprintf(stderr,"Mach"); for (int i = 0; i < size; i++) {char *st0 = 0; char *st1 = 0; int src = -1; char *st2 = 0; if (cent[i]->ptr->mem != Transferz) showMemory(cent[i]->ptr->mem,&st1);  else if (cent[i]->ptr->exe[0].xfr == Bopy || cent[i]->ptr->exe[0].xfr == Qopy) {src = machineIval(&cent[i]->ptr->exe[0].arg[0]); if (src <= Memorys+1) showTransfer(cent[i]->ptr->exe[0].xfr,&st1); else assignStr(&st1,"-");} else {showTransfer(cent[i]->ptr->exe[0].xfr,&st1);} fprintf(stderr," -- %d %s(%d)",boot[i],st1,src); free(st0); free(st1); free(st2);} fprintf(stderr,"\n");}
    for (int i = 0; i < size; i++) {
    if (boot[i] >= 0) {cent[i]->sub = boot[i]; centerPlace(cent[i]);}
    else {struct Center *cptr = cent[i]->ptr;
    int next = 0; struct Machine *mach = 0;
    switch (cptr->mem) {default: next = -1;
    break; case (Machinez): mach = cptr->mch;
    break; case (Transferz): mach = cptr->exe;}
    while (next >= 0 && next < cptr->siz) {
    struct Machine *mptr = &mach[next];
    int save = next;
    switch (mptr->xfr) {default: machineSwitch(mptr); next += 1; break;
    case (Goto): next += machineIval(&mptr->exp[0]); break;
    case (Jump): next = machineEscape(mach,cptr->siz,machineIval(&mptr->exp[0]),next); break;
    case (Nest): next += 1; break;}
    if (next == save) {
    if (waitSafe(safeSafe(MachThd,idx)) < 0) next = -1;
    else next += 1;}}}}
    for (int i = 0; i < size; i++) if (boot[i] < 0) {
    if (cent[i]) deleteSmart(cent[i]->log); freeExtend(cent[i]); allocExtend(&cent[i],0);}
    free(boot); free(cent);
    waitSafe(safeSem);
    machine[idx] = -1;
    postSafe(safeSem);
}
void planeCenter(enum Thread tag, int idx)
{
    while (1) {
    if (waitSafe(safeSafe(PipeThd,idx)) < 0) break;
    if (waitSafe(pipeSem) != 0) ERROR();
    struct Extend *center = maybeCenterq(0,response);
    if (postSafe(pipeSem) != 1) ERROR();
    if (center != 0 && center->asr != RespAsr) ERROR(); else if (center != 0) center->asr = PullAsr;
    if (center && center->ptr->slf < 0) {
    center->ptr->slf = planeInfo(RegisterSelf,0,planeRcfg);
    {char *st0 = 0; showExtend(center,&st0); printfSmart(center->log,"Loop %s",st0); free(st0);}
    center->asr = PipeAsr;
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(center,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<SlctMsk),planeWots);}
    else if (center && center->ptr->slf >= 0) {
    if (center->src < 0 || center->src >= Programs) ERROR();
    if (waitSafe(pipeSem) != 0) ERROR();
    int sub = inverse[center->src];
    if (postSafe(pipeSem) != 1) ERROR();
    {char *st0 = 0; showExtend(center,&st0); printfSmart(center->log,"Write %s",st0); free(st0);}
    writeCenter(center->ptr,sub);
    center->ret = DoneRet;
    centerDone(center);}}
}
void planeExternal(enum Thread tag, int idx)
{
    while (1) {
    if (waitSafe(pipeSem) != 0) ERROR();
    int temp = external;
    if (postSafe(pipeSem) != 1) ERROR();
    int sub = waitRead(0.0,(temp|(1<<extdone)));
    // WARN semaphore inside of pipeSem will deadlock,
    // because callbacks inside of Jnfo semaphore wait on pipeSem.
    // Nested semaphores are fine if they are nested in the same order.
    if (sub == extdone) {if (readChr(extdone)) break; else continue;}
    if ((1<<sub)&temp != (1<<sub)) ERROR();
    struct Extend *center = 0; allocExtend(&center,1);
    readCenter(center->ptr,sub);
    if (waitSafe(pipeSem) != 0) ERROR();
    center->src = (int*)*userIdent(sub) - inverse;
    if (postSafe(pipeSem) != 1) ERROR();
    {int debug = ((planeInfo(RegisterVerb,0,planeRcfg)&(1<<PipeVrb)) != 0);
    center->log = selfSmart(debug?"Pipe":0);
    if (debug) {char *st0 = 0; showExtend(center,&st0); printfSmart(center->log,"%s",st0); free(st0);}}
    center->asr = PipeAsr;
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(center,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<SlctMsk),planeWots);}
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
    pushChrq(chr,tempq);
    if (chr == '\n') {char *str = malloc(sizeChrq(tempq)+1); char *ptr = str;
    while (sizeChrq(tempq)) {*(ptr++) = frontChrq(tempq); popChrq(tempq);} *(ptr++) = 0;
    if (waitSafe(stdioSem) != 0) ERROR();
    pushStrq(str,strin);
    int size = sizeStrq(strin);
    if (postSafe(stdioSem) != 1) ERROR();
    planeJnfo(RegisterStrq,size,planeWcfg);
    planeJnfo(RegisterWake,(1<<CnslMsk),planeWots);}}
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
    if (sizeTimeq(timeq) != sizeIntq(wakeq)) ERROR();
    float time = frontTimeq(timeq); int wake = frontIntq(wakeq);
    if (postSafe(timeSem) != 1) ERROR();
    float delta = time-(float)processTime(); // how long to wait
    if (timeSafe(safeSafe(TimeThd,0),delta) < 0) break;
    if ((float)processTime() >= time) {
    if (waitSafe(timeSem) != 0) ERROR();
    dropTimeq(timeq); dropIntq(wakeq);
    if (postSafe(timeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<TimeMsk),planeWots);
    postSafe(safeSafe(MachThd,wake));}}
}
void planeTest(enum Thread tag, int idx)
{
    switch (idx) {default: ERROR();

    break; case (0): {
    int debug = 0; int count = 0; float time = 0.0; int tested = 0; int alt = 0;
    int mode = (planeInfo(RegisterPlan,0,planeRcfg)==Bringup);

    while (timeSafe(safeSafe(TestThd,idx),0.0) >= 0) {
    if (time == 0.0) time = processTime();
    if (processTime()-time > 0.1) {time = processTime(); count += 1;}

    struct Extend *mat = centerPeek(Matrixz,(debug?"Test0":0)); if (!mat) {callWait(); continue;}
    freeCenter(mat->ptr); mat->ptr->mem = Matrixz;
    if (alt) {mat->ptr->idx = 2; mat->ptr->siz = 2;} // uni.pro tri.pol
    else {mat->ptr->idx = 0; mat->ptr->siz = 1;} // uni.all
    allocMatrix(&mat->ptr->mat,mat->ptr->siz);
    if (alt) {planeWindow(mat->ptr->mat[0].mat);
    float fix[] = {0.0f,0.0f,0.4f};
    float org[] = {0.0f,0.0f};
    float time = processTime();
    float leg = 0.4f*sinf(time*8.0f);
    float cur[] = {leg,leg};
    planeRotateFocalMouse(mat->ptr->mat[1].mat,fix,0,org,cur);}
    else planeMatrix(mat->ptr->mat[0].mat);
    mat->sub = Matrixz; mat->rsp = RptRsp;
    callCopy(mat,1,(debug?"matrix":0));
    if (alt) alt = 0; else alt = 1;

    if (count == tested) {
    int width,height; {enum Configure cfg[2] = {UniformWid,UniformHei}; int val[2] = {0,0};
    callInfo(cfg,val,2,planeRcfg); width = val[0]; height = val[1];}
    int giv[] = {width,height,0,12}; // idx,siz
    struct Extend *drw = centerPeek(Drawz,(debug?"Test1":0)); if (!drw) {callWait(); continue;}
    freeCenter(drw->ptr);
    drw->ptr->mem = Drawz; drw->ptr->idx = 0; drw->ptr->siz = 1;
    allocDraw(&drw->ptr->drw,drw->ptr->siz);
    drw->ptr->drw[0].con.tag = MicroCon;
    drw->ptr->drw[0].con.mic = (mode?MicroFetDrw:MicroVtxDrw);
    drw->ptr->drw[0].siz = sizeof(giv)/sizeof(int);
    allocInt(&drw->ptr->drw[0].arg,drw->ptr->drw[0].siz);
    for (int i = 0; i < drw->ptr->drw[0].siz; i++) drw->ptr->drw[0].arg[i] = giv[i];
    drw->sub = Drawz; drw->rsp = RetRsp;
    callCopy(drw,1,(debug?"test":0));}
    tested = count;}}

    break; case (1): {
    int debug = 0; int count = 0; float time = 0.0; int tested = 0;
    int width,height; {enum Configure cfg[2] = {UniformWid,UniformHei}; int val[2] = {0,0};
    callInfo(cfg,val,2,planeRcfg); width = val[0]; height = val[1];}
    int hiv[] = {width,height,0,12}; // width,height,idx,siz
    int fiv[] = {width,height}; // width,height

    while (timeSafe(safeSafe(TestThd,idx),0.0) >= 0) {
    if (time == 0.0) time = processTime();
    if (processTime()-time > 0.1) {time = processTime(); count += 1;}

    if (count == tested) {}

    else if (count%6 == 1 || count%6 == 4) {
    struct Extend *eek = centerPeek(Getoldz,(debug?"Test2":0)); if (!eek) {callWait(); continue;}
    freeCenter(eek->ptr);
    eek->ptr->mem = Getoldz; eek->ptr->idx = (int)(0.3*width)+(int)(0.3*height)*width; eek->ptr->siz = 1;
    allocOld(&eek->ptr->old,eek->ptr->siz);
    eek->sub = Getoldz; eek->rsp = RptRsp;
    callCopy(eek,0,(debug?"peek":0));}

    else if (count%6 == 2 || count%6 == 5) {
    struct Extend *eek = centerPeek(Getintz,(debug?"Test3":0)); if (!eek) {callWait(); continue;}
    freeCenter(eek->ptr);
    eek->ptr->mem = Getintz; eek->ptr->idx = (int)(0.3*width)+(int)(0.3*height)*width; eek->ptr->siz = 1;
    allocInt(&eek->ptr->uns,eek->ptr->siz);
    eek->sub = Getintz; eek->rsp = RptRsp;
    callCopy(eek,0,(debug?"ident":0));}

    else if (count%6 == 3 || count%6 == 0) {
    struct Extend *vec = centerPeek(Vectorz,(debug?"Test4":0)); if (!vec) {callWait(); continue;}
    freeCenter(vec->ptr);
    vec->ptr->mem = Vectorz; vec->ptr->idx = (int)(0.3*width)+(int)(0.3*height)*width; vec->ptr->siz = 1;
    allocVector(&vec->ptr->vec,vec->ptr->siz);
    vec->ptr->vec[0].vec[0] = 1.0; vec->ptr->vec[0].vec[1] = 2.0;
    vec->ptr->vec[0].vec[2] = 3.0; vec->ptr->vec[0].vec[3] = 4.0;
    vec->sub = Vectorz; vec->rsp = RptRsp;
    callCopy(vec,0,(debug?"getvec":0));}

    tested = count;}}}
}

// register callbacks
void registerCall(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterCall) ERROR();
    int wake = val & 0xff; // thread to wake
    int indx = val >> 8; // machine center
    safeInit(MachThd,wake+1,0);
    if (indx >= 0) {
    if (funcSafe(safeSem,safeFunc,&wake) != 0) ERROR(); // wait for machine[wake] < 0
    machine[wake] = indx;
    if (postSafe(safeSem) != 1) ERROR();
    callFork(MachThd,wake,planeMachine,planeClose,planeJoin,planeWake);}
    else doneSafe(safeSafe(MachThd,wake));
}
void registerOpen(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterOpen) ERROR();
    if ((act & (1<<PipeThd)) && !(sav & (1<<PipeThd))) {
        extdone = openPipe();
        safeInit(PipeThd,1,0);
        callFork(PipeThd,0,planeCenter,planeClose,planeJoin,planeWake);
        callFork(PipeThd,1,planeExternal,planeClose,planeJoin,planeWake);}
    if (!(act & (1<<PipeThd)) && (sav & (1<<PipeThd))) {
        doneSafe(safeSafe(PipeThd,0));
        writeChr(1,extdone);}
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
        planeKnfo(RegisterCall,planeGnfo(RegisterMain,0,planeRcfg)<<8,planeWcfg);}
    if (!(act & (1<<MachThd)) && (sav & (1<<MachThd))) {
        planeKnfo(RegisterCall,(-1<<8),planeWcfg);}
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
    int able = (sizeIntq(ableq) > i ? *ptrIntq(i,ableq) : 0);
    wake |= able;}
    wake &= planeGnfo(RegisterOpen,0,planeRcfg);
    for (int i = ffs(wake)-1; wake; i = ffs(wake&=~(1<<i))-1) {
    planeWake(MachThd,i);}
}
void registerAble(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterAble) ERROR();
    int wake = val & 0xff; // thread to wake
    int mask = val >> 8; // mask of events
    while (sizeIntq(maskq) <= wake) pushIntq(0,maskq);
    int even = *ptrIntq(wake,maskq);
    for (int i = ffs(even)-1; even; i = ffs(even&=~(1<<i))-1) {
    while (sizeIntq(ableq) <= i) pushIntq(0,ableq);
    *ptrIntq(i,ableq) &= ~(1<<wake);}
    *ptrIntq(wake,maskq) = mask;
    for (int i = ffs(mask)-1; mask; i = ffs(mask&=~(1<<i))-1) {
    while (sizeIntq(ableq) <= i) pushIntq(0,ableq);
    *ptrIntq(i,ableq) |= 1<<wake;}
}
void registerTime(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterTime) ERROR();
    int lwr = val & 0xff; // machine thread to wake
    int upr = val >> 8; // amount to advance
    if (lwr < 0 || lwr >= Threads) ERROR();
    if (waitSafe(timeSem) != 0) ERROR();
    if (!existTimep(lwr,timep)) insertTimep(lwr,0.0,timep);
    if (*ptrTimep(lwr,timep) < start) *ptrTimep(lwr,timep) = processTime();
    float time = *ptrTimep(lwr,timep) + (float)upr/1000.0; *ptrTimep(lwr,timep) = time;
    if (sizeTimeq(timeq) && backTimeq(timeq) > time) {
    pushTimeq(backTimeq(timeq),timeq);
    int idx = sizeTimeq(timeq)-2;
    while (idx > 0 && *ptrTimeq(idx,timeq) > time) {idx--;
    *ptrTimeq(idx+1,timeq) = *ptrTimeq(idx,timeq);
    *ptrIntq(idx+1,wakeq) = *ptrIntq(idx,wakeq);}
    *ptrTimeq(idx,timeq) = time;
    *ptrIntq(idx,wakeq) = lwr;} else {
    pushTimeq(time,timeq);
    pushIntq(lwr,wakeq);}
    if (postSafe(timeSem) != 1) ERROR();
    postSafe(safeSafe(TimeThd,0));
}
void registerExit(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterExit) ERROR();
    callWake();
}
void registerVerb(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != RegisterVerb) ERROR();
    clearSmart();
}
void registerUniform(enum Configure cfg, int sav, int val, int act)
{
    switch (cfg) {default: ERROR();
    case (UniformWid):
    case (UniformHei):
    planeKnfo(RegisterWake,(1<<ProjMsk),planeWots);
    case (UniformAll):
    case (UniformOne):
    case (UniformIdx):
    case (UniformUse):
    case (UniformTri):
    case (UniformNum):
    case (UniformVtx):
    case (UniformMat):
    case (UniformBas):
    case (UniformMod):
    planeKnfo(RegisterWake,(1<<UnifMsk),planeWots);}
}
void registerArgument(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != ArgumentInp && cfg != ArgumentOut && cfg != ArgumentSrc) ERROR();
    enum Configure arg[3] = {ArgumentInp,ArgumentOut,ArgumentSrc}; int num[3] = {0,0,0};
    callGnfo(arg,num,3,planeRcfg);
    int rdfd = num[0];
    int wrfd = num[1];
    int asrc = num[2];
    if (waitSafe(pipeSem) != 0) ERROR();
    int sub = rdwrInit(rdfd,wrfd);
    external |= 1<<sub;
    inverse[asrc] = sub;
    *userIdent(sub) = inverse + asrc;
    if (postSafe(pipeSem) != 1) ERROR();
    writeChr(0,extdone);
}
void registerQue(enum Configure cfg, int val, enum Configure ary[], void *ptr[], int siz, int msk)
{
    void *que = 0;
    for (int i = 0; i < siz; i++)
    if (cfg == ary[i]) que = ptr[i];
    if (que == 0) ERROR();
    if (waitSafe(pressSem) != 0) ERROR();
    pushIntq(val,que);
    planeGnfo(cfg,frontIntq(que),planeWcfg);
    planeKnfo(RegisterWake,(1<<msk),planeWots);
    if (postSafe(pressSem) != 1) ERROR();
}
void registerQues(enum Configure cfg, int act, enum Configure chk, void *ptr[], int siz, int msk)
{
    if (cfg != chk) ERROR();
    if (waitSafe(pressSem) != 0) ERROR();
    for (int i = 0; i < siz; i++) {
    while (act < sizeIntq(ptr[i])) popIntq(ptr[i]);
    while (act > sizeIntq(ptr[i])) pushIntq(0,ptr[i]);}
    int num = 0; for (int i = 0; i < siz; i++)
    if (sizeIntq(ptr[i]) > 0) num += 1;
    if (num > 0) planeKnfo(RegisterWake,(1<<msk),planeWots);
    if (postSafe(pressSem) != 1) ERROR();
}
void registerChar(enum Configure cfg, int sav, int val, int act)
{
    enum Configure ary[1] = {PressKey};
    void *ptr[1] = {charq};
    registerQue(cfg,val,ary,ptr,1,PrssMsk);
}
void registerChars(enum Configure cfg, int sav, int val, int act)
{
    void *ptr[1] = {charq};
    registerQues(cfg,act,PressQueue,ptr,1,PrssMsk);
}
void registerClick(enum Configure cfg, int sav, int val, int act)
{
    enum Configure ary[3] = {ClickLeft,ClickBase,ClickAngle};
    void *ptr[3] = {leftq,baseq,angleq};
    registerQue(cfg,val,ary,ptr,3,ClckMsk);
}
void registerClicks(enum Configure cfg, int sav, int val, int act)
{
    void *ptr[3] = {leftq,baseq,angleq};
    registerQues(cfg,act,ClickQueue,ptr,3,ClckMsk);
}
void registerMove(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != ManipLeft && cfg != ManipBase) ERROR();
    planeKnfo(RegisterWake,(1<<MoveMsk),planeWots);
}
void registerRoll(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != ManipAngle) ERROR();
    planeKnfo(RegisterWake,(1<<RollMsk),planeWots);
}
void registerLog(enum Configure cfg, int sav, int val, int act)
{
    if (cfg != CenterLog) ERROR();
    if (sav != act) {deleteSmart(sav); planeGnfo(cfg,otherSmart(act),planeWcfg);}
    if (sav != 0 && act == 0) for (int i = 0; i < centers; i++)
    if (center[i] != 0 && center[i]->log != 0) {
    // this is to allow centerPeek from machineTsage to get placed log
    deleteSmart(center[i]->log); center[i]->log = 0;}
}

// expression callbacks
enum DatxEnum centerField(int num, int fld, int sub, int typ, struct DatxField *arg)
{
    struct InitCenter *cst = (struct InitCenter *)arg;
    struct Center *src = cst->src->ptr;
    struct Center *dst = cst->dst->ptr;
    if (waitSafe(loopSem) != 0) ERROR();
    if (num == TYPECenter && fld == 1) {
    writeInt(cst->siz,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    else if (num == TYPECenter && fld < 4) {
    fwriteCenter(src,fld,sub,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    else if (num == TYPECenter && typ == TYPEKernel && sub < src->siz) {
    fwriteCenter(src,fld,sub,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    else if (num == TYPECenter && typ == TYPEKernel) {
    struct Kernel init;
    identmat(init.saved.mat,4);
    identmat(init.local.mat,4);
    identmat(init.sent.mat,4);
    identmat(init.global.mat,4);
    writeKernel(&init,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    if (postSafe(loopSem) != 1) ERROR();
    return 0; // TODO return whether changed
}
enum DatxEnum centerElem(int num, int fld, int sub, int typ, struct DatxField *arg)
{
    struct InitCenter *cst = (struct InitCenter *)arg;
    struct Center *src = cst->src->ptr;
    struct Center *sav = cst->sav->ptr;
    struct Center *dst = cst->dst->ptr;
    if (waitSafe(loopSem) != 0) ERROR();
    if (num == TYPECenter && fld == 1) {
    writeInt(cst->tot,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    else if (num == TYPECenter && fld < 4) {
    fwriteCenter(sav,fld,sub,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    else if (num == TYPECenter && sub < cst->ddx) {
    fwriteCenter(sav,fld,sub,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    else if (num == TYPECenter && sub >= cst->ddx && sub < cst->ddx+cst->siz && sub-cst->ddx+cst->sdx < src->siz) {
    fwriteCenter(src,fld,sub-cst->ddx+cst->sdx,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    else if (num == TYPECenter && sub >= cst->ddx+cst->siz) {
    fwriteCenter(sav,fld,sub-cst->siz,loopfd);
    freadCenter(dst,fld,sub,loopfd);}
    if (postSafe(loopSem) != 1) ERROR();
    return 0; // TODO return whether changed
}
int changed = 0; enum Memory mem = Memorys;
int resized = 0; int size = 0;
enum DatxEnum planeField(int num, int fld, int sub, int typ, struct DatxField *arg)
{
    if (fld == 0) changed = resized = 0;
    if (num == TYPECenter && resized && sub >= size && typ == TYPEMatrix) {
    struct Matrix init;
    identmat(init.mat,4);
    writeMatrix(&init,arg->idx);
    return InsrDat;}
    if (num == TYPECenter && resized && sub >= size && typ == TYPEKernel) {
    struct Kernel init;
    identmat(init.saved.mat,4);
    identmat(init.local.mat,4);
    identmat(init.sent.mat,4);
    identmat(init.global.mat,4);
    writeKernel(&init,arg->idx);
    return InsrDat;}
    if (num == TYPECenter && resized && sub >= size) return ZeroDat;
    if (num == TYPECenter && changed) return DscdDat;
    if (num == TYPECenter && fld == identField(num,"mem") && fld == arg->num) {changed = 1; mem = readInt(arg->src); writeInt(readInt(arg->fld),arg->idx); return InsrDat;}
    if (num == TYPECenter && fld == identField(num,"siz") && fld == arg->num) {resized = 1; size = readInt(arg->src); writeInt(readInt(arg->fld),arg->idx); return InsrDat;}
    if (num == TYPEExtend && fld == identField(num,"log") && fld == arg->num) {writeInt(otherSmart(readInt(arg->fld)),arg->idx); return ReplDat;}
    if (fld == arg->num && sub == arg->sub) return CopyDat;
    return KeepDat;
}
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
    planeJnfo(sub,val,planeWcfg);
}
void planeWoscfg(int val, int sub)
{
    planeJnfo(sub,val,planeWots);
}
void planeWoccfg(int val, int sub)
{
    planeJnfo(sub,val,planeWotc);
}
int planeRawcfg(int val, int sub)
{
    return planeJnfo(sub,val,planeRdwr);
}
int planeRetcfg(int sub)
{
    return planeInfo(sub,0,planeRcfg);
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
void planeArgv(int argc, char **argv)
{
    int debug = 0;
    for (int i = 0; i < argc; i++) {
    int asiz = 0; int csiz = 0; int msiz = 0; int esiz = 0; int ssiz = 0;
    struct Argument arg = {0}; struct Center cntr = {0}; struct Machine mchn = {0};
    struct Express expr = {0}; char *str = 0;
    if (hideArgument(&arg, argv[i], &asiz)) {
    enum Configure cfg[3] = {ArgumentInp,ArgumentOut,ArgumentSrc};
    int val[3] = {arg.inp,arg.out,arg.oth};
    callJnfo(cfg,val,3,planeWcfg); freeArgument(&arg);}
    else if (hideCenter(&cntr, argv[i], &csiz)) {
    struct Extend *ptr = 0; allocExtend(&ptr,1);
    copyCenter(ptr->ptr,&cntr); freeCenter(&cntr);
    ptr->sub = centers; ptr->log = selfSmart(debug?"Argv":0);
    centerPlace(ptr);}
    else if (hideMachine(&mchn, argv[i], &msiz)) {
    machineSwitch(&mchn); freeMachine(&mchn);}
    else if (hideExpress(&expr, argv[i], &esiz)) {
    machineVoid(&expr); freeExpress(&expr);}
    else if (hideStr(&str,argv[i],&ssiz)) {
    planePutstr(str); freeStr(&str,1);}
    else {fprintf(stderr,"Argument:%d Center:%d Machine:%d Express:%d Str:%d unmatched:%s\n",asiz,csiz,msiz,esiz,ssiz,argv[i]); exit(-1);}}
}

void initSafe()
{
    if (!(copySem = allocSafe(1))) ERROR(); // protect array of Center
    if (!(pipeSem = allocSafe(1))) ERROR(); // protect internal and response queues
    if (!(stdioSem = allocSafe(1))) ERROR(); // protect planeConsole queues
    if (!(pressSem = allocSafe(1))) ERROR(); // protect glfw queues
    if (!(timeSem = allocSafe(1))) ERROR(); // protect planeTime queue
    if (!(evalSem = allocSafe(1))) ERROR(); // protect data evaluation
    if (!(safeSem = allocSafe(1))) ERROR(); // protect thread semaphores
    if (!(loopSem = allocSafe(1))) ERROR(); // protect field pipe
    internal = allocCenterq(); response = allocCenterq(); replace = allocCenterq();
    strout = allocStrq(); strin = allocStrq(); tempq = allocChrq();
    charq = allocIntq(); leftq = allocIntq(); baseq = allocIntq(); angleq = allocIntq();
    timeq = allocTimeq(); wakeq = allocIntq(); timep = allocTimep();
    ableq = allocIntq(); maskq = allocIntq(); loopfd = openPipe(); 
    callBack(RegisterCall,registerCall);
    callBack(RegisterOpen,registerOpen);
    callBack(RegisterWake,registerWake);
    callBack(RegisterAble,registerAble);
    callBack(RegisterTime,registerTime);
    callBack(RegisterExit,registerExit);
    callBack(RegisterVerb,registerVerb);
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
    callBack(ArgumentInp,registerArgument);
    callBack(ArgumentOut,registerArgument);
    callBack(ArgumentSrc,registerArgument);
    callBack(PressKey,registerChar);
    callBack(PressQueue,registerChars);
    callBack(ClickLeft,registerClick);
    callBack(ClickBase,registerClick);
    callBack(ClickAngle,registerClick);
    callBack(ClickQueue,registerClicks);
    callBack(ManipLeft,registerMove);
    callBack(ManipBase,registerMove);
    callBack(ManipAngle,registerRoll);
    callBack(CenterLog,registerLog);
    datxFnptr(planeRetcfg,planeSetcfg,planeWoscfg,planeWoccfg,planeRawcfg,planeGetstr,planePutstr,planeField);
    start = processTime();
}
void initBoot()
{
    int size = 0; int cmnd = 0;
    for (int i = 0; callCmnd(i); i++) {size++; cmnd++;}
    for (int i = 0; Bootstrap__Int__Str(i); i++) size++;
    const char **temp = malloc(size*sizeof(const char *)); size = 0;
    for (int i = 0; callCmnd(i); i++) temp[size++] = callCmnd(i);
    for (int i = 0; Bootstrap__Int__Str(i); i++) temp[size++] = Bootstrap__Int__Str(i);
    char **boot = malloc(size*sizeof(char *));
    for (int i = 0; i < size; i++) {
    int len = strlen(temp[i]);
    boot[i] = malloc(len+1);
    strncpy(boot[i],temp[i],len); boot[i][len] = 0;}
    // change strings according to sugar
    for (int i = 0; i < size; i++) {
    sugarRepl(&boot[i],'$'); // replace $() by Express
    sugarEval(planeSugar,boot[i],'!'); // evaluate !() in the embedding
    sugarFilt(&boot[i],'!');} // filter out !() before hide and process below
    // record which kinds of boot strings there are
    for (int i = 0; i < cmnd; i++) {
    int asiz = 0; int csiz = 0; int msiz = 0; int esiz = 0; int ssiz = 0;
    struct Argument arg = {0}; struct Center cntr = {0}; struct Machine mchn = {0};
    struct Express expr = {0}; char *str = 0;
    if (hideArgument(&arg, boot[i], &asiz)) {planeInfo(RegisterShow,1,planeWots); freeArgument(&arg);}
    else if (hideCenter(&cntr, boot[i], &csiz)) {planeInfo(RegisterShow,2,planeWots); freeCenter(&cntr);}
    else if (hideMachine(&mchn, boot[i], &msiz)) {planeInfo(RegisterShow,4,planeWots); freeMachine(&mchn);}
    else if (hideExpress(&expr, boot[i], &esiz)) {planeInfo(RegisterShow,8,planeWots); freeExpress(&expr);}
    else if (hideStr(&str,boot[i],&ssiz)) {planeInfo(RegisterShow,16,planeWots); freeStr(&str,1);}
    else {fprintf(stderr,"Argument:%d Center:%d Machine:%d Express:%d Str:%d unmatched:%s\n",asiz,csiz,msiz,esiz,ssiz,boot[i]); exit(-1);}}
    // Bootstrap first to initialize RegisterPlan
    planeArgv(size-cmnd,boot+cmnd);
    switch (planeInfo(RegisterPlan,0,planeRcfg)) {default: ERROR();
    break; case (Bringup): case (Builtin):
    planeJnfo(RegisterPoll,1,planeWcfg);
    planeJnfo(RegisterMain,planeSugval("@machine"),planeWcfg);
    planeJnfo(RegisterAble,(((1<<DoneMsk)<<8)|0),planeWcfg);
    planeJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    planeJnfo(RegisterOpen,(1<<MachThd),planeWots);
    planeJnfo(RegisterOpen,(1<<PipeThd),planeWots);
    planeJnfo(RegisterOpen,(1<<StdioThd),planeWots);
    planeJnfo(RegisterOpen,(1<<TimeThd),planeWots);
    planeJnfo(RegisterTime,1000<<8,planeWcfg);
    break; case (Regress): case (Release):
    planeJnfo(RegisterMain,planeSugval("@machine"),planeWcfg);
    planeJnfo(RegisterAble,((((1<<SlctMsk)|(1<<DoneMsk))<<8)|0),planeWcfg);
    // the RegisterAble mask of events remembered per indicated MachThd wakes up the thread upon wos of event mask to RegisterWake
    planeJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    planeJnfo(RegisterOpen,(1<<MachThd),planeWots);
    planeJnfo(RegisterOpen,(1<<PipeThd),planeWots);
    planeJnfo(RegisterOpen,(1<<StdioThd),planeWots);}
    // callCmnd strings after so threads are started
    planeArgv(cmnd,boot);
}
void initTest()
{
    int debug = 0;
    const struct Vertex vertices[] = {
        {{-0.5f, -0.5f, 0.40f, 1.0f}, {1.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, -0.5f, 0.40f, 1.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, 0.5f, 0.40f, 1.0f}, {0.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{-0.5f, 0.5f, 0.40f, 1.0f}, {1.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        //
        {{-0.5f, -0.5f, 0.50f, 1.0f}, {1.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, -0.5f, 0.50f, 1.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{0.5f, 0.5f, 0.50f, 1.0f}, {0.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        {{-0.5f, 0.5f, 0.50f, 1.0f}, {1.0f, 1.0f, 0.0f, 0.0f}, {0, 0, 0, 0}},
        //
    };
    const uint32_t primitive[] = {
        3, 3, 3, 3,
        4, 4, 4, 4,
    };
    const uint16_t indices[] = {
        0, 1, 2, 2, 3, 0,
        4, 5, 6, 6, 7, 4,
    };
    planeInfo(FetchBase,0,planeWcfg);
    planeInfo(FetchSize,sizeof(indices)/sizeof(int16_t),planeWcfg);
    int mode = false;
    switch (planeInfo(RegisterPlan,0,planeRcfg)) {
    default: ERROR();

    break; case (Bringup): mode = true; case (Builtin): {
    int frames = planeInfo(ScratchFrames,0,planeRcfg);

    int test = selfSmart("Init");
    printfSmart(test,"test %d",test);
    deleteSmart(test);

    struct Extend *ptr = centerPull(Drawz,(debug?"Init0":0)); freeCenter(ptr->ptr);
    ptr->ptr->mem = Drawz; ptr->ptr->siz = 1;
    allocDraw(&ptr->ptr->drw,ptr->ptr->siz);
    ptr->ptr->drw[0].con.tag = ResrcCon;
    ptr->ptr->drw[0].con.res = SwapRes;
    ptr->sub = Drawz; ptr->rsp = RptRsp;
    callCopy(ptr,0,(debug?"swap":0));
    while (!centerCheck(Drawz)) usleep(1000);
    // UniformWid and UniformHei set by swap resize
    int width,height; {enum Configure cfg[2] = {UniformWid,UniformHei}; int val[2] = {0,0};
    callInfo(cfg,val,2,planeRcfg); width = val[0]; height = val[1];}

    ptr = centerPull(Drawz,(debug?"Init1":0)); freeCenter(ptr->ptr);
    ptr->ptr->mem = Drawz; ptr->ptr->siz = Micros;
    allocDraw(&ptr->ptr->drw,ptr->ptr->siz);
    for (int i = 0; i < Micros; i++) {
    ptr->ptr->drw[i].con.tag = ResrcCon;
    ptr->ptr->drw[i].con.res = PipeRes;
    int arg[] = {/*IDerIns*/i,/*Micro*/i};
    ptr->ptr->drw[i].siz = sizeof(arg)/sizeof(int);
    allocInt(&ptr->ptr->drw[i].arg,ptr->ptr->drw[i].siz);
    for (int j = 0; j < ptr->ptr->drw[i].siz; j++) {
    ptr->ptr->drw[i].arg[j] = arg[j];}}
    ptr->sub = Drawz; ptr->rsp = MptRsp;
    callCopy(ptr,0,(debug?"pipe":0));
    while (!centerCheck(Drawz)) usleep(1000);

    for (int i = 0; i < frames; i++) {
    struct Extend *ptr = centerPull(Drawz,(debug?"Init2":0)); freeCenter(ptr->ptr);
    ptr->ptr->mem = Drawz; ptr->ptr->siz = 1;
    allocDraw(&ptr->ptr->drw,ptr->ptr->siz);
    ptr->ptr->drw[0].con.tag = ResrcCon;
    ptr->ptr->drw[0].con.res = ChainRes;
    ptr->sub = Drawz; ptr->rsp = RptRsp;
    callCopy(ptr,0,(debug?"chain":0));
    while (!centerCheck(Drawz)) usleep(1000);}

    struct Extend *uni = centerPull(Uniformz,(debug?"Init3":0)); freeCenter(uni->ptr);
    uni->ptr->mem = Uniformz; uni->ptr->siz = 1; allocUniform(&uni->ptr->uni,uni->ptr->siz);
    uni->ptr->uni[0].all = 0; uni->ptr->uni[0].one = 1; uni->ptr->uni[0].pro = 2;
    uni->ptr->uni[0].wid = width; uni->ptr->uni[0].hei = height;
    uni->sub = Uniformz; uni->rsp = RptRsp;
    callCopy(uni,0,(debug?"uniform":0));
    {enum Configure cfg[2] = {UniformWid,UniformHei}; int val[2] = {width,height}; callJnfo(cfg,val,2,planeWcfg);}

    struct Extend *img = centerPull(Imagez,(debug?"Init4":0)); freeCenter(img->ptr);
    img->ptr->mem = Imagez; img->ptr->siz = 1; allocImage(&img->ptr->img,img->ptr->siz);
    fmtxStbi(&img->ptr->img[0].dat,&img->ptr->img[0].wid,&img->ptr->img[0].hei,&img->ptr->img[0].cha,"texture.jpg");
    img->sub = Imagez; img->rsp = RptRsp;
    callCopy(img,0,(debug?"image":0));

    struct Extend *sto = centerPull(Storagez,(debug?"Init5":0)); freeCenter(sto->ptr);
    sto->ptr->mem = Storagez; sto->ptr->siz = 1; allocInt32(&sto->ptr->sto,sto->ptr->siz);
    sto->ptr->sto[0] = 456;
    sto->sub = Storagez; sto->rsp = RptRsp;
    callCopy(sto,0,(debug?"storage":0));

    for (int i = 0; i < frames; i++) {
    struct Extend *mat = centerPull(Matrixz,(debug?"Init6":0)); freeCenter(mat->ptr);
    mat->ptr->mem = Matrixz; mat->ptr->siz = 5; allocMatrix(&mat->ptr->mat,mat->ptr->siz);
    float ident[16]; identmat(ident,4);
    float proj[16]; planeWindow(proj);
    copymat(mat->ptr->mat[0].mat,ident,4); // uni.all
    copymat(mat->ptr->mat[1].mat,ident,4); // uni.one
    copymat(mat->ptr->mat[2].mat,proj,4);  // uni.pro
    copymat(mat->ptr->mat[3].mat,ident,4); // tri.pol
    copymat(mat->ptr->mat[4].mat,ident,4); // tri.pol
    mat->sub = Matrixz; mat->rsp = RptRsp;
    callCopy(mat,0,(debug?"initmat":0));
    while (!centerCheck(Matrixz)) callWait();}

    struct Extend *bup = centerPull(Bringupz,(debug?"Init7":0)); freeCenter(bup->ptr);
    bup->ptr->mem = Bringupz; bup->ptr->siz = sizeof(vertices)/sizeof(struct Vertex); allocVertex(&bup->ptr->ver,bup->ptr->siz);
    for (int i = 0; i < bup->ptr->siz; i++) memcpy(&bup->ptr->ver[i],&vertices[i],sizeof(struct Vertex));
    bup->sub = Bringupz; bup->rsp = RptRsp;
    callCopy(bup,0,(debug?"bringup":0));

    struct Extend *idt = centerPull(Identz,(debug?"Init8":0)); freeCenter(idt->ptr);
    idt->ptr->mem = Identz; idt->ptr->siz = sizeof(primitive)/sizeof(uint32_t); allocInt32(&idt->ptr->idt,idt->ptr->siz);
    for (int i = 0; i < idt->ptr->siz; i++) memcpy(&idt->ptr->idt[i],&primitive[i],sizeof(uint32_t));
    idt->sub = Identz; idt->rsp = RptRsp;
    callCopy(idt,0,(debug?"ident":0));

    struct Extend *ind = centerPull(Indexz,(debug?"Init9":0)); freeCenter(ind->ptr);
    ind->ptr->mem = Indexz; ind->ptr->siz = sizeof(indices)/sizeof(int32_t); allocInt32(&ind->ptr->ind,ind->ptr->siz);
    memcpy(ind->ptr->ind,indices,sizeof(indices)); // note that two int16_t are packed into each int32_t; don't care
    ind->sub = Indexz; ind->rsp = RptRsp;
    callCopy(ind,0,(debug?"index":0));

    struct Extend *vtx = centerPull(Vertexz,(debug?"Init10":0)); freeCenter(vtx->ptr);
    vtx->ptr->mem = Vertexz; vtx->ptr->siz = sizeof(vertices)/sizeof(struct Vertex); allocVertex(&vtx->ptr->vtx,vtx->ptr->siz);
    for (int i = 0; i < vtx->ptr->siz; i++) memcpy(&vtx->ptr->vtx[i],&vertices[i],sizeof(struct Vertex));
    // for (int i = 4; i < 8; i++) vtx->ptr->vtx[i].vec[2] = 0.9;
    vtx->sub = Vertexz; vtx->rsp = RptRsp;
    callCopy(vtx,0,(debug?"vertex":0));

    struct Extend *tri = centerPull(Trianglez,(debug?"Init11":0)); freeCenter(tri->ptr);
    tri->ptr->mem = Trianglez; tri->ptr->siz = (sizeof(indices)/sizeof(uint16_t))/3; allocTriangle(&tri->ptr->tri,tri->ptr->siz);
    for (int i = 0; i < tri->ptr->siz; i++) for (int j = 0; j < 3; j++) {
    int ind = j+i*3; if ((ind/3)/2 != i/2) ERROR(); // three indices per triangle, two triangles per polytope
    tri->ptr->tri[i].vtx[j] = indices[ind]; tri->ptr->tri[i].tex = i/2; tri->ptr->tri[i].pol = (i/2?4:3);}
    /*for (int i = 0; i < tri->ptr->siz; i++) {
    fprintf(stderr,"triangle number:%d texture:%d polytope:%d\n",i,tri->ptr->tri[i].tex,tri->ptr->tri[i].pol);
    for (int j = 0; j < 4; j++) {fprintf(stderr,"corner number:%d",tri->ptr->tri[i].vtx[j]);
    fprintf(stderr," %f",vtx->ptr->vtx[tri->ptr->tri[i].vtx[j]].vec[0]);
    for (int k = 1; k < 4; k++) fprintf(stderr,"/%f",vtx->ptr->vtx[tri->ptr->tri[i].vtx[j]].vec[k]);
    fprintf(stderr,"\n");}}*/
    tri->sub = Trianglez; tri->rsp = RptRsp;
    callCopy(tri,0,(debug?"triangle":0));

    int giv[] = {width,height};
    int giw[] = {0,12}; // idx,siz
    for (int i = 0; i < 2; i++) {
    struct Extend *fil = centerPull(Drawz,(debug?"Init12":0)); freeCenter(fil->ptr);
    fil->ptr->mem = Drawz; fil->ptr->siz = 1; allocDraw(&fil->ptr->drw,fil->ptr->siz);
    fil->ptr->drw[0].con.tag = MicroCon;
    fil->ptr->drw[0].con.mic = (i?(mode?MicroFetRel:MicroVtxRel):MicroFilRel);
    fil->ptr->drw[0].siz = sizeof((i?giw:giv))/sizeof(int);
    allocInt(&fil->ptr->drw[0].arg,fil->ptr->drw[0].siz);
    for (int j = 0; j < fil->ptr->drw[0].siz; j++) fil->ptr->drw[0].arg[j] = (i?giw:giv)[j];
    fil->sub = Drawz; fil->rsp = RetRsp;
    callCopy(fil,0,(debug?"relate":0));
    while (!centerCheck(Drawz)) callWait();}

    planeJnfo(RegisterOpen,(1<<TestThd),planeWots);}

    break; case(Regress): case(Release): break;}
}

void planeInit(uftype copy, wftype cont, nftype call, vftype fork, zftype gnfo, zftype info, zftype jnfo, zftype knfo, bftype hnfo, oftype cmnd, aftype wait, aftype wake)
{
    callCopy = copy;
    callCont = cont;
    callBack = call;
    callFork = fork;
    callGnfo = gnfo;
    callInfo = info;
    callJnfo = jnfo;
    callKnfo = knfo;
    callHnfo = hnfo; // TODO is this needed? can expression extensions evaluate expressions?
    callCmnd = cmnd;
    callWait = wait;
    callWake = wake;
    initSafe();
    initBoot();
    initTest();
}
int planeLoop()
{
    int fever = 0;
    switch (planeInfo(RegisterPlan,0,planeRcfg)) {default: break;
    break; case (Bringup): case (Builtin):
    if (fever || (processTime()-start)*1000 < 2000) return 1;
    break; case (Regress): case (Release):
    if (planeInfo(RegisterExit,0,planeRcfg) == 0) return 1;}
    return 0;
}
void planeDone()
{
    clearSmart();
    doneSmart(); // destructor for SlogState happens after destructor for ThreadState
    // TODO stop all the threads
    // TODO free heap allocations
}
