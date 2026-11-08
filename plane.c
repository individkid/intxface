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

struct Extend **center = 0; // mostly for planeMachine
int centers = 0; // mostly for planeMachine
struct Extend *local[Threads][3][3] = {0};
void *machque[Threads][3] = {0};
// struct Extend ***local[Threads] = 0; // per thread per instance centers
// int *locals[Threads] = 0; // centers per thread per instance
// int threads[Threads] = 0; // number of center threads
void *copySem = 0; // protect centers
int extdone = 0; // done for planeExternal
int external = 0; // pipes to planeExternal
int inverse[Programs] = {0}; // inverse to userIdent
void *internal = 0; // queue of center
void *response = 0; // queue of center
void *replace = 0; // queue of center
void *pipeSem = 0; // protect internal response replace
void *callSem = 0; // protect external inverse
// Configure inside of pipeSem will deadlock if callback waits on pipeSem.
// Nested semaphores are fine if they are nested in the same order in every thread.
void *machSem = 0; // protect threaded expressions
int console = 0; // pipe to planeConsole
int condone = 0; // done for planeConsole
void *strin = 0; // queue of string
void *strout = 0; // queue of string
void *stdioSem = 0; // protect strin and strout
int level[Masks] = {0}; // level sensitive interrupts
void *ableq = 0; // event to virt mask
void *evntq = 0; // virt to event mask
void *virtq = 0; // thread/subthread to virt
void *backq = 0; // virt to thread/subthread
void *timeq = 0; // queue of wakeup times
void *wakeq = 0; // queue of wakeup threads
void *timep = 0; // map from thread to time
void *timeSem = 0; // protect wakeup queues
void *charq = 0; // queue of keyboard presses
void *leftq = 0; // queue of mouse presses
void *baseq = 0; // queue of mouse presses
void *angleq = 0; // queue of mouse presses
void *pressSem = 0; // protect press queues
void **wakeSem[Threads] = {0}; // for event driven threads
int sizeSem[Threads] = {0}; // number of thread instances
int *protocol[Threads] = {0}; // mask per thread instance
int sizeMch = 0; // same as sizeSem[MachThd] after safeMach
struct Reboot **reboot = 0; // initializations done in a MachThd
struct Extend ***recent = 0; // resources for initialization
int *resize = 0; // number of initializations
void *safeSem = 0; // protect reboot recent resize and wakeSem
// following initialized before threads so safe
void *tempq = 0; // temporary queue to convert chars to str
void *evalSem = 0; // protect state in expressions
int safeEot = 0;
int pipeEot = 0;
int copyEot = 0;
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

// simple registers access; for structured registers, see "resource access" below
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
int planeHnfo(enum Configure cfg, int val, yftype fnc)
{
    return (callHnfo()?planeKnfo:planeJnfo)(cfg,val,fnc);
}
int planeFnfo(enum Configure cfg, int val, yftype fnc)
{
    return (callHnfo()?planeGnfo:planeInfo)(cfg,val,fnc);
}

// thread sharing
void safeInit(enum Thread thd, int siz)
{
    waitSafe(safeSem);
    if (siz <= sizeSem[thd]) {postSafe(safeSem); return;}
    void **temp = malloc(sizeof(void*)*siz);
    int *prot = malloc(sizeof(int)*siz);
    for (int i = 0; i < sizeSem[thd]; i++) temp[i] = wakeSem[thd][i];
    for (int i = sizeSem[thd]; i < siz; i++) temp[i] = allocSafe(0);
    for (int i = 0; i < sizeSem[thd]; i++) prot[i] = protocol[thd][i];
    for (int i = sizeSem[thd]; i < siz; i++) prot[i] = 0;
    free(wakeSem[thd]); wakeSem[thd] = temp;
    free(protocol[thd]); protocol[thd] = prot;
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
    if (safeEot) return -1;
    int *idx = (int*)arg;
    return (!resize[*idx]);
}
int safeGunc(void *arg)
{
    if (safeEot) return -1;
    int *idx = (int*)arg;
    return (resize[*idx]);
}
void safeMach(int idx, struct Reboot *boot, struct Extend **cent, int siz)
{
    int mch = idx+1;
    safeInit(MachThd,idx+1);
    waitSafe(safeSem);
    if (mch > sizeMch) {
    struct Reboot **temr = malloc(sizeof(struct Reboot*)*mch);
    struct Extend ***tems = malloc(sizeof(struct Extend**)*mch);
    int *temt = malloc(sizeof(int)*mch);
    for (int i = 0; i < sizeMch; i++) {
    temr[i] = reboot[i]; tems[i] = recent[i]; temt[i] = resize[i];}
    for (int i = sizeMch; i < mch; i++) {
    temr[i] = 0; tems[i] = 0; temt[i] = 0;}
    free(reboot); free(recent); free(resize);
    reboot = temr; recent = tems; resize = temt; sizeMch = mch;}
    postSafe(safeSem);
    if (funcSafe(safeSem,safeFunc,&idx) != 0) ERROR();
    free(reboot[idx]); reboot[idx] = (struct Reboot *)malloc(sizeof(struct Reboot*)*siz); for (int i = 0; i < siz; i++) reboot[idx][i] = boot[i];
    free(recent[idx]); recent[idx] = (struct Extend **)malloc(sizeof(struct Extend *)*siz); for (int i = 0; i < siz; i++) recent[idx][i] = cent[i];
    resize[idx] = siz;
    if (postSafe(safeSem) != 1) ERROR();
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
float *planeMouseRotateCursor(float *mat, float *fix, float *nml, float *org, float *cur)
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
float *planeMouseRotateNormal(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // tip by angle A=(fix+nml) B=fix C=(fix+nml+(cur-org)); line through B, perpendicular to plane containing A B C, is fixed.
    // TODO
}
float *planeMouseRotateOrtho(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // tip by angle A=(fix-{0,0,fix[2]}) B=fix C=(fix-{0,0,fix[2]}+(cur-org)); line through B, perpendicular to plane containing A B C, is fixed.
    // TODO
}
float *planeMouseSlideOrtho(float *mat, float *fix, float *nrm, float *org, float *cur)
{
    // slide by (cur-org) leaving [2] fixed.
    float v[4]; zerovec(v,4); v[0] = cur[0]-org[0]; v[1] = cur[1]-org[1];
    float h0[4], h1[4]; zerovec(h0,3); h0[3] = 1.0; plusvec(copyvec(h1,h0,4),v,4);
    float i0[4], i1[4]; unitvec(i0,3,0); i0[3] = 1.0; plusvec(copyvec(i1,i0,4),v,4);
    float j0[4], j1[4]; unitvec(j0,3,1); j0[3] = 1.0; plusvec(copyvec(j1,j0,4),v,4);
    float k0[4], k1[4]; unitvec(k0,3,2); k0[3] = 1.0; plusvec(copyvec(k1,k0,4),v,4);
    return planeTransform(mat,h0,h1,i0,i1,j0,j1,k0,k1);
}
float *planeMouseSlideNormal(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // slide parallel to nml by dot(nml,(cur-fix))-dot(nml,(org-fix))
    // TODO
}
float *planeRollerRotateCursor(float *mat, float *fix, float *nml, float *org, float *cur)
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
float *planeRollerRotateFocal(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // rotate by cur[2]-org[2] angle, keeping line from fix to focal point fixed
    // TODO
}
float *planeRollerSlideOrtho(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // change [2] by cur[2]-org[2]
    // TODO
}
float *planeRollerSlideNormal(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // slide parallel to nml by cur[2]-org[2]
    // TODO
}
float *planeRollerScalePierce(float *mat, float *fix, float *nml, float *org, float *cur)
{
    // multiply distance from fix by log(cur[2]-org[2])
    // TODO
}
typedef float *(*planeXform)(float *mat, float *fix, float *nrm, float *org, float *cur);
float *planeMatrix(float *mat)
{
    planeXform fnc = 0; int tmp; int cfg = planeInfo(ManipFixed,0,planeRcfg);
    tmp = ((1<<Mouse)|(1<<Rotate)|(1<<Cursor)); if ((cfg&tmp)==tmp) fnc = planeMouseRotateCursor;
    tmp = ((1<<Mouse)|(1<<Rotate)|(1<<Normal)); if ((cfg&tmp)==tmp) fnc = planeMouseRotateNormal;
    tmp = ((1<<Mouse)|(1<<Rotate)|(1<<Ortho)); if ((cfg&tmp)==tmp) fnc = planeMouseRotateOrtho;
    tmp = ((1<<Mouse)|(1<<Slide)|(1<<Ortho)); if ((cfg&tmp)==tmp) fnc = planeMouseSlideOrtho;
    tmp = ((1<<Mouse)|(1<<Slide)|(1<<Normal)); if ((cfg&tmp)==tmp) fnc = planeMouseSlideNormal;
    tmp = ((1<<Roller)|(1<<Rotate)|(1<<Cursor)); if ((cfg&tmp)==tmp) fnc = planeRollerRotateCursor;
    tmp = ((1<<Roller)|(1<<Rotate)|(1<<Focal)); if ((cfg&tmp)==tmp) fnc = planeRollerRotateFocal;
    tmp = ((1<<Roller)|(1<<Slide)|(1<<Ortho)); if ((cfg&tmp)==tmp) fnc = planeRollerSlideOrtho;
    tmp = ((1<<Roller)|(1<<Slide)|(1<<Normal)); if ((cfg&tmp)==tmp) fnc = planeRollerSlideNormal;
    tmp = ((1<<Roller)|(1<<Scale)|(1<<Pierce)); if ((cfg&tmp)==tmp) fnc = planeRollerScalePierce;
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
    enum Configure cfg[5] = {UniformWid,UniformHei,FocalFull,FocalDepth,FocalSlope};
    int val[5] = {0,0,0,0,0};
    callInfo(cfg,val,5,planeRcfg);
    float width = val[0];
    float height = val[1];
    float full = val[2];
    float length = val[3];
    float slope = val[4];
    // length is from focal point to zero z. slope is change of w per unit z.
    // w = a + bz, y'=y/w, x'=x/w; fullscreen is a at z=0; fullscreen is 1 at z=(1-a)/b
    // slope = a/length, slope = 1/(length+(1-a)/b); a=slope*length, slope*(length+(1-a)/b)=1;
    // slope*length*b+slope*(1-a)=b; slope*(1-a)=b-b*slope*length; b=slope*(1-a)/(1-slope*length);
    // b=slope*(1-slope*length)/(1-slope*length); a=slope*length, b=slope;
    *matrc(mat,3,2,4) = slope/full; // row major; row number 3; column number 2
    *matrc(mat,3,3,4) = slope*length/(full*full);
    *matrc(mat,0,0,4) = full/width; // x'=x*full/width
    *matrc(mat,1,1,4) = full/height; // y'=y*full/height
    return mat;
}

// resource access
void centerSmart(struct Extend *ext, const char *log)
{
    char *st0 = 0; showMemory(ext->ptr->mem,&st0);
    if (ext->ptr->mem == Transferz) {char *st1 = 0;
    showTransfer(ext->ptr->exe->xfr,&st1);
    printfSmart(ext->log,"%s %s %s",log,st0,st1);
    free(st1);} else if (ext->ptr->mem == Drawz) {char *st1 = 0;
    showConst(&ext->ptr->drw->con,&st1);
    printfSmart(ext->log,"%s %s %s %d/%d",log,st0,st1,ext->ptr->idx,ext->ptr->siz);} else {
    printfSmart(ext->log,"%s %s %d %d/%d",log,st0,ext->loc.sub,ext->ptr->idx,ext->ptr->siz);}
}
void centerSize(struct Reboot loc, enum Thread tag, int idx)
{
    if (waitSafe(copySem) != 0) ERROR();
    if (tag < 0 || tag >= Threads) ERROR();
    if (loc.asr == LocalAsr) {
    if (idx < 0 || idx >= 3) ERROR(); // TODO make local infinite
    /*TODO allocate local[tag], locals[tag], threads[tag]*/}
    if (loc.sub >= centers) {int size = loc.sub+1;
    center = realloc(center,size*sizeof(struct Extend *));
    for (int i = centers; i < size; i++) center[i] = 0; centers = size;}
    if (postSafe(copySem) != 1) ERROR();
}
void centerQues(enum Thread tag, int idx)
{
    if (idx < 0 || idx >= 3) ERROR(); // TODO also make machque monotonically dynamic
    if (waitSafe(pipeSem) != 0) ERROR();
    if (machque[tag][idx] == 0) machque[tag][idx] = allocCenterq();
    if (postSafe(pipeSem) != 1) ERROR();
}
void planeProt(enum Thread tag, int idx);
int centerPro(enum Thread thd, int idx, enum Protocol pro)
{
    safeInit(thd,idx+1);
    planeProt(thd,idx);
    if (waitSafe(safeSem) != 0) ERROR();
    int ret = (0 != (protocol[thd][idx] & (1<<pro)));
    if (postSafe(safeSem) != 1) ERROR();
    return ret;
}
struct Return {
    struct Extend *ptr;
    int siz;
    int vld;
};
enum Protocol centerRepro(enum Transfer mod)
{
    enum Protocol pro;
    switch (mod) {default: pro = PullPro;
    break; case(Sage): pro = PrmwPro;
    break; case(Chck): pro = NonePro;
    break; case(Move): pro = PrnwPro;
    break; case(Copy): pro = PrnwPro;
    break; case(Tend): pro = PrmwPro;
    } return pro;
}
enum Protocol centerReque(enum Transfer mod)
{
    enum Protocol pro;
    switch (mod) {default: ERROR();
    break; case(Sage): pro = QrmwPro;
    break; case(Chck): pro = NonePro;
    break; case(Move): pro = QrnwPro;
    break; case(Copy): pro = QrnwPro;
    break; case(Tend): pro = QrmwPro;
    } return pro;
}
int centerRemod(enum Transfer mod)
{
    int ret;
    switch (mod) {default: ret = 1;
    break; case (Sage): ret = 1;
    break; case (Chck): ret = 0;
    break; case (Move): ret = 1;
    break; case (Copy): ret = 0;
    break; case (Tend): ret = 1;
    } return ret;
}
int centerFunc(void *arg)
{
    if (copyEot) return -1;
    struct Extend **center = (struct Extend **)arg;
    return (*center != 0);
}
struct Return centerPull(int sub, enum Transfer mod, enum Thread tag, int idx, const char *log)
{
    struct Reboot loc = {sub<0?-1-sub:sub,sub<0?LocalAsr:PlaceAsr};
    int pro = centerPro(tag,idx,centerRepro(mod));
    centerSize(loc,tag,idx);
    struct Extend **arg = 0;
    if (loc.asr == LocalAsr) {
    if (tag < 0 || tag >= Threads) ERROR();
    if (idx < 0 || idx >= 3 || loc.sub < 0 || loc.sub >= 3) ERROR();
    arg = local[tag][idx]+loc.sub;}
    else if (loc.asr == PlaceAsr) {
    if (centers <= loc.sub) ERROR();
    arg = center+loc.sub;}
    else ERROR();
    if (pro && tag == MachThd) postSafe(machSem);
    if (pro) {if (funcSafe(copySem,centerFunc,(void*)arg) < 0) {
    if (tag == MachThd) waitSafe(machSem);
    struct Return ret = {0,0,-1}; return ret;}}
    else {if (waitSafe(copySem) != 0) ERROR();}
    struct Extend *ptr = *arg;
    if (centerRemod(mod)) *arg = 0;
    if (postSafe(copySem) != 1) ERROR();
    if (pro && tag == MachThd) waitSafe(machSem);
    if (ptr == 0 && pro) ERROR();
    if (ptr == 0) {
    struct Return ret = {0,0,0}; return ret;}
    if (sub >= 0 && ptr->loc.asr != PlaceAsr) ERROR();
    else if (sub < 0 && ptr->loc.asr != LocalAsr) ERROR();
    int tmp = planeFnfo(RegisterLog,0,planeRcfg);
    int vrb = ((planeFnfo(RegisterVerb,0,planeRcfg)&(1<<PullVrb)) != 0);
    if (tmp) {deleteSmart(ptr->log); ptr->log = otherSmart(tmp);}
    else if (vrb) {deleteSmart(ptr->log); ptr->log = selfSmart(log);}
    else {deleteSmart(ptr->log); ptr->log = noneSmart();}
    struct Return ret = {ptr,1,0}; return ret;
}
int centerGunc(void *arg)
{
    if (pipeEot) return -1;
    return (sizeCenterq(arg) > 0);
}
struct Return centerDeque(void *que, enum Transfer mod, enum Thread tag, int idx, const char *log)
{
    int pro = centerPro(tag,idx,centerReque(mod));
    if (pro && tag == MachThd) postSafe(machSem);
    if (pro) {if (funcSafe(pipeSem,centerGunc,que) < 0) {
    if (tag == MachThd) waitSafe(machSem);
    struct Return ret = {0,0,-1}; return ret;}}
    else {if (waitSafe(pipeSem) != 0) ERROR();}
    struct Extend *ptr = (centerRemod(mod) ? maybeCenterq(0,que) : frontCenterq(que));
    int siz = sizeCenterq(que);
    if (postSafe(pipeSem) != 1) ERROR();
    if (pro && tag == MachThd) waitSafe(machSem);
    struct Return ret = {ptr,siz,0}; return ret;
}
void *centerQueue(enum Thread tag, int idx)
{
    if (tag != MachThd) ERROR(); // TODO use machque instead of internal external replace etc or better yet remove tag arg
    centerQues(tag,idx);
    if (waitSafe(pipeSem) != 0) ERROR();
    void *ret = machque[tag][idx];
    if (postSafe(pipeSem) != 1) ERROR();
    return ret;
}
struct Return centerRepull(struct Reboot src, enum Transfer mod, enum Thread tag, int idx, const char *log)
{
    struct Return ret; if (src.sub < 0) ERROR();
    switch (src.asr) {default: ERROR();
    break; case (PlaceAsr): ret = centerPull(src.sub,mod,tag,idx,log);
    break; case (LocalAsr): ret = centerPull(-1-src.sub,mod,tag,idx,log);
    break; case (PipeAsr): ret = centerDeque(internal,mod,tag,idx,log);
    break; case (RespAsr): ret = centerDeque(response,mod,tag,idx,log);
    break; case (DoneAsr): ret = centerDeque(replace,mod,tag,idx,log);
    break; case (MachAsr): ret = centerDeque(centerQueue(tag,idx),mod,tag,idx,log);}
    if (ret.ptr && ret.ptr->loc.asr != src.asr) ERROR();
    if (ret.ptr == 0 || ret.vld < 0 || centerRemod(mod)) return ret;
    struct Extend *cpy = 0; allocExtend(&cpy,1); copyExtend(cpy,ret.ptr);
    cpy->log = otherSmart(ret.ptr->log);
    ret.ptr = cpy; return ret;
}
void centerPlace(struct Extend *ptr, enum Thread tag, int idx)
{
    if (ptr == 0) return;
    struct Reboot loc = ptr->loc;
    if (loc.asr != LocalAsr && loc.asr != PlaceAsr) ERROR();
    centerSize(loc,tag,idx);
    if (waitSafe(copySem) != 0) ERROR();
    struct Extend **arg = 0;
    if (loc.asr == LocalAsr) {
    if (tag < 0 || tag >= Threads) ERROR();
    if (idx < 0 || idx >= 3 || loc.sub < 0 || loc.sub >= 3) ERROR();
    arg = local[tag][idx]+loc.sub;}
    else if (loc.asr == PlaceAsr) {
    if (centers <= loc.sub) ERROR();
    arg = center+loc.sub;}
    else ERROR();
    struct Extend *tmp = *arg;
    *arg = ptr;
    if (postSafe(copySem) != 1) ERROR();
    if (tmp) {deleteSmart(tmp->log); freeExtend(tmp); allocExtend(&tmp,0);}
}
void centerEnque(void *que, struct Extend *ptr, enum Mask msk, const char *log)
{
    int debug = ((planeInfo(RegisterVerb,0,planeRcfg)&(1<<QueuVrb)) != 0);
    if (debug) centerSmart(ptr,log);
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(ptr,que);
    if (postSafe(pipeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<msk),planeWots);
}
void centerCall(struct Extend *ptr, const char *log)
{
    int debug = ((planeInfo(RegisterVerb,0,planeRcfg)&(1<<QueuVrb)) != 0);
    if (debug) centerSmart(ptr,log);
    callCont(ptr,ptr->loc.sub,ptr->log);
}
void centerReplace(struct Extend *ptr, enum Thread tag, int idx)
{
    if (ptr == 0) return;
    switch (ptr->loc.asr) {default: ERROR();
    break; case (PlaceAsr): centerPlace(ptr,tag,idx);
    break; case (LocalAsr): centerPlace(ptr,tag,idx);
    break; case (PipeAsr): centerEnque(internal,ptr,SlctMsk,"internal");
    break; case (RespAsr): centerEnque(response,ptr,RespMsk,"response");
    break; case (DoneAsr): centerEnque(replace,ptr,DoneMsk,"replace");
    break; case (MachAsr): centerEnque(centerQueue(tag,ptr->loc.sub),ptr,MachMsk,"permach");
    break; case (CallAsr): centerCall(ptr,"gpucall");}
}
void centerDone(struct Extend *ptr)
{
    ptr->loc.asr = DoneAsr; ptr->loc.sub = 0;
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(ptr,replace); 
    if (postSafe(pipeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<DoneMsk),planeWots);
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
void centerProt(enum Thread thd, int idx, int mask)
{
    safeInit(thd,idx+1);
    if (waitSafe(safeSem) != 0) ERROR();
    protocol[thd][idx] = mask;
    if (postSafe(safeSem) != 1) ERROR();
}
int centerMask(enum Thread thd, int idx)
{
    safeInit(thd,idx+1);
    if (waitSafe(safeSem) != 0) ERROR();
    int ret = protocol[thd][idx];
    if (postSafe(safeSem) != 1) ERROR();
    return ret;
}

// expanded minimal experience
int demoJect(struct Menu *menu)
{
    switch (menu->jec) {default: ERROR();
    break; case(Inject): return menu->inj;
    break; case(Object): return menu->obj;
    break; case(Subject): return menu->suj;}
    return 0;
}
void demoWake(struct Menu *menu)
{
    enum Configure cfg = (menu->msk==PrssMsk?PressQueue:ClickQueue);
    if (planeJnfo(cfg,-1,planeRmw) <= 0) ERROR();
}
void demoMenu(struct Menu *menu)
{
    switch (planeInfo(PressKey,0,planeRcfg)) {default:
    // C NOT SenioR Fire DePartment Man
    break; case ('C'): menu->coo = (1<<Mouse)|(1<<Rotate)|(1<<Cursor); // Cursor
    break; case ('N'): menu->coo = (1<<Mouse)|(1<<Rotate)|(1<<Normal); // Normal
    break; case ('O'): menu->coo = (1<<Mouse)|(1<<Rotate)|(1<<Ortho); // Ortho
    break; case ('T'): menu->coo = (1<<Mouse)|(1<<Slide)|(1<<Ortho); // Translate
    break; case ('S'): menu->coo = (1<<Mouse)|(1<<Slide)|(1<<Normal); // Slide
    break; case ('R'): menu->ang = (1<<Roller)|(1<<Rotate)|(1<<Cursor); // Rotate
    break; case ('F'): menu->ang = (1<<Roller)|(1<<Rotate)|(1<<Focal); // Focal
    break; case ('D'): menu->ang = (1<<Roller)|(1<<Slide)|(1<<Ortho); // Depth
    break; case ('P'): menu->ang = (1<<Roller)|(1<<Slide)|(1<<Normal); // Parallel
    break; case ('M'): menu->ang = (1<<Roller)|(1<<Scale)|(1<<Pierce); // Magnify
    break; case ('I'): menu->jec = Inject; // Inject
    break; case ('J'): menu->jec = Object; // obJect
    break; case ('U'): menu->jec = Subject; // sUbject;
    break; case ('A'): menu->act = Additive; // Additive
    break; case ('B'): menu->act = Subtractive; // suBtractive
    break; case ('V'): menu->act = Divisive; // diVisive
    break; case ('Q'): menu->act = Operative; // Query
    break; case ('W'): /*TODO Warp to last metric sent*/}
}
void demoRead(struct Menu *menu, enum Thread tag, int idx) // pull/modify/place Kernelz, pull/read/place Matrixz
{
    struct Extend *src = centerPull(menu->src,Demo,tag,idx,"source").ptr;
    struct Extend *ptr = centerPull(menu->ker,Demo,tag,idx,"read").ptr;
    int slf = src->ptr->slf;
    if (src->ptr->mem != Matrixz) ERROR();
    struct Matrix *mat = src->ptr->mat;
    struct Kernel *ker = &ptr->ptr->ker[demoJect(menu)];
    timesmat(ker->global.mat,mat->mat,4); // G = GM
    if (slf < 0) {float inv[16]; jumpmat(ker->sent.mat,invmat(copymat(inv,mat->mat,4),4),4);} // S = M'S
    centerPlace(src,tag,idx);
    centerPlace(ptr,tag,idx);
}
void demoSend(struct Menu *menu, enum Thread tag, int idx) // pull/read/place Kernelz, alloc/send Matrixz
{
    struct Extend *dst = 0; allocExtend(&dst,1);
    struct Extend *ptr = centerPull(menu->ker,Demo,tag,idx,"send").ptr;
    dst->loc.sub = menu->dst; dst->loc.asr = PlaceAsr;
    freeCenter(dst->ptr); dst->ptr->mem = Matrixz;
    dst->ptr->idx = demoJect(menu); dst->ptr->siz = 1;
    allocMatrix(&dst->ptr->mat,dst->ptr->siz);
    struct Matrix *mat = dst->ptr->mat;
    struct Kernel *ker = &ptr->ptr->ker[demoJect(menu)];
    timesmat(timesmat(timesmat(copymat(mat->mat,ker->global.mat,4),ker->sent.mat,4),ker->local.mat,4),ker->saved.mat,4); // M = GSLT
    callCont(dst,1,dst->log);
    centerPlace(ptr,tag,idx);
}
void demoCont(struct Menu *menu, enum Thread tag, int idx) // pull/modify/place Kernelz
{
    struct Extend *ptr = centerPull(menu->ker,Demo,tag,idx,"cont").ptr;
    struct Kernel *ker = &ptr->ptr->ker[demoJect(menu)];
    float mat[16]; float inv[16]; invmat(copymat(inv,planeMatrix(mat),4),4);
    timesmat(timesmat(ker->local.mat,ker->saved.mat,4),inv,4); // L = LTC'
    copymat(ker->saved.mat,mat,4); // T = C
    centerPlace(ptr,tag,idx);
}
void demoNone(struct Menu *menu, enum Thread tag, int idx) // pull/modify/place Kernelz
{
    struct Extend *ptr = centerPull(menu->ker,Demo,tag,idx,"none").ptr;
    struct Kernel *ker = &ptr->ptr->ker[demoJect(menu)];
    timesmat(ker->local.mat,ker->saved.mat,4); // L = LT
    identmat(ker->saved.mat,4); // T = I
    centerPlace(ptr,tag,idx);
}
void demoPush(struct Menu *menu, enum Thread tag, int idx) // pull/modify/place Kernelz, alloc/push Matrixz
{
    struct Extend *dst = 0; allocExtend(&dst,1);
    struct Extend *ptr = centerPull(menu->ker,Demo,tag,idx,"push").ptr;
    dst->loc.sub = menu->dst; dst->loc.asr = RespAsr;
    freeCenter(dst->ptr); dst->ptr->mem = Matrixz;
    dst->ptr->idx = demoJect(menu); dst->ptr->siz = 1;
    allocMatrix(&dst->ptr->mat,dst->ptr->siz);
    struct Kernel *ker = &ptr->ptr->ker[demoJect(menu)];
    struct Matrix *mat = dst->ptr->mat;
    copymat(mat->mat,ker->local.mat,4); // M = L
    timesmat(ker->sent.mat,ker->local.mat,4); // S = SL
    identmat(ker->local.mat,4); // L = I
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(dst,response);
    if (postSafe(pipeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<RespMsk),planeWots);
    centerPlace(ptr,tag,idx);
    demoWake(menu);
}
void demoMask(struct Menu *menu, enum Thread tag, int idx) // pull/read/place Getoldz\Getintz\Vectorz
{
    int left = planeInfo(ClickLeft,0,planeRcfg);
    int base = planeInfo(ClickBase,0,planeRcfg);
    int width = planeInfo(UniformWid,0,planeRcfg);
    int index = base*width+left;
    struct Extend *src = centerPull(menu->src,Demo,tag,idx,"mask").ptr;
    switch (src->ptr->mem) {default: ERROR();
    break; case (Getoldz): {
        int full = planeInfo(FocalFull,0,planeRcfg);
        int depth = planeInfo(FocalDepth,0,planeRcfg);
        int slope = planeInfo(FocalSlope,0,planeRcfg);
        float b = 1.0*slope/full;
        float a = b*depth/full;
        float x = 1.0*left/full;
        float y = 1.0*base/full;
        float z = src->ptr->old[index];
        planeJnfo(FixedLeft,x*(a+b*z)*full,planeWcfg);
        planeJnfo(FixedBase,y*(a+b*z)*full,planeWcfg);
        planeJnfo(FixedDeep,z*full,planeWcfg);
        menu->met |= (1<<Getoldz);}
    break; case (Getintz):
        planeJnfo(SelectIdent,src->ptr->uns[index],planeWcfg);
        menu->met |= (1<<Getintz);
    break; case (Vectorz):
        planeJnfo(NormalLeft,src->ptr->vec[index].vec[0],planeWcfg);
        planeJnfo(NormalBase,src->ptr->vec[index].vec[1],planeWcfg);
        planeJnfo(NormalDeep,src->ptr->vec[index].vec[2],planeWcfg);
        menu->met |= (1<<Vectorz);}
    centerPlace(src,tag,idx);
}
void demoDone(struct Menu *menu) // maybe alloc/push Metricz
{
    int msk = (1<<Getoldz)|(1<<Getintz)|(1<<Vectorz);
    if ((menu->met&msk)==msk) {
    menu->met = 0; demoWake(menu);
    switch (menu->act) {default: ERROR();
    break; case (Indicate):
        menu->act = Manipulate;
    break; case (Divisive): case (Additive): case (Subtractive): case (Operative):
        // do Jnfo for menu->dst as Metricz to Qopy
        struct Extend *dst = 0; allocExtend(&dst,1);
        dst->loc.sub = menu->dst; dst->loc.asr = RespAsr;
        freeCenter(dst->ptr); dst->ptr->mem = Metricz;
        dst->ptr->idx = demoJect(menu); dst->ptr->siz = 1;
        allocMetric(&dst->ptr->met,dst->ptr->siz);
        struct Metric *met = dst->ptr->met;
        met->act = menu->act;
        met->fix[0] = planeJnfo(FixedLeft,0,planeRcfg);
        met->fix[1] = planeJnfo(FixedBase,0,planeRcfg);
        met->fix[2] = planeJnfo(FixedDeep,0,planeRcfg);
        met->nor[0] = planeJnfo(NormalLeft,0,planeRcfg);
        met->nor[1] = planeJnfo(NormalBase,0,planeRcfg);
        met->nor[2] = planeJnfo(NormalDeep,0,planeRcfg);
        met->act = planeJnfo(SelectIdent,0,planeRcfg);
        if (waitSafe(pipeSem) != 0) ERROR();
        pushCenterq(dst,response);
        if (postSafe(pipeSem) != 1) ERROR();
        planeJnfo(RegisterWake,(1<<RespMsk),planeWots);}}
}
void demoDisp(struct Menu *menu, enum Thread tag, int idx) // pull/send Drawz
{
    struct Extend *drw = centerPull(menu->drw,Demo,tag,idx,"disp").ptr;
    if (drw->ptr->mem != Drawz) ERROR();
    for (int i = 0; i < drw->ptr->siz; i++) drw->ptr->drw[i].siz = -abs(drw->ptr->drw[i].siz);
    int sub[] = {menu->dsp};
    for (int i = 0; i < sizeof(sub)/sizeof(int); i++)
    drw->ptr->drw[i].siz = abs(drw->ptr->drw[i].siz);
    callCont(drw,0,drw->log);
}
void demoPute(struct Menu *menu, enum Thread tag, int idx) // pull/send Drawz
{
    struct Extend *drw = centerPull(menu->drw,Demo,tag,idx,"pute").ptr;
    if (drw->ptr->mem != Drawz) ERROR();
    for (int i = 0; i < drw->ptr->siz; i++) drw->ptr->drw[i].siz = -abs(drw->ptr->drw[i].siz);
    int sub[] = {menu->pie,menu->nor,menu->sel};
    for (int i = 0; i < sizeof(sub)/sizeof(int); i++)
    drw->ptr->drw[i].siz = abs(drw->ptr->drw[i].siz);
    callCont(drw,0,drw->log);
}
void demoSize(struct Menu *menu) // alloc/send Matrixz
{
    struct Extend *dst = 0; allocExtend(&dst,1);
    dst->sav.sub = menu->dst; dst->sav.asr = PlaceAsr;
    dst->loc.sub = 0; dst->loc.asr = CallAsr;
    freeCenter(dst->ptr); dst->ptr->mem = Matrixz;
    dst->ptr->idx = menu->pro; dst->ptr->siz = 1;
    allocMatrix(&dst->ptr->mat,dst->ptr->siz);
    planeWindow(dst->ptr->mat->mat);
    callCont(dst,1,dst->log);
}

// generic callbacks
// following globals protected by evalSem, could move to PlaneField
// currently powerful enough only for structs with only one size and only after the tag
struct PlaneField {
    int num, sub;
};
int changed = 0; enum Memory newmem = Memorys;
int resized = 0; int oldsize = 0; int newsize = 0;
struct MergeEnum centerField(int num, int fld, int sub, int typ, struct MergeStruct *arg)
{
    struct PlaneField *usr = (struct PlaneField*)arg->usr;
    // fprintf(stderr,"Field %d %d %d %d %d %d\n",num,fld,sub,typ,usr->num,usr->sub);
    if (fld == 0) changed = resized = 0;
    switch (num) {default:
    break; case (TYPECenter):
    if (resized && sub >= oldsize) switch (typ) {
    default: return (struct MergeEnum){ZerMrg,0};
    break; case (TYPEMatrix): {
    struct Matrix init;
    identmat(init.mat,4);
    writeMatrix(&init,arg->idx);
    return (struct MergeEnum){IdxMrg,(1<<IdxMrg)};}
    break; case (TYPEKernel): {
    struct Kernel init;
    identmat(init.saved.mat,4);
    identmat(init.local.mat,4);
    identmat(init.sent.mat,4);
    identmat(init.global.mat,4);
    writeKernel(&init,arg->idx);
    return (struct MergeEnum){IdxMrg,(1<<IdxMrg)};}}
    if (resized && sub == newsize-1 && oldsize > newsize) {oldsize -= 1; return (struct MergeEnum){NonMrg,(1<<LftMrg)};}
    // if changed, then size must be zero, so none of the above apply
    if (changed) return (struct MergeEnum){ZerMrg,0};
    if (fld == identField(num,"mem") && fld == usr->num) {
    changed = 1; newmem = readInt(arg->lft); return (struct MergeEnum){RgtMrg,(1<<RgtMrg)};}
    if (fld == identField(num,"siz") && fld == usr->num) {
    resized = 1; oldsize = readInt(arg->lft); newsize = readInt(arg->rgt); writeInt(newsize,arg->idx); return (struct MergeEnum){IdxMrg,(1<<IdxMrg)};}
    break; case (TYPEExtend):
    if (fld == identField(num,"log") && fld == usr->num) {
    int sav = readInt(arg->rgt);
    int tmp = otherSmart(sav);
    writeInt(tmp,arg->idx);
    deleteSmart(sav);
    return (struct MergeEnum){IdxMrg,(1<<LftMrg)|(1<<IdxMrg)};}}
    if (fld == usr->num && sub == usr->sub) return (struct MergeEnum){RgtMrg,(1<<LftMrg)|(1<<RgtMrg)};
    return (struct MergeEnum){LftMrg,(1<<LftMrg)};
}
struct PlaneRange {
    int src, dst, siz;
};
int skipped = 0; int toread = 0;
struct MergeEnum centerRange(int num, int fld, int sub, int typ, struct MergeStruct *arg)
{
    struct PlaneRange *usr = (struct PlaneRange*)arg->usr;
    // fprintf(stderr,"Range %d %d %d %d %d %d %d\n",num,fld,sub,typ,usr->src,usr->dst,usr->siz);
    if (fld == 0) {skipped = 0; toread = 0;}
    switch (num) {default:
    break; case (TYPECenter):
    if (fld == identField(num,"siz")) {int siz = readInt(arg->rgt); toread = readInt(arg->lft); writeInt(siz+usr->siz,arg->idx); return (struct MergeEnum){IdxMrg,(1<<IdxMrg)};}
    if (fld >= identField(num,"ind")) if (toread > 0) {toread -= 1;
    if (sub == 0 && skipped < usr->src) {skipped += 1; return (struct MergeEnum){NonMrg,(1<<LftMrg)};}
    else if (sub >= usr->dst && sub < usr->dst+usr->siz) return (struct MergeEnum){LftMrg,(1<<LftMrg)|(1<<RgtMrg)};
    else if (sub == usr->dst+usr->siz) return (struct MergeEnum){NonMrg,(1<<LftMrg)};}
    else if (sub >= usr->dst && sub < usr->dst+usr->siz) return (struct MergeEnum){ZerMrg,0}; else return (struct MergeEnum){RgtMrg,(1<<LftMrg)|(1<<RgtMrg)};}
    return (struct MergeEnum){RgtMrg,(1<<LftMrg)|(1<<RgtMrg)};
}

void machineTage(int sim, struct Extend *ptr, char **nam)
{
    if (waitSafe(evalSem) != 0) ERROR();
    for (int i = 0; i < sim; i++) {
    int wfd = datxClr(1); int ftp;
    int num, stp; int found = 0;
    int types[] = {TYPEExtend,TYPECenter,TYPEMetric,TYPEReboot};
    for (int j = 0; j < sizeof(types)/sizeof(int) && !found; j++) {
    num = identField(types[j],nam[i]); if (num >= 0) {found = 1;
    stp = types[j]; ftp = identSubtype(stp,num);}} if (!found) ERROR();
    int rfd = datxClr(0); switch (stp) {default: ERROR();
    break; case (TYPEExtend): writeExtend(ptr,rfd);
    break; case (TYPECenter): writeCenter(ptr->ptr,rfd);
    break; case (TYPEMetric): writeMetric(ptr->ptr->met,rfd);
    break; case (TYPEReboot): writeReboot(&ptr->sav,rfd);}
    writeField(stp,num,0,rfd,wfd);
    void *dat0 = 0; datxStr(&dat0,nam[i]);
    void *dat1 = 0; datxGet(1,&dat1);
    datxInsert(dat0,dat1,ftp); free(dat0); free(dat1);}
    if (postSafe(evalSem) != 1) ERROR();
}
void machineSage(int sim, struct Extend *ptr, char **nam)
{
    if (waitSafe(evalSem) != 0) ERROR();
    for (int i = 0; i < sim; i++) {
    void *dat = 0; datxStr(&dat,nam[i]); void *val = 0; datxFind(&val,dat); free(dat);
    if (val == 0) ERROR();
    int num, stp, ftp; int found = 0;
    int types[] = {TYPEExtend,TYPECenter,TYPEMetric,TYPEReboot};
    for (int j = 0; j < sizeof(types)/sizeof(int) && !found; j++) {
    num = identField(types[j],nam[i]); if (num >= 0) {found = 1;
    stp = types[j]; ftp = identSubtype(stp,num);}} if (!found) ERROR();
    int rfd = datxClr(0); switch (stp) {default: ERROR();
    break; case (TYPEExtend): writeExtend(ptr,rfd);
    break; case (TYPECenter): writeCenter(ptr->ptr,rfd);
    break; case (TYPEMetric): writeMetric(ptr->ptr->met,rfd);
    break; case (TYPEReboot): writeReboot(&ptr->sav,rfd);}
    struct PlaneField usr = {num,0};
    struct MergeStruct dtf = {&usr,rfd,datxPut(1,val),datxClr(3)};
    int wfd = datxClr(2); switch (stp) {default: ERROR();
    break; case (TYPEExtend): mergeExtend(wfd,centerField,&dtf);
    break; case (TYPECenter): mergeCenter(wfd,centerField,&dtf);
    break; case (TYPEMetric): mergeMetric(wfd,centerField,&dtf);
    break; case (TYPEReboot): mergeReboot(wfd,centerField,&dtf);}
    switch (stp) {default: ERROR();
    break; case (TYPEExtend): readExtend(ptr,wfd);
    break; case (TYPECenter): readCenter(ptr->ptr,wfd);
    break; case (TYPEMetric): readMetric(ptr->ptr->met,wfd);
    break; case (TYPEReboot): readReboot(&ptr->sav,wfd);}
    free(val);}
    if (postSafe(evalSem) != 1) ERROR();
}
void machineTend(struct Express *exp, struct Extend *ptr)
{
    if (waitSafe(evalSem) != 0) ERROR();
    writeExtend(ptr,datxClr(0));
    void *dat0 = 0; datxStr(&dat0,"_");
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPEExtend);
    free(dat0); free(dat1);
    void *dat = 0; int typ = datxEval(&dat,exp,TYPEExtend);
    if (typ != TYPEExtend) ERROR();
    freeExtend(ptr); readExtend(ptr,datxPut(0,dat)); free(dat);
    if (postSafe(evalSem) != 1) ERROR();
}
void machineEval(struct Express *exp, struct Center *ptr)
{
    if (waitSafe(evalSem) != 0) ERROR();
    writeCenter(ptr,datxClr(0));
    void *dat0 = 0; datxStr(&dat0,"_");
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPECenter);
    free(dat0); free(dat1);
    void *dat = 0; int typ = datxEval(&dat,exp,TYPECenter);
    if (typ != TYPECenter) ERROR();
    freeCenter(ptr); readCenter(ptr,datxPut(0,dat)); free(dat);
    if (postSafe(evalSem) != 1) ERROR();
}
void machineKern(struct Express *exp, struct Kernel *ptr)
{
    if (waitSafe(evalSem) != 0) ERROR();
    writeKernel(ptr,datxClr(0));
    void *dat0 = 0; datxStr(&dat0,"_");
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPEKernel);
    free(dat0); free(dat1);
    void *dat = 0; int typ = datxEval(&dat,exp,TYPEKernel);
    if (typ != TYPEKernel) ERROR();
    freeKernel(ptr); readKernel(ptr,datxPut(0,dat)); free(dat);
    if (postSafe(evalSem) != 1) ERROR();
}
void machineMatr(struct Express *exp, struct Matrix *ptr)
{
    if (waitSafe(evalSem) != 0) ERROR();
    writeMatrix(ptr,datxClr(0));
    void *dat0 = 0; datxStr(&dat0,"_");
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPEMatrix);
    free(dat0); free(dat1);
    void *dat = 0; int typ = datxEval(&dat,exp,TYPEMatrix);
    if (typ != TYPEMatrix) ERROR();
    freeMatrix(ptr); readMatrix(ptr,datxPut(0,dat)); free(dat);
    if (postSafe(evalSem) != 1) ERROR();
}
void machineMetr(struct Express *exp, struct Metric *ptr)
{
    if (waitSafe(evalSem) != 0) ERROR();
    writeMetric(ptr,datxClr(0));
    void *dat0 = 0; datxStr(&dat0,"_");
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPEMetric);
    free(dat0); free(dat1);
    void *dat = 0; int typ = datxEval(&dat,exp,TYPEMetric);
    if (typ != TYPEMetric) ERROR();
    freeMetric(ptr); readMetric(ptr,datxPut(0,dat)); free(dat);
    if (postSafe(evalSem) != 1) ERROR();
}
void machineLine(struct Express *exp, struct Matrix *lft, struct Matrix *rgt)
{
    float mat[16]; planeTransform(mat,
    lft->mat+0,rgt->mat+0,lft->mat+4,rgt->mat+4,
    lft->mat+8,rgt->mat+8,lft->mat+12,rgt->mat+12);
    struct Matrix tmp; for (int i = 0; i < 16; i++) tmp.mat[i] = mat[i];
    machineMatr(exp,&tmp);
}
void machineFunc(struct Express *exp, float *mat)
{
    struct Matrix tmp; for (int i = 0; i < 16; i++) tmp.mat[i] = mat[i];
    machineMatr(exp,&tmp);
}
void machineVoid(struct Express *exp)
{
    if (waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int typ = datxEval(&dat,exp,-1); free(dat);
    if (postSafe(evalSem) != 1) ERROR();
}
void machineIarg(const char *str, int idx)
{
    if (waitSafe(evalSem) != 0) ERROR();
    writeInt(idx,datxClr(0));
    void *dat0 = 0; datxStr(&dat0,str);
    void *dat1 = 0; datxGet(0,&dat1);
    datxInsert(dat0,dat1,TYPEInt);
    free(dat0); free(dat1);
    if (postSafe(evalSem) != 1) ERROR();
}
int machineSwitch(struct Machine *mptr, enum Thread tag, int idx);
void planeMachine(enum Thread tag, int idx);
void planeFork(enum Thread thd, int idx, mftype fnc);
void machineExec(enum Thread tag, int idx, struct Extend *ext)
{
    int debug = ((planeInfo(RegisterVerb,0,planeRcfg)&(1<<ExecVrb)) != 0);
    if (debug) {deleteSmart(ext->log); ext->log = selfSmart("exec");}
    centerSmart(ext,"reboot");
    struct Center *ptr = ext->ptr;
    // {char *st0 = 0; showMemory(ptr->mem,&st0); fprintf(stderr,"machineExec %d %s %d/%d\n",idx,st0,ptr->idx,ptr->siz); free(st0);}
    switch (ptr->mem) {default: ERROR();
    case (Expressz): for (int i = 0; i < ptr->siz; i++) machineVoid(&ptr->exp[i]); break;
    case (Transferz): for (int i = 0; i < ptr->siz; i++) machineSwitch(&ptr->exe[i],tag,idx); break;
    case (Machinez): {struct Extend *cent[1]; struct Reboot boot[1]; cent[0] = ext; struct Reboot init = {0,BootAsr}; boot[0] = init;
    safeMach(idx,boot,cent,1); planeFork(tag,idx,planeMachine);} break;
    case (Rebootz): {struct Extend *cent[ptr->siz]; struct Reboot boot[ptr->siz]; void *repush = 0; repush = allocCenterq();
    for (int i = 0; i < ptr->siz; i++) {
    // clear event before clearing the condition that the event indicates
    planeInfo(RegisterWake,1<<SlctMsk,planeWotc);
    if (waitSafe(pipeSem) != 0) ERROR();
    struct Extend *nxt = maybeCenterq(0,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    if (nxt == 0 && waitSafe(safeSafe(tag,idx)) < 0) break;
    if (nxt == 0) {i--; continue;}
    if (nxt->src != ext->src || nxt->ptr->slf != ptr->slf) {
    nxt->loc.asr = PipeAsr; pushCenterq(nxt,repush); continue;}
    if (debug) {deleteSmart(nxt->log); nxt->log = otherSmart(ext->log);}
    centerSmart(nxt,"exec");
    // if (nxt->ptr->mem == Transferz) {char *st0 = 0; showTransfer(nxt->ptr->exe[0].xfr,&st0); fprintf(stderr,"machineExec %d %s\n",nxt->loc,st0); free(st0);}
    // else {char *st0 = 0; showMemory(nxt->ptr->mem,&st0); fprintf(stderr,"machineExec %d %s\n",nxt->loc,st0); free(st0);}
    boot[i] = ptr->loc[i]; cent[i] = nxt;}
    if (sizeCenterq(repush) > 0) {
    if (waitSafe(pipeSem) != 0) ERROR();
    joinCenterq(repush,internal);
    if (postSafe(pipeSem) != 1) ERROR();}
    if (waitSafe(pipeSem) != 0) ERROR();
    int size = sizeCenterq(internal);
    if (postSafe(pipeSem) != 1) ERROR();
    for (int i = 0; i < size; i++) planeJnfo(RegisterWake,(1<<SlctMsk),planeWots);
    freeCenterq(repush);
    safeMach(idx,boot,cent,ptr->siz);
    planeFork(tag,idx,planeMachine);}
    break;}
}
// identity I
// current manipulation C
// Kernel.saved T
// Kernel.local L
// Kernel.sent S
// Kernel.global G
// Matrix M
// T goes to C thus changing GSLT, when C is not I upon MoveMsk or RollMsk
// T goes to C without changing GSLT, upon menu->dev change
// T goes to I without changing GSLT, when C is I upon menu->dev change
// L goes to I without changing GSLT, upon Push
// S goes to I without changing GSLT, upon last outstanding slf<0
// G changes by M thus changing GSLT, upon slf>=0
void machineDemo(struct Menu *menu, enum Thread tag, int idx)
{
    switch (menu->msk) {default: ERROR();
    break; case (SlctMsk): // M from external in src
    demoRead(menu,tag,idx); // G = GM, maybe S = M'S
    demoSend(menu,tag,idx); // M = GSLT, Call dst
    demoDisp(menu,tag,idx); // Draw dsp
    break; case (DoneMsk): // pierce from replace in src
    demoMask(menu,tag,idx); // Jnfo geo
    demoDone(menu); // Push dst
    break; case (PrssMsk): // Press* in queue
    menu->act = Indicate; menu->dev = Devices;
    demoPush(menu,tag,idx); // M = L, S = SL, L = I, Push dst
    demoMenu(menu); // Jnfo cfg
    break; case (ProjMsk): // change in Focal* UniformWid/Hei
    demoSize(menu); // Proj, Call dst
    demoDisp(menu,tag,idx); // Draw dsp
    break; case (EoodMsk):
    // TODO wait for window resize
    break; case (MoveMsk): // change in ManipLeft/Base
    if (menu->act == Manipulate) {
    if (menu->dev != Coord) {menu->dev = Coord;
    demoCont(menu,tag,idx); // L = LTC', T = C
    planeJnfo(ManipFixed,menu->coo,planeWcfg);}
    demoSend(menu,tag,idx); // M = GSLT, Call dst
    demoDisp(menu,tag,idx); /* Draw dsp*/}
    break; case (ClckMsk): // Click* in queue
    if (menu->act == Manipulate) {
    menu->act = Indicate;
    demoPush(menu,tag,idx); /* M = L, S = SL, L = I, Push dst*/} else {
    demoPute(menu,tag,idx); /* Draw pie/nor/sel*/}
    break; case (RollMsk): // change in ManipAngle
    if (menu->act == Manipulate) {
    if (menu->dev != Angle) {menu->dev = Angle;
    demoCont(menu,tag,idx); // L = LTC', T = C
    planeJnfo(ManipFixed,menu->ang,planeWcfg);}
    demoSend(menu,tag,idx); // M = GSLT, Call dst
    demoDisp(menu,tag,idx); /* Draw dsp*/}
    break; case (TimeMsk): // timer expired
    if (menu->act == Manipulate) {
    menu->act = Indicate;
    demoNone(menu,tag,idx); // L = LT, T = I
    demoPush(menu,tag,idx); /* M = L, S = SL, L = I, Push dst*/}}
}
int machineEscape(struct Machine *mch, int siz, int level, int next)
{
    int inc = (level > 0 ? 1 : (level == 0 ? 0 : -1)); level *= inc;
    while (1) {
    next += inc;
    if (next < 0 || next >= siz) break;
    struct Machine *mptr = &mch[next];
    if (mptr->xfr == Nest) level += mptr->nia*inc;
    if (level <= 0) break;}
    return next;
}
int machineIval(struct Express *exp)
{
    if (waitSafe(evalSem) != 0) ERROR();
    void *dat = 0; int typ = datxEval(&dat,exp,TYPEInt);
    if (typ != TYPEInt) ERROR();
    int val = readInt(datxPut(0,dat));
    free(dat);
    if (postSafe(evalSem) != 1) ERROR();
    return val;
}
#define PULL(PTR,SRC,LOG) {struct Return ret = centerPull(SRC,mptr->xfr,tag,idx,LOG);\
    if (ret.vld < 0) return -1;\
    if (ret.ptr == 0) {if (centerPro(tag,idx,NoopPro)) return 0; else ERROR();}\
    PTR = ret.ptr;}
#define REPULL(PTR,SRC,LOG) {struct Return ret = centerRepull(SRC,mptr->xfr,tag,idx,LOG);\
    if (ret.vld < 0) return -1;\
    if (ret.ptr == 0) {if (centerPro(tag,idx,NoopPro)) return 0; else ERROR();}\
    PTR = ret.ptr;}
#define RECHCK(SIZ,SRC,LOG) {struct Return ret = centerRepull(SRC,mptr->xfr,tag,idx,LOG);\
    if (ret.vld < 0) return -1;\
    if (ret.ptr == 0) {if (centerPro(tag,idx,NoopPro)) return 0; else ERROR();}\
    SIZ = ret.siz;}
#define PULL2(PTR0,PTR1,SRC0,SRC1,LOG) {struct Return ret0, ret1;\
    ret0 = centerPull(SRC0,mptr->xfr,tag,idx,LOG);\
    if (ret0.vld < 0) return -1;\
    if (ret0.ptr == 0) {if (centerPro(tag,idx,NoopPro)) return 0; else ERROR();}\
    if (ret0.ptr->loc.sub != SRC0 || (ret0.ptr->loc.asr != PlaceAsr && ret0.ptr->loc.asr != LocalAsr)) ERROR();\
    ret1 = centerPull(SRC1,mptr->xfr,tag,idx,LOG);\
    if (ret0.ptr && !ret1.ptr) centerPlace(ret0.ptr,tag,idx);\
    if (ret1.vld < 0) return -1;\
    if (ret1.ptr == 0) {if (centerPro(tag,idx,NoopPro)) return 0; else ERROR();}\
    if (ret1.ptr->loc.sub != SRC1 || (ret1.ptr->loc.asr != PlaceAsr && ret1.ptr->loc.asr != LocalAsr)) ERROR();\
    PTR0 = ret0.ptr; PTR1 = ret1.ptr;}
int registerVirtual(int thrd);
int registerEvent(int virt);
int machineSwitch(struct Machine *mptr, enum Thread tag, int idx)
{
    if (!mptr) ERROR();
    switch (mptr->xfr) {default: ERROR();
    break; case (This): {const char *str;
        str = mptr->isa[0];
        machineIarg(str,idx);}
    break; case (Able): {const char *str;
        str = mptr->asa[0];
        int val = registerEvent(registerVirtual(MachThd|(idx<<4)));
        machineIarg(str,val);}
    break; case (Tage): {struct Extend *ptr;
        int src = machineIval(mptr->sop[0].sup);
        PULL(ptr,src,"tage");
        machineTage(mptr->ssz,ptr,mptr->ssa);
        centerPlace(ptr,tag,idx);}
    break; case (Sage): {struct Extend *ptr;
        int src = machineIval(mptr->gop[0].sup);
        PULL(ptr,src,"sage");
        machineSage(mptr->gsz,ptr,mptr->gsa);
        centerReplace(ptr,tag,idx);}
    break; case (Chck): {int siz; struct Reboot src;
        src.sub = machineIval(mptr->cop[0].sup); src.asr = mptr->cop[0].asr;
        RECHCK(siz,src,"chck");
        machineIarg(mptr->csa[0],siz);}
    break; case (Move): {struct Extend *ptr; struct Reboot src;
        src.sub = machineIval(mptr->pop[0].sup); src.asr = mptr->pop[0].asr;
        REPULL(ptr,src,"move");
        ptr->loc.sub = machineIval(mptr->pop[1].sup); ptr->loc.asr = mptr->pop[1].asr;
        centerReplace(ptr,tag,idx);}
    break; case (Copy): {struct Extend *ptr; struct Reboot src;
        src.sub = machineIval(mptr->qop[0].sup); src.asr = mptr->qop[0].asr;
        REPULL(ptr,src,"copy");
        ptr->loc.sub = machineIval(mptr->qop[1].sup); ptr->loc.asr = mptr->qop[1].asr;
        centerReplace(ptr,tag,idx);}
    break; case (Tend): {struct Extend *ptr; struct Express *fnc; struct Reboot src;
        src.sub = machineIval(mptr->mop[0].sup); src.asr = mptr->mop[0].asr;
        REPULL(ptr,src,"tend");
        machineTend(mptr->mpo[0].fnc,ptr);
        centerReplace(ptr,tag,idx);}
    break; case (Eval): {struct Extend *ptr;
        int src = machineIval(mptr->eop[0].sup);
        PULL(ptr,src,"eval");
        machineEval(mptr->epo[0].fnc,ptr->ptr);
        centerPlace(ptr,tag,idx);}
    break; case (Kern): {struct Extend *ptr;
        int src = machineIval(mptr->kop[0].sup);
        PULL(ptr,src,"kern");
        int sub = machineIval(mptr->kop[0].sub);
        if (ptr->ptr->mem != Kernelz || ptr->ptr->siz <= sub) ERROR();
        machineKern(mptr->kpo[0].fnc,&ptr->ptr->ker[sub]);
        centerPlace(ptr,tag,idx);}
    break; case (Matr): {struct Extend *ptr;
        int src = machineIval(mptr->kop[0].sup);
        PULL(ptr,src,"matr");
        int sub = machineIval(mptr->kop[0].sub);
        if (ptr->ptr->mem != Matrixz || ptr->ptr->siz <= sub) ERROR();
        machineMatr(mptr->kpo[0].fnc,&ptr->ptr->mat[sub]);
        centerPlace(ptr,tag,idx);}
    break; case (Metr): {struct Extend *ptr;
        int src = machineIval(mptr->kop[0].sup);
        PULL(ptr,src,"metr");
        int sub = machineIval(mptr->kop[0].sub);
        if (ptr->ptr->mem != Metricz || ptr->ptr->siz <= sub) ERROR();
        machineMetr(mptr->kpo[0].fnc,&ptr->ptr->met[sub]);
        centerPlace(ptr,tag,idx);}
    break; case (Line): {struct Extend *lft; struct Extend *rgt;
        int lrc = machineIval(mptr->lop[0].sup);
        int rrc = machineIval(mptr->lop[1].sup);
        PULL2(lft,rgt,lrc,rrc,"line");
        int lub = machineIval(mptr->lop[0].sub);
        int rub = machineIval(mptr->lop[1].sub);
        machineLine(mptr->lpo[0].fnc,&lft->ptr->mat[lub],&rgt->ptr->mat[rub]);
        centerPlace(lft,tag,idx); centerPlace(rgt,tag,idx);}
    break; case (Proj): {float mat[16];
        machineFunc(mptr->fpo[0].fnc,planeWindow(mat));}
    break; case (Form): {float mat[16];
        machineFunc(mptr->fpo[0].fnc,planeMatrix(mat));}
    // TODO resurrect operations upon Kernel
    break; case (Void): {
        machineVoid(mptr->fpo[0].fnc);}
    break; case (Exec): {struct Extend *ptr;
        int src = machineIval(mptr->top[0].sup);
        PULL(ptr,src,"exec");
        machineExec(tag,machineIval(mptr->tie[0].val),ptr);}
    break; case (Demo): {struct Extend *ptr;
        int src = machineIval(mptr->nop[0].sup);
        PULL(ptr,src,"demo");
        int sub = machineIval(mptr->nop[0].sub);
        machineDemo(&ptr->ptr->men[sub],tag,idx);
        centerPlace(ptr,tag,idx);}
    }
    return 0;
}

// thread callbacks
void planeMachine(enum Thread tag, int idx)
{
    struct Reboot *boot = reboot[idx]; reboot[idx] = 0;
    struct Extend **cent = recent[idx]; recent[idx] = 0;
    int size = resize[idx];
    if (!size) ERROR();
    for (int i = 0; i < size; i++) {
    // {char *st0 = 0; char *st1 = 0; showReboot(&boot[i],&st0); showMemory(cent[i]->ptr->mem,&st1); fprintf(stderr,"planeMachine%d %s %s\n",idx,st0,st1); free(st0); free(st1);}
    if (boot[i].asr == PlaceAsr) {cent[i]->loc = boot[i]; centerPlace(cent[i],tag,idx);}
    // TODO push to queue if boot[i].asr is not BootAsr
    else if (boot[i].asr == BootAsr) {
    struct Center *cptr = cent[i]->ptr;
    int next = 0; struct Machine *mach = 0;
    switch (cptr->mem) {default: next = -1;
    break; case (Machinez): mach = cptr->mch;
    break; case (Transferz): mach = cptr->exe;}
    waitSafe(machSem);
    while (next >= 0 && next < cptr->siz) {
    struct Machine *mptr = &mach[next];
    // {char *st0 = 0; showTransfer(mptr->xfr,&st0); fprintf(stderr,"planeMachine%d %d %s\n",idx,next,st0); free(st0);}
    int save = next;
    switch (mptr->xfr) {default: if (machineSwitch(mptr,tag,idx) < 0) next = -1; else next += 1; break;
    case (Goto): next += machineIval(mptr->jie[0].val); break;
    case (Jump): next = machineEscape(mach,cptr->siz,machineIval(mptr->jie[0].val),next); break;
    case (Nest): next += 1; break;}
    if (next == save) {
    postSafe(machSem);
    if (waitSafe(safeSafe(tag,idx)) < 0) next = -1;
    else next += 1;
    waitSafe(machSem);}}
    postSafe(machSem);}
    else ERROR();}
    for (int i = 0; i < size; i++) if (boot[i].asr == BootAsr) {
    deleteSmart(cent[i]->log); freeExtend(cent[i]); allocExtend(&cent[i],0);}
    free(boot); free(cent);
    waitSafe(safeSem);
    resize[idx] = 0;
    postSafe(safeSem);
}
void planeCenter(enum Thread tag, int idx)
{
    while (1) {
    if (waitSafe(safeSafe(tag,idx)) < 0) break;
    while (1) {
    int loop = centerPro(tag,idx,LoopPro); // whether to flush upon wake
    // the default pops only one before waiting for another event
    if (waitSafe(pipeSem) != 0) ERROR();
    struct Extend *center = maybeCenterq(0,response);
    if (postSafe(pipeSem) != 1) ERROR();
    if (center == 0 && loop) break;
    if (center == 0) ERROR();
    if (center->loc.asr != RespAsr) ERROR();
    if (center->src < 0 || center->src >= Programs) ERROR();
    if (waitSafe(pipeSem) != 0) ERROR();
    int sub = inverse[center->src];
    if (postSafe(pipeSem) != 1) ERROR();
    writeCenter(center->ptr,sub);
    center->ret = DoneRet;
    centerDone(center);
    if (!loop) break;}}
}
void planeExternal(enum Thread tag, int idx)
{
    while (1) {
    if (waitSafe(callSem) != 0) ERROR();
    int temp = external;
    if (postSafe(callSem) != 1) ERROR();
    int sub = waitRead(0.0,(temp|(1<<extdone)));
    if (sub == extdone) {if (readChr(extdone)) break; else continue;}
    if ((1<<sub)&temp != (1<<sub)) ERROR();
    struct Extend *center = 0; allocExtend(&center,1);
    readCenter(center->ptr,sub);
    if (waitSafe(callSem) != 0) ERROR();
    center->src = (int*)*userIdent(sub) - inverse;
    if (postSafe(callSem) != 1) ERROR();
    int debug = ((planeInfo(RegisterVerb,0,planeRcfg)&(1<<PipeVrb)) != 0);
    center->log = selfSmart(debug?"pipe":0);
    if (debug) centerSmart(center,"pipe");
    center->loc.asr = PipeAsr;
    // {char *st0 = 0; showExtend(center,&st0); fprintf(stderr,"planeExternal %s\n",st0); free(st0);}
    if (waitSafe(pipeSem) != 0) ERROR();
    pushCenterq(center,internal);
    if (postSafe(pipeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<SlctMsk),planeWots);}
}
void planeString(enum Thread tag, int idx)
{
    while (1) {
    if (waitSafe(safeSafe(tag,idx)) < 0) break;
    while (1) {
    int prot = centerPro(tag,idx,LoopPro);
    // the default pops only one before waiting for another event
    if (waitSafe(stdioSem) != 0) ERROR();
    char *str = maybeStrq(0,strout);
    if (postSafe(stdioSem) != 1) ERROR();
    if (str == 0 && prot) break;
    if (str == 0) ERROR();
    writeStr(str,console);
    free(str);
    if (!prot) break;}}
}
void planeConsole(enum Thread tag, int idx)
{
    while (1) {
    int sub = waitRead(0.0,(1<<console)|(1<<condone));
    if (sub == condone) {if (readChr(condone)) break; else continue;}
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
    if (timeSafe(safeSafe(tag,idx),0.0) < 0) break; else continue;}
    if (sizeTimeq(timeq) != sizeIntq(wakeq)) ERROR();
    float time = frontTimeq(timeq); int wake = frontIntq(wakeq);
    if (postSafe(timeSem) != 1) ERROR();
    float delta = time-(float)processTime(); // how long to wait
    if (timeSafe(safeSafe(tag,idx),delta) < 0) break;
    if ((float)processTime() >= time) {
    if (waitSafe(timeSem) != 0) ERROR();
    dropTimeq(timeq); dropIntq(wakeq);
    if (postSafe(timeSem) != 1) ERROR();
    planeJnfo(RegisterWake,(1<<TimeMsk),planeWots);}}
}
int planeSugval(const char *str);
void planeTest(enum Thread tag, int idx)
{
    switch (idx) {default: ERROR();

    break; case (0): {
    int debug = 0; int count = 0; float time = 0.0; int tested = 0; int alt = 0;
    int mode = (planeInfo(RegisterPlan,0,planeRcfg)==Bringup);

    while (1) {struct Extend *blk;
    blk = centerPull(planeSugval("@scratch2"),This,TestThd,0,"Test").ptr;
    if (blk) {centerPlace(blk,TestThd,0); break;}
    callWait();}

    while (timeSafe(safeSafe(tag,idx),0.001) >= 0) {
    if (time == 0.0) time = processTime();
    if (processTime()-time > 0.1) {time = processTime(); count += 1;}

    struct Extend *mat = centerPull(Matrixz,This,TestThd,0,"Test0").ptr; if (!mat) {callWait(); continue;}
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
    planeMouseRotateCursor(mat->ptr->mat[1].mat,fix,0,org,cur);}
    else planeMatrix(mat->ptr->mat[0].mat);
    mat->loc.sub = 0; mat->loc.asr = CallAsr;
    mat->sav.sub = Matrixz; mat->sav.asr = PlaceAsr;
    mat->rsp = RptRsp; mat->ret = NoneRet;
    if (debug) callCopy(mat,1,"matrix"); else callCont(mat,1,mat->log);
    if (alt) alt = 0; else alt = 1;

    if (count == tested) {
    int width,height; {enum Configure cfg[2] = {UniformWid,UniformHei}; int val[2] = {0,0};
    callInfo(cfg,val,2,planeRcfg); width = val[0]; height = val[1];}
    int giv[] = {width,height,0,12}; // idx,siz
    struct Extend *drw = centerPull(Drawz,This,TestThd,0,"Test1").ptr; if (!drw) {callWait(); continue;}
    freeCenter(drw->ptr);
    drw->ptr->mem = Drawz; drw->ptr->idx = 0; drw->ptr->siz = 1;
    allocDraw(&drw->ptr->drw,drw->ptr->siz);
    drw->ptr->drw[0].con.tag = MicroCon;
    drw->ptr->drw[0].con.mic = (mode?MicroFetDrw:MicroVtxDrw);
    drw->ptr->drw[0].siz = sizeof(giv)/sizeof(int);
    allocInt(&drw->ptr->drw[0].arg,drw->ptr->drw[0].siz);
    for (int i = 0; i < drw->ptr->drw[0].siz; i++) drw->ptr->drw[0].arg[i] = giv[i];
    drw->loc.sub = 0; drw->loc.asr = CallAsr;
    drw->sav.sub = Drawz; drw->sav.asr = PlaceAsr;
    drw->rsp = RetRsp; drw->ret = NoneRet;
    if (debug) callCopy(drw,1,"draw"); else callCont(drw,1,drw->log);}
    tested = count;}}

    break; case (1): {
    int debug = 0; int count = 0; float time = 0.0; int tested = 0;
    int width,height; {enum Configure cfg[2] = {UniformWid,UniformHei}; int val[2] = {0,0};
    callInfo(cfg,val,2,planeRcfg); width = val[0]; height = val[1];}
    int hiv[] = {width,height,0,12}; // width,height,idx,siz
    int fiv[] = {width,height}; // width,height

    while (1) {struct Extend *blk;
    blk = centerPull(planeSugval("@scratch2"),This,TestThd,0,"Test").ptr;
    if (blk) {centerPlace(blk,TestThd,0); break;}
    callWait();}

    while (timeSafe(safeSafe(tag,idx),0.001) >= 0) {
    if (time == 0.0) time = processTime();
    if (processTime()-time > 0.1) {time = processTime(); count += 1;}

    if (count == tested) {}

    else if (count%6 == 1 || count%6 == 4) {
    struct Extend *eek = centerPull(Getoldz,This,TestThd,0,"Test2").ptr; if (!eek) {callWait(); continue;}
    freeCenter(eek->ptr);
    eek->ptr->mem = Getoldz; eek->ptr->idx = (int)(0.3*width)+(int)(0.3*height)*width; eek->ptr->siz = 1;
    allocOld(&eek->ptr->old,eek->ptr->siz);
    eek->loc.sub = 0; eek->loc.asr = CallAsr;
    eek->sav.sub = Getoldz; eek->sav.asr = PlaceAsr;
    eek->rsp = RptRsp; eek->ret = NoneRet;
    if (debug) callCopy(eek,0,"getold"); else callCont(eek,0,eek->log);}

    else if (count%6 == 2 || count%6 == 5) {
    struct Extend *eek = centerPull(Getintz,This,TestThd,0,"Test3").ptr; if (!eek) {callWait(); continue;}
    freeCenter(eek->ptr);
    eek->ptr->mem = Getintz; eek->ptr->idx = (int)(0.3*width)+(int)(0.3*height)*width; eek->ptr->siz = 1;
    allocInt(&eek->ptr->uns,eek->ptr->siz);
    eek->loc.sub = 0; eek->loc.asr = CallAsr;
    eek->sav.sub = Getintz; eek->sav.asr = PlaceAsr;
    eek->rsp = RptRsp; eek->ret = NoneRet;
    if (debug) callCopy(eek,0,"getint"); else callCont(eek,0,eek->log);}

    else if (count%6 == 3 || count%6 == 0) {
    struct Extend *vec = centerPull(Vectorz,This,TestThd,0,"Test4").ptr; if (!vec) {callWait(); continue;}
    freeCenter(vec->ptr);
    vec->ptr->mem = Vectorz; vec->ptr->idx = (int)(0.3*width)+(int)(0.3*height)*width; vec->ptr->siz = 1;
    allocVector(&vec->ptr->vec,vec->ptr->siz);
    vec->ptr->vec[0].vec[0] = 1.0; vec->ptr->vec[0].vec[1] = 2.0;
    vec->ptr->vec[0].vec[2] = 3.0; vec->ptr->vec[0].vec[3] = 4.0;
    vec->loc.sub = 0; vec->loc.asr = CallAsr;
    vec->sav.sub = Vectorz; vec->sav.asr = PlaceAsr;
    vec->rsp = RptRsp; vec->ret = NoneRet;
    if (debug) callCopy(vec,0,"vector"); else callCont(vec,0,vec->log);}

    tested = count;}}

    break; case (2):{
    while (timeSafe(safeSafe(tag,idx),0.1) >= 0) {
    planeJnfo(RegisterWake,(1<<DoneMsk),planeWots);
    }}}
}

// phase callbacks
void planeMain(enum Configure cfg, enum Thread tag, int idx)
{
    // TODO what would logging on the main Machinez mean
    struct Extend *ext = centerPull(planeGnfo(cfg,0,planeRcfg),This,tag,idx,0).ptr;
    struct Extend *cent[1]; struct Reboot boot[1]; cent[0] = ext;
    struct Reboot init = {0,BootAsr}; boot[0] = init;
    safeMach(idx,boot,cent,1);
}
void planeCons()
{
    condone = openPipe();
    console = rdwrInit(STDIN_FILENO,STDOUT_FILENO);
    if (console < 0) ERROR();
}
void planeClose(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): switch (idx) {default: ERROR();
    break; case (0): doneSafe(safeSafe(tag,idx));
    break; case (1): writeChr(1,extdone);}
    break; case (StdioThd): switch (idx) {default: ERROR();
    break; case (0): doneSafe(safeSafe(tag,idx));
    break; case (1): writeChr(1,condone);}
    break; case (MachThd): switch (idx) {default: ERROR();
    break; case (0): doneSafe(safeSafe(tag,idx));
    break; case (1): doneSafe(safeSafe(tag,idx));}
    break; case (TimeThd): switch (idx) {default: ERROR();
    break; case (0): doneSafe(safeSafe(tag,idx));}
    break; case (TestThd): switch (idx) {default: ERROR();
    break; case (0): doneSafe(safeSafe(tag,idx));
    break; case (1): doneSafe(safeSafe(tag,idx));
    break; case (2): doneSafe(safeSafe(tag,idx));}}
}
void planeJoin(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): if (idx) {closeIdent(extdone); for (int i = ffs(external)-1; external; external &= ~(1<<i), i = ffs(external)-1) freeIdent(i);}
    break; case (StdioThd): if (idx) {freeIdent(console); closeIdent(condone);}
    break; case (MachThd): case (TimeThd): case (TestThd):}
}
void planeWake(enum Thread tag, int idx)
{
    // FenceThd handled by vulkanBack
    switch (tag) {default: ERROR();
    break; case (PipeThd): case (StdioThd): case (MachThd): if (idx < 0 || idx >= 2) ERROR();
    break; case (TimeThd): if (idx < 0 || idx >= 1) ERROR();
    break; case (TestThd): if (idx < 0 || idx >= 3) ERROR();}
    if (idx == 1) switch (tag) {default: ERROR();
    break; case (PipeThd): writeChr(0,extdone); return;
    break; case (StdioThd): writeChr(0,condone); return;
    break; case (MachThd): case (TimeThd): case (TestThd):}
    int prot = centerPro(tag,idx,LoopPro);
    // the default, post increments above 1
    (prot?qostSafe:postSafe)(safeSafe(tag,idx));
}
void planeProt(enum Thread tag, int idx)
{
    if (centerMask(tag,idx) == 0) switch (tag) {default: ERROR();
    break; case (PipeThd): switch (idx) {default: ERROR();
    // don't rely on event repetition
    break; case (0): centerProt(tag,idx,((1<<LoopPro)));
    break; case (1):}
    break; case (StdioThd): switch (idx) {default: ERROR();
    // don't rely on event repetition
    break; case (0): centerProt(tag,idx,((1<<LoopPro)));
    break; case (1):}
    break; case (MachThd): switch (idx) {default: ERROR();
    // wake for each event; only block for events
    break; case (0): centerProt(tag,idx,((1<<PrnwPro)));
    // don't rely on event repetition; block on Move/Copy from queues
    break; case (1): centerProt(tag,idx,((1<<LoopPro)|(1<<QrnwPro)));
    break; case (2): centerProt(tag,idx,((1<<LoopPro)|(1<<QrnwPro)));}
    break; case (TimeThd): switch (idx) {default: ERROR();
    break; case (0):}
    break; case (TestThd): switch (idx) {default: ERROR();
    break; case (0): centerProt(tag,idx,((1<<ZeroPro)));
    break; case (1): centerProt(tag,idx,((1<<ZeroPro)));
    break; case (2): centerProt(tag,idx,((1<<ZeroPro)));}
    break; case (MainThd): switch (idx) {default: ERROR();
    break; case (0): centerProt(tag,idx,((1<<ZeroPro)));}}
}
void planeFork(enum Thread tag, int idx, mftype fnc)
{
    callFork(tag,idx,fnc,planeClose,planeJoin,planeWake);
}
void planeOpen(enum Thread tag, int idx)
{
    switch (tag) {default: ERROR();
    break; case (PipeThd): switch (idx) {default: ERROR();
    break; case (0): safeInit(PipeThd,1); planeFork(tag,idx,planeCenter);
    break; case (1): extdone = openPipe(); planeFork(tag,idx,planeExternal);}
    break; case (StdioThd): switch (idx) {default: ERROR();
    break; case (0): safeInit(StdioThd,1); planeFork(tag,idx,planeString);
    break; case (1): planeCons(); planeFork(tag,idx,planeConsole);}
    break; case (MachThd): switch (idx) {default: ERROR();
    break; case (0): planeMain(RegisterMain,tag,idx); planeFork(tag,idx,planeMachine);
    break; case (1): planeMain(RegisterSide,tag,idx); planeFork(tag,idx,planeMachine);}
    break; case (TimeThd): switch (idx) {default: ERROR();
    break; case (0): safeInit(TimeThd,1); planeFork(tag,idx,planeTime);}
    break; case (TestThd): switch (idx) {default: ERROR();
    break; case (0): safeInit(TestThd,1); planeFork(tag,idx,planeTest);
    break; case (1): safeInit(TestThd,2); planeFork(tag,idx,planeTest);
    break; case (2): safeInit(TestThd,3); planeFork(tag,idx,planeTest);}}
}
int planeThread(enum Thread tag)
{
    switch (tag) {default:
    break; case (TimeThd): return 1;
    break; case (PipeThd): case (StdioThd): case (MachThd): return 2;
    break; case (TestThd): return 3;}
    return 0;
}

// register callbacks
void registerOpen(int sav, int val, int act)
{
    int opn = act&~sav; int cls = ~act&sav; int wak = act&sav&val;
    for (int i = ffs(opn)-1; opn; i = ffs(opn&=~(1<<i))-1) for (int j = 0; j < planeThread(i); j++) planeOpen(i,j);
    for (int i = ffs(cls)-1; cls; i = ffs(cls&=~(1<<i))-1) for (int j = 0; j < planeThread(i); j++) planeClose(i,j);
    for (int i = ffs(wak)-1; wak; i = ffs(wak&=~(1<<i))-1) for (int j = 0; j < planeThread(i); j++) planeWake(i,j);
}
void registerWake(int val, int act)
{
    int mask = val&act; // mask of events
    // increment semafor for each write, so no need to clear RegisterWake
    for (int i = ffs(mask)-1; mask; i = ffs(mask&=~(1<<i))-1) {
    if (i >= Masks) ERROR();
    // i is an event
    int able = (sizeIntq(ableq) > i ? *ptrIntq(i,ableq) : 0);
    // able is mask of threads for the event
    int levl = planeGnfo(RegisterMask,0,planeRcfg);
    // levl is whether events are level sensitive
    for (int j = ffs(able)-1; able; j = ffs(able&=~(1<<j))-1) {
    // j is an abstract thread
    int back = *ptrIntq(j,backq);
    // back is the physical thread
    enum Thread thrd = back & 0xf; // thread
    int indx = (back >> 4) & 0xf; // subthread
    if (testSafe(safeSafe(thrd,indx))) { // thread is running
    // increment or set event i per thread j that depends on it
    if ((levl&(1<<i)) != 0) level[i] += 1;
    planeWake(thrd,indx);}}}
}
void registerWane(int val, int act)
{
    int mask = val&act; // mask of events
    // decrement count for each mask bit
    for (int i = ffs(mask)-1; mask; i = ffs(mask&=~(1<<i))-1) {
    // i is an event
    if (level[i] > 0) level[i] -= 1;
    if (level[i] == 0) planeKnfo(RegisterWake,(1<<i),planeWotc);}
}
int registerVirtual(int thrd)
{
    // thrd is Thread plus index, so virtq is sparse and huge
    while (sizeIntq(virtq) <= thrd) pushIntq(-1,virtq);
    if (*ptrIntq(thrd,virtq) < 0) {
    // backq is packed and smaller, indexed by virtual thread
    if (sizeIntq(backq) >= sizeof(int)*8) ERROR();
    *ptrIntq(thrd,virtq) = sizeIntq(backq);
    pushIntq(thrd,backq);}
    return *ptrIntq(thrd,virtq);
}
int registerEvent(int virt)
{
    while (sizeIntq(evntq) <= virt) pushIntq(0,evntq);
    return *ptrIntq(virt,evntq);
}
void registerAble(int val)
{
    int thrd = val & 0xff; // thread and subthread
    int virt = registerVirtual(thrd);
    // remove virt from every event, so RegisterAble is not cummulative
    int temp = (virt < sizeIntq(evntq) ? *ptrIntq(virt,evntq) : 0);
    for (int i = ffs(temp)-1; temp; i = ffs(temp&=~(1<<i))-1) { // i is an event
    *ptrIntq(i,ableq) &= ~(1<<virt);
    *ptrIntq(virt,evntq) &= ~(1<<i);}
    // add virt to mask for each event
    int mask = val >> 8; // mask of events
    for (int i = ffs(mask)-1; mask; i = ffs(mask&=~(1<<i))-1) { // i is an event
    while (sizeIntq(ableq) <= i) pushIntq(0,ableq);
    *ptrIntq(i,ableq) |= 1<<virt;
    while (sizeIntq(evntq) <= virt) pushIntq(0,evntq);
    *ptrIntq(virt,evntq) |= 1<<i;}
}
void registerProt(int act)
{
    enum Thread thd = act & 0xf;
    int idx = (act >> 4) & 0xf;
    centerProt(thd,idx,act>>8);
}
void registerTime(int val)
{
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
void registerArgument(int act[3])
{
    enum Configure arg[3] = {ArgumentInp,ArgumentOut,ArgumentSrc}; int num[3] = {0,0,0};
    callGnfo(arg,num,3,planeRcfg);
    int rdfd = num[0];
    int wrfd = num[1];
    int asrc = num[2];
    if (waitSafe(callSem) != 0) ERROR();
    int sub = rdwrInit(rdfd,wrfd);
    external |= 1<<sub;
    inverse[asrc] = sub;
    *userIdent(sub) = inverse + asrc;
    if (postSafe(callSem) != 1) ERROR();
    writeChr(0,extdone);
}
void registerQue(enum Configure cfg, int val, void *que, int msk)
{
    if (waitSafe(pressSem) != 0) ERROR();
    pushIntq(val,que);
    planeGnfo(cfg,frontIntq(que),planeWcfg);
    if (postSafe(pressSem) != 1) ERROR();
    planeKnfo(RegisterWake,(1<<msk),planeWots);
}
void registerQues(enum Configure cfg, int act, void *que)
{
    if (act < 0) ERROR();
    if (waitSafe(pressSem) != 0) ERROR();
    while (act < sizeIntq(que)) popIntq(que);
    while (act > sizeIntq(que)) pushIntq(0,que);
    if (act > 0) planeGnfo(cfg,frontIntq(que),planeWcfg);
    if (postSafe(pressSem) != 1) ERROR();
}
void registerLog(int sav, int act)
{
    if (sav != act) {
        deleteSmart(sav);
        int tmp = otherSmart(act);
        planeGnfo(RegisterLog,tmp,planeWcfg);
    }
}
void registerCall(const enum Configure *cfg, const int *sav, const int *val, const int *act, int siz)
{
    // multi-callback called once per multi-write, so registerAgument for example not called redundantly
    int argn = 0; int argv[3] = {-1,-1,-1};
    int ufmn = 0; int prjn = 0; int mnpn = 0; int angn = 0;
    for (int i = 0; i < siz; i++) switch (cfg[i]) {default: ERROR();
    break; case (RegisterOpen): registerOpen(sav[i],val[i],act[i]);
    break; case (RegisterWake): registerWake(val[i],act[i]);
    break; case (RegisterWane): registerWane(val[i],act[i]);
    break; case (RegisterAble): registerAble(val[i]);
    break; case (RegisterProt): registerProt(act[i]);
    break; case (RegisterTime): registerTime(val[i]);
    break; case (RegisterExit): callWake();
    break; case (RegisterVerb): clearSmart();
    break; case (UniformAll): case (UniformOne): case (UniformIdx): case (UniformUse): ufmn = 1;
    break; case (UniformTri): case (UniformNum): case (UniformVtx): ufmn = 1;
    break; case (UniformMat): case (UniformBas): ufmn = 1;
    break; case (UniformMod): case (UniformWid): case (UniformHei): ufmn = 1; prjn = 1;
    break; case (ArgumentInp): if (argv[0] == -1) argn += 1; argv[0] = act[i];
    break; case (ArgumentOut): if (argv[1] == -1) argn += 1; argv[1] = act[i];
    break; case (ArgumentSrc): if (argv[2] == -1) argn += 1; argv[2] = act[i];
    break; case (PressKey): registerQue(PressKey,val[i],charq,PrssMsk);
    break; case (PressQueue): registerQues(PressKey,act[i],charq);
    break; case (ClickLeft): registerQue(ClickLeft,val[i],leftq,ClckMsk);
    break; case (ClickBase): registerQue(ClickBase,val[i],baseq,ClckMsk);
    break; case (ClickAngle): registerQue(ClickAngle,val[i],angleq,ClckMsk);
    break; case (ClickQueue):
    registerQues(ClickLeft,act[i],leftq);
    registerQues(ClickBase,act[i],baseq);
    registerQues(ClickAngle,act[i],angleq);
    break; case (ManipLeft): case (ManipBase): mnpn = 1;
    break; case (ManipAngle): angn = 1;
    break; case (RegisterLog): registerLog(sav[i],act[i]);}
    if (argn == 3) registerArgument(argv);
    if (ufmn) planeKnfo(RegisterWake,(1<<UnifMsk),planeWots);
    if (prjn) planeKnfo(RegisterWake,(1<<ProjMsk),planeWots);
    if (mnpn) planeKnfo(RegisterWake,(1<<MoveMsk),planeWots);
    if (angn) planeKnfo(RegisterWake,(1<<RollMsk),planeWots);
}

// expression callbacks
void planeField(void **dst, void *src, void *fld, int num, int sub, int stp, int ftp)
{
    switch (stp) {
    default: {readField(stp,num,sub,datxPut(0,src),datxPut(1,fld),datxClr(2)); datxGet(2,dst);}
    break; case (TYPEExtend): {
    struct PlaneField usr = {num,sub};
    struct MergeStruct arg = {&usr,datxPut(0,src),datxPut(1,fld),datxClr(3)};
    mergeExtend(datxClr(2),centerField,&arg); datxGet(2,dst);}
    break; case (TYPECenter): {
    struct PlaneField usr = {num,sub};
    struct MergeStruct arg = {&usr,datxPut(0,src),datxPut(1,fld),datxClr(3)};
    mergeCenter(datxClr(2),centerField,&arg); datxGet(2,dst);}}
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
    char *str = malloc(strlen(src)+1); strcpy(str,src);
    if (waitSafe(stdioSem) != 0) ERROR();
    pushStrq(str,strout);
    // callHnfo and planeKnfo to allow planePutstr passed to slog
    // logging might happen in Configure callback
    if (postSafe(stdioSem) != 1) ERROR();
    planeHnfo(RegisterWake,(1<<PutsMsk),planeWots);
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
    ptr->loc.sub = centers; ptr->loc.asr = PlaceAsr; ptr->sav = ptr->loc; ptr->log = selfSmart(debug?"Argv":0);
    centerPlace(ptr,MainThd,0);}
    else if (hideMachine(&mchn, argv[i], &msiz)) {
    machineSwitch(&mchn,MainThd,0); freeMachine(&mchn);}
    else if (hideExpress(&expr, argv[i], &esiz)) {
    machineVoid(&expr); freeExpress(&expr);}
    else if (hideStr(&str,argv[i],&ssiz)) {
    planePutstr(str); freeStr(&str,1);}
    else {fprintf(stderr,"Argument:%d Center:%d Machine:%d Express:%d Str:%d unmatched:%s\n",asiz,csiz,msiz,esiz,ssiz,argv[i]); exit(-1);}}
}

// initialization
void initSafe()
{
    if (!(copySem = allocSafe(1))) ERROR(); // protect array of Center
    if (!(pipeSem = allocSafe(1))) ERROR(); // protect internal and response queues
    if (!(callSem = allocSafe(1))) ERROR(); // no Configure nested within
    if (!(machSem = allocSafe(1))) ERROR(); // variables shared by threads
    if (!(stdioSem = allocSafe(1))) ERROR(); // protect planeConsole queues
    if (!(pressSem = allocSafe(1))) ERROR(); // protect glfw queues
    if (!(timeSem = allocSafe(1))) ERROR(); // protect planeTime queue
    if (!(evalSem = allocSafe(1))) ERROR(); // protect data evaluation
    if (!(safeSem = allocSafe(1))) ERROR(); // protect thread semaphores
    internal = allocCenterq(); response = allocCenterq(); replace = allocCenterq();
    strout = allocStrq(); strin = allocStrq(); tempq = allocChrq();
    charq = allocIntq(); leftq = allocIntq(); baseq = allocIntq(); angleq = allocIntq();
    timeq = allocTimeq(); wakeq = allocIntq(); timep = allocTimep();
    ableq = allocIntq(); evntq = allocIntq(); virtq = allocIntq(); backq = allocIntq();
    enum Configure cfg[] = {
    RegisterOpen,RegisterWake,RegisterWane,RegisterAble,RegisterProt,RegisterTime,RegisterExit,RegisterVerb,
    UniformAll,UniformOne,UniformIdx,UniformUse,UniformTri,UniformNum,UniformVtx,UniformMat,UniformBas,UniformMod,UniformWid,UniformHei,
    ArgumentInp,ArgumentOut,ArgumentSrc,
    PressKey,PressQueue,
    ClickLeft,ClickBase,ClickAngle,ClickQueue,
    ManipLeft,ManipBase,ManipAngle,
    RegisterLog};
    callBack(cfg,sizeof(cfg)/sizeof(enum Configure),registerCall);
    datxSingle();
    datxFnptr(planeRetcfg,planeSetcfg,planeWoscfg,planeWoccfg,planeRawcfg,planeGetstr,planePutstr,planeField);
    start = processTime();
}
void initBoot()
{
    int size = 0; int cmnd = 0;
    for (int i = 0; Bootstrap__Int__Str(i); i++) size++;
    for (int i = 0; callCmnd(i); i++) {size++; cmnd++;}
    const char **temp = malloc(size*sizeof(const char *)); size = 0;
    for (int i = 0; Bootstrap__Int__Str(i); i++) temp[size++] = Bootstrap__Int__Str(i);
    for (int i = 0; callCmnd(i); i++) temp[size++] = callCmnd(i);
    char **boot = malloc(size*sizeof(char *));
    for (int i = 0; i < size; i++) {
    int len = strlen(temp[i]); boot[i] = malloc(len+1);
    strncpy(boot[i],temp[i],len); boot[i][len] = 0;}
    free(temp); temp = 0;
    // change strings according to sugar
    for (int i = 0; i < size; i++) {
    sugarRepl(&boot[i],'$'); // replace $() by Express
    sugarEval(planeSugar,boot[i],'!'); // evaluate !() in the embedding
    sugarFilt(&boot[i],'!');} // filter out !() before hide and process below
    // record which kinds of boot strings there are
    for (int i = size-cmnd; i < size; i++) {
    int asiz = 0; int csiz = 0; int msiz = 0; int esiz = 0; int ssiz = 0;
    struct Argument arg = {0}; struct Center cntr = {0}; struct Machine mchn = {0};
    struct Express expr = {0}; char *str = 0;
    if (hideArgument(&arg, boot[i], &asiz)) {planeInfo(RegisterShow,1,planeWots); freeArgument(&arg);}
    else if (hideCenter(&cntr, boot[i], &csiz)) {planeInfo(RegisterShow,2,planeWots); freeCenter(&cntr);}
    else if (hideMachine(&mchn, boot[i], &msiz)) {planeInfo(RegisterShow,4,planeWots); freeMachine(&mchn);}
    else if (hideExpress(&expr, boot[i], &esiz)) {planeInfo(RegisterShow,8,planeWots); freeExpress(&expr);}
    else if (hideStr(&str,boot[i],&ssiz)) {planeInfo(RegisterShow,16,planeWots); freeStr(&str,1);}
    else {fprintf(stderr,"Argument:%d Center:%d Machine:%d Express:%d Str:%d unmatched:%s\n",asiz,csiz,msiz,esiz,ssiz,boot[i]); exit(-1);}}
    planeArgv(size,boot);
    switch (planeInfo(RegisterPlan,0,planeRcfg)) {default: ERROR();
    break; case (Bringup): case (Builtin):
    planeJnfo(RegisterPoll,1,planeWcfg);
    planeJnfo(RegisterMain,planeSugval("@main"),planeWcfg);
    planeJnfo(RegisterSide,planeSugval("@side"),planeWcfg);
    planeJnfo(RegisterAble,((((1<<SlctMsk)|(1<<TimeMsk)|0)<<8)|(0<<4)|MachThd),planeWcfg);
    planeJnfo(RegisterAble,((((1<<DoneMsk)|0)<<8)|(1<<4)|MachThd),planeWcfg);
    planeJnfo(RegisterAble,((((1<<MachMsk)|0)<<8)|(2<<4)|MachThd),planeWcfg);
    planeJnfo(RegisterAble,((((1<<PutsMsk)|0)<<8)|(0<<4)|StdioThd),planeWcfg);
    planeJnfo(RegisterMask,((1<<TimeMsk)|0),planeWcfg);
    planeJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    planeJnfo(RegisterOpen,(1<<MachThd),planeWots);
    planeJnfo(RegisterOpen,(1<<PipeThd),planeWots);
    planeJnfo(RegisterOpen,(1<<StdioThd),planeWots);
    planeJnfo(RegisterOpen,(1<<TimeThd),planeWots);
    planeJnfo(RegisterTime,1000<<8,planeWcfg);
    break; case (Regress): case (Release):
    planeJnfo(RegisterMain,planeSugval("@main"),planeWcfg);
    planeJnfo(RegisterSide,planeSugval("@side"),planeWcfg);
    planeJnfo(RegisterAble,((((1<<SlctMsk)|(1<<PrssMsk)|(1<<ClckMsk)|(1<<MoveMsk)|(1<<RollMsk)|(1<<TimeMsk)|0)<<8)|(0<<4)|MachThd),planeWcfg);
    planeJnfo(RegisterAble,((((1<<DoneMsk)|0)<<8)|(1<<4)|MachThd),planeWcfg);
    planeJnfo(RegisterAble,((((1<<MachMsk)|0)<<8)|(2<<4)|MachThd),planeWcfg);
    planeJnfo(RegisterAble,((((1<<PutsMsk)|0)<<8)|(0<<4)|StdioThd),planeWcfg);
    planeJnfo(RegisterAble,((((1<<RespMsk)|0)<<8)|(0<<4)|PipeThd),planeWcfg);
    planeJnfo(RegisterMask,((1<<SlctMsk)|(1<<PrssMsk)|(1<<ClckMsk)|(1<<MoveMsk)|(1<<RollMsk)|(1<<TimeMsk)|0),planeWcfg);
    planeJnfo(RegisterOpen,(1<<FenceThd),planeWots);
    planeJnfo(RegisterOpen,(1<<MachThd),planeWots);
    planeJnfo(RegisterOpen,(1<<PipeThd),planeWots);
    planeJnfo(RegisterOpen,(1<<StdioThd),planeWots);}
    for (int i = 0; i < size; i++) free(boot[i]);
    free(boot);
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
    planeJnfo(RegisterOpen,(1<<TestThd),planeWots);

    int frames = planeInfo(ScratchFrames,0,planeRcfg);

    int test = selfSmart("Init");
    printfSmart(test,"test %d",test);
    deleteSmart(test);

    struct Extend *ptr = centerPull(Drawz,Move,MainThd,0,"Init0").ptr; freeCenter(ptr->ptr);
    ptr->ptr->mem = Drawz; ptr->ptr->siz = 1;
    allocDraw(&ptr->ptr->drw,ptr->ptr->siz);
    ptr->ptr->drw[0].con.tag = ResrcCon;
    ptr->ptr->drw[0].con.res = SwapRes;
    ptr->loc.sub = 0; ptr->loc.asr = CallAsr;
    ptr->sav.sub = Drawz; ptr->sav.asr = PlaceAsr;
    ptr->rsp = RptRsp; ptr->ret = NoneRet;
    if (debug) callCopy(ptr,0,"swap"); else callCont(ptr,0,ptr->log);
    while (1) {int siz = centerPull(Drawz,Chck,MainThd,0,"Init0").siz; if (siz) break; usleep(1000);}
    // UniformWid and UniformHei set by swap resize
    int width,height; {enum Configure cfg[2] = {UniformWid,UniformHei}; int val[2] = {0,0};
    callInfo(cfg,val,2,planeRcfg); width = val[0]; height = val[1];}
    // 0.83 = slope/full, 0.58 = slope*length/(full*full);
    int full = (width+height)/2; int length = 0.58*full/0.83; int slope = 0.83*full;
    {enum Configure cfg[3] = {FocalFull,FocalDepth,FocalSlope}; int val[3] = {full,length,slope};
    callInfo(cfg,val,3,planeWcfg);}

    ptr = centerPull(Drawz,Move,MainThd,0,"Init1").ptr; freeCenter(ptr->ptr);
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
    ptr->loc.sub = 0; ptr->loc.asr = CallAsr;
    ptr->sav.sub = Drawz; ptr->sav.asr = PlaceAsr;
    ptr->rsp = MptRsp; ptr->ret = NoneRet;
    if (debug) callCopy(ptr,0,"line"); else callCont(ptr,0,ptr->log);
    while (1) {int siz = centerPull(Drawz,Chck,MainThd,0,"Init0").siz; if (siz) break; usleep(1000);}

    for (int i = 0; i < frames; i++) {
    struct Extend *ptr = centerPull(Drawz,Move,MainThd,0,"Init2").ptr; freeCenter(ptr->ptr);
    ptr->ptr->mem = Drawz; ptr->ptr->siz = 1;
    allocDraw(&ptr->ptr->drw,ptr->ptr->siz);
    ptr->ptr->drw[0].con.tag = ResrcCon;
    ptr->ptr->drw[0].con.res = ChainRes;
    ptr->loc.sub = 0; ptr->loc.asr = CallAsr;
    ptr->sav.sub = Drawz; ptr->sav.asr = PlaceAsr;
    ptr->rsp = RptRsp; ptr->ret = NoneRet;
    if (debug) callCopy(ptr,0,"chain"); else callCont(ptr,0,ptr->log);
    while (1) {int siz = centerPull(Drawz,Chck,MainThd,0,"Init0").siz; if (siz) break; usleep(1000);}}

    struct Extend *uni = centerPull(Uniformz,Move,MainThd,0,"Init3").ptr; freeCenter(uni->ptr);
    uni->ptr->mem = Uniformz; uni->ptr->siz = 1; allocUniform(&uni->ptr->uni,uni->ptr->siz);
    uni->ptr->uni[0].all = 0; uni->ptr->uni[0].one = 1; uni->ptr->uni[0].pro = 2;
    uni->ptr->uni[0].wid = width; uni->ptr->uni[0].hei = height;
    uni->loc.sub = 0; uni->loc.asr = CallAsr;
    uni->sav.sub = Uniformz; uni->sav.asr = PlaceAsr;
    uni->rsp = RptRsp; uni->ret = NoneRet;
    if (debug) callCopy(uni,0,"uniform"); else callCont(uni,0,uni->log);
    {enum Configure cfg[2] = {UniformWid,UniformHei}; int val[2] = {width,height}; callJnfo(cfg,val,2,planeWcfg);}

    struct Extend *img = centerPull(Imagez,Move,MainThd,0,"Init4").ptr; freeCenter(img->ptr);
    img->ptr->mem = Imagez; img->ptr->siz = 1; allocImage(&img->ptr->img,img->ptr->siz);
    fmtxStbi(&img->ptr->img[0].dat,&img->ptr->img[0].wid,&img->ptr->img[0].hei,&img->ptr->img[0].cha,"texture.jpg");
    img->loc.sub = 0; img->loc.asr = CallAsr;
    img->sav.sub = Imagez; img->sav.asr = PlaceAsr;
    img->rsp = RptRsp; img->ret = NoneRet;
    if (debug) callCopy(img,0,"image"); else callCont(img,0,img->log);

    struct Extend *sto = centerPull(Storagez,Move,MainThd,0,"Init5").ptr; freeCenter(sto->ptr);
    sto->ptr->mem = Storagez; sto->ptr->siz = 1; allocInt32(&sto->ptr->sto,sto->ptr->siz);
    sto->ptr->sto[0] = 456;
    sto->loc.sub = 0; sto->loc.asr = CallAsr;
    sto->sav.sub = Storagez; sto->sav.asr = PlaceAsr;
    sto->rsp = RptRsp; sto->ret = NoneRet;
    if (debug) callCopy(sto,0,"storage"); else callCont(sto,0,sto->log);

    for (int i = 0; i < frames; i++) {
    struct Extend *mat = centerPull(Matrixz,Move,MainThd,0,"Init6").ptr; freeCenter(mat->ptr);
    mat->ptr->mem = Matrixz; mat->ptr->siz = 5; allocMatrix(&mat->ptr->mat,mat->ptr->siz);
    float ident[16]; identmat(ident,4);
    float proj[16]; planeWindow(proj);
    copymat(mat->ptr->mat[0].mat,ident,4); // uni.all
    copymat(mat->ptr->mat[1].mat,ident,4); // uni.one
    copymat(mat->ptr->mat[2].mat,proj,4);  // uni.pro
    copymat(mat->ptr->mat[3].mat,ident,4); // tri.pol
    copymat(mat->ptr->mat[4].mat,ident,4); // tri.pol
    mat->loc.sub = 0; mat->loc.asr = CallAsr;
    mat->sav.sub = Matrixz; mat->sav.asr = PlaceAsr;
    mat->rsp = RptRsp; mat->ret = NoneRet;
    if (debug) callCopy(mat,0,"initmat"); else callCont(mat,0,mat->log);
    while (1) {int siz = centerPull(Matrixz,Chck,MainThd,0,"Init0").siz; if (siz) break; callWait();}}

    struct Extend *bup = centerPull(Bringupz,Move,MainThd,0,"Init7").ptr; freeCenter(bup->ptr);
    bup->ptr->mem = Bringupz; bup->ptr->siz = sizeof(vertices)/sizeof(struct Vertex); allocVertex(&bup->ptr->ver,bup->ptr->siz);
    for (int i = 0; i < bup->ptr->siz; i++) memcpy(&bup->ptr->ver[i],&vertices[i],sizeof(struct Vertex));
    bup->loc.sub = 0; bup->loc.asr = CallAsr;
    bup->sav.sub = Bringupz; bup->sav.asr = PlaceAsr;
    bup->rsp = RptRsp; bup->ret = NoneRet;
    if (debug) callCopy(bup,0,"bringup"); else callCont(bup,0,bup->log);

    struct Extend *idt = centerPull(Identz,Move,MainThd,0,"Init8").ptr; freeCenter(idt->ptr);
    idt->ptr->mem = Identz; idt->ptr->siz = sizeof(primitive)/sizeof(uint32_t); allocInt32(&idt->ptr->idt,idt->ptr->siz);
    for (int i = 0; i < idt->ptr->siz; i++) memcpy(&idt->ptr->idt[i],&primitive[i],sizeof(uint32_t));
    idt->loc.sub = 0; idt->loc.asr = CallAsr;
    idt->loc.sub = Identz; idt->loc.asr = PlaceAsr;
    idt->rsp = RptRsp; idt->ret = NoneRet;
    if (debug) callCopy(idt,0,"ident"); else callCont(idt,0,idt->log);

    struct Extend *ind = centerPull(Indexz,Move,MainThd,0,"Init9").ptr; freeCenter(ind->ptr);
    ind->ptr->mem = Indexz; ind->ptr->siz = sizeof(indices)/sizeof(int32_t); allocInt32(&ind->ptr->ind,ind->ptr->siz);
    memcpy(ind->ptr->ind,indices,sizeof(indices)); // note that two int16_t are packed into each int32_t; don't care
    ind->loc.sub = 0; ind->loc.asr = CallAsr;
    ind->loc.sub = Indexz; ind->loc.asr = PlaceAsr;
    ind->rsp = RptRsp; ind->ret = NoneRet;
    if (debug) callCopy(ind,0,"index"); else callCont(ind,0,ind->log);

    struct Extend *vtx = centerPull(Vertexz,Move,MainThd,0,"Init10").ptr; freeCenter(vtx->ptr);
    vtx->ptr->mem = Vertexz; vtx->ptr->siz = sizeof(vertices)/sizeof(struct Vertex); allocVertex(&vtx->ptr->vtx,vtx->ptr->siz);
    for (int i = 0; i < vtx->ptr->siz; i++) memcpy(&vtx->ptr->vtx[i],&vertices[i],sizeof(struct Vertex));
    // for (int i = 4; i < 8; i++) vtx->ptr->vtx[i].vec[2] = 0.9;
    vtx->loc.sub = 0; vtx->loc.asr = CallAsr;
    vtx->loc.sub = Vertexz; vtx->loc.asr = PlaceAsr;
    vtx->rsp = RptRsp; vtx->ret = NoneRet;
    if (debug) callCopy(vtx,0,"vertex"); else callCont(vtx,0,vtx->log);

    struct Extend *tri = centerPull(Trianglez,Move,MainThd,0,"Init11").ptr; freeCenter(tri->ptr);
    tri->ptr->mem = Trianglez; tri->ptr->siz = (sizeof(indices)/sizeof(uint16_t))/3; allocTriangle(&tri->ptr->tri,tri->ptr->siz);
    for (int i = 0; i < tri->ptr->siz; i++) for (int j = 0; j < 3; j++) {
    int ind = j+i*3; if ((ind/3)/2 != i/2) ERROR(); // three indices per triangle, two triangles per polytope
    tri->ptr->tri[i].vtx[j] = indices[ind]; tri->ptr->tri[i].tex = i/2; tri->ptr->tri[i].num = tri->ptr->tri[i].pol = (i/2?4:3);}
    /*for (int i = 0; i < tri->ptr->siz; i++) {
    fprintf(stderr,"triangle number:%d texture:%d polytope:%d\n",i,tri->ptr->tri[i].tex,tri->ptr->tri[i].pol);
    for (int j = 0; j < 4; j++) {fprintf(stderr,"corner number:%d",tri->ptr->tri[i].vtx[j]);
    fprintf(stderr," %f",vtx->ptr->vtx[tri->ptr->tri[i].vtx[j]].vec[0]);
    for (int k = 1; k < 4; k++) fprintf(stderr,"/%f",vtx->ptr->vtx[tri->ptr->tri[i].vtx[j]].vec[k]);
    fprintf(stderr,"\n");}}*/
    // for (int i = 0; i < tri->ptr->siz; i++)
    // {char *st0 = 0; showTriangle(&tri->ptr->tri[i],&st0); fprintf(stderr,"Trianglez %s\n",st0); free(st0);}
    tri->loc.sub = 0; tri->loc.asr = CallAsr;
    tri->sav.sub = Trianglez; tri->sav.asr = PlaceAsr;
    tri->rsp = RptRsp; tri->ret = NoneRet;
    if (debug) callCopy(tri,0,"triangle"); else callCont(tri,0,tri->log);

    int giv[] = {width,height};
    int giw[] = {0,12}; // idx,siz
    for (int i = 0; i < 2; i++) {
    struct Extend *fil = centerPull(Drawz,Move,MainThd,0,"Init12").ptr; freeCenter(fil->ptr);
    fil->ptr->mem = Drawz; fil->ptr->siz = 1; allocDraw(&fil->ptr->drw,fil->ptr->siz);
    fil->ptr->drw[0].con.tag = MicroCon;
    fil->ptr->drw[0].con.mic = (i?(mode?MicroFetRel:MicroVtxRel):MicroFilRel);
    fil->ptr->drw[0].siz = sizeof((i?giw:giv))/sizeof(int);
    allocInt(&fil->ptr->drw[0].arg,fil->ptr->drw[0].siz);
    for (int j = 0; j < fil->ptr->drw[0].siz; j++) fil->ptr->drw[0].arg[j] = (i?giw:giv)[j];
    fil->loc.sub = 0; fil->loc.asr = CallAsr;
    fil->sav.sub = Drawz; fil->sav.asr = PlaceAsr;
    fil->rsp = RetRsp; fil->ret = NoneRet;
    if (debug) callCopy(fil,0,"relate"); else callCont(fil,0,fil->log);
    while (1) {int siz = centerPull(Drawz,Chck,MainThd,0,"Init0").siz; if (siz) break; callWait();}}

    struct Extend *start = 0; allocExtend(&start,1);
    start->loc.sub = planeSugval("@scratch2"); start->loc.asr = PlaceAsr;
    start->sav = start->loc;
    centerPlace(start,MainThd,0);}

    break; case(Regress): case(Release): break;}
}

// called by main
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
    callHnfo = hnfo;
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
    waitSafe(copySem); copyEot = 1; postSafe(copySem);
    waitSafe(pipeSem); pipeEot = 1; postSafe(pipeSem);
    waitSafe(safeSem); safeEot = 1; postSafe(safeSem);
    return 0;
}
void planeDone()
{
    // TODO free local and such allocated in centerSize
    for (int i = 0; i < sizeMch; i++) free(reboot[i]); free(reboot);
    for (int i = 0; i < sizeMch; i++) free(recent[i]); free(recent);
    free(resize);
    clearSmart();
    doneSmart();
    datxNon();
}
