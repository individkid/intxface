#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "stlx.h"
#include "type.h"
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#ifdef __APPLE__
#include <dispatch/dispatch.h>
#define sem_t dispatch_semaphore_t
#define sem_init(S,P,V) {*S = dispatch_semaphore_create(V);}
#define sem_post(S) {dispatch_semaphore_signal(*S);}
#define sem_wait(S) {dispatch_semaphore_wait(*S,DISPATCH_TIME_FOREVER);}
#else
#include <semaphore.h>
#endif
#include <setjmp.h>
#include <signal.h>

// Order of matrix application is window * project * subject * object * element * vector.
// To change X such that YX changes to ZYX, change X to Y’ZXY.
// Each matrix is product of local, towrite, written, maintain.
struct Kernel {
	int optimize;
	struct Matrix compose;
	struct Matrix maintain;
	struct Matrix written;
	struct Matrix towrite;
	struct Matrix local;
	struct Matrix inverse;
};
// owned by main thread:
// TODO Add map from polytope to matrix, captured from copy to Triangle buffer.
struct Kernel *matrix = 0;
struct Pierce *found = 0;
struct Pierce pierce = {0};
struct Pierce unfound = {0};
struct Machine *machine = 0;
int *intstk = 0;
int numstk = 0;
int idxstk = 0;
int configure[Configures] = {0};
struct Center *center = 0;
int sub0 = 0;
int idx0 = 0;
void **dat0 = 0;
int running = 0;
void planeStarted(int val);
int planeRunning();
int planeCall(void **dat, const char *str);
// constant after other threads start:
int external = 0;
int wakeup = 0;
void *internal = 0;
void *response = 0;
vftype callSafe = 0;
zftype callLoop = 0;
zftype callBlock = 0;
yftype callPhase = 0;
uftype callCopy = 0;
rftype callReady = 0;
xftype callDone = 0;
sftype callWake = 0;
tftype callInfo = 0;
pthread_t thread[Threads];
pthread_key_t retstr;
// resource protected:
char **string = 0;
int strsiz = 0;
int qsize = 0;
int qfull = 0;
int qwait = 0;
int qhead = 0;
int qtail = 0;
enum Configure *hints = 0;
enum Phase *waits = 0;
enum Thread *procs = 0;
// thread safe:
sem_t resource;
sem_t pending;
sem_t ready[Threads];

DECLARE_DEQUE(struct Center *,Centerq)

unsigned char *moveCursor(int dim, int e, int t, int r, int b, int l) {
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
unsigned char *rotateCursor(int e) {
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
unsigned char *translateCursor(int e) {
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
unsigned char *refineCursor() {
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
unsigned char *sculptCursor(int e) {
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
// not all combinations of Effect Fixed Tool are supported.
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
typedef float *(*planeXform)(float *mat, float *fix, float *nrm, float *org, float *cur);
planeXform planeFunc()
{
    int tmp; int cfg = configure[ManipFixed];
    tmp = ((1<<Slide)|(1<<Ortho)|(1<<Mouse)); if ((cfg&tmp)==tmp) return planeSlideOrthoMouse;
    tmp = ((1<<Rotate)|(1<<Focal)|(1<<Mouse)); if ((cfg&tmp)==tmp) return planeRotateFocalMouse;
    tmp = ((1<<Rotate)|(1<<Cursor)|(1<<Roller)); if ((cfg&tmp)==tmp) return planeRotateCursorRoller;
    return 0;
}
float *planeMatrix(float *mat)
{
    return mat; // TODO
}
extern struct timeval planraTime;
float *planraMatrix(float *mat)
{
    struct timeval stop; gettimeofday(&stop, NULL);
    float time = (stop.tv_sec - planraTime.tv_sec) + (stop.tv_usec - planraTime.tv_usec) / (double)MICROSECONDS;
    float fix[3]; fix[0] = 0.0; fix[1] = 0.0; fix[2] = 0.0;
    float nml[3]; nml[0] = 0.0; nml[1] = 0.0; nml[2] = -1.0;
    float org[3]; org[0] = 0.0; org[1] = 0.0; org[2] = 0.0;
    float cur[3]; cur[0] = 0.2*sinf(time*2.0944);
    cur[1] = 0.2*cosf(time*2.0944);
    cur[2] = time*1.5708;
    identmat(mat,4);
    planeSlideOrthoMouse(mat,fix,nml,org,cur);
    planeRotateFocalMouse(mat,fix,nml,org,cur);
    planeRotateCursorRoller(mat,fix,nml,org,cur);
    return identmat(mat,4);
}
void physicalToScreen(float *xptr, float *yptr)
{
    int width, height, xphys, yphys;
    width = callInfo(MonitorWidth); height = callInfo(MonitorHeight);
    xphys = callInfo(PhysicalWidth); yphys = callInfo(PhysicalHeight);
    *xptr *= width/xphys; *yptr *= height/yphys;
}
void physicalFromScreen(float *xptr, float *yptr)
{
    int width, height, xphys, yphys;
    width = callInfo(MonitorWidth); height = callInfo(MonitorHeight);
    xphys = callInfo(PhysicalWidth); yphys = callInfo(PhysicalHeight);
    *xptr *= xphys/width; *yptr *= yphys/height;
}
void screenToWindow(float *xptr, float *yptr)
{
    int width, height, left, base;
    width = callInfo(WindowWidth); height = callInfo(WindowHeight);
    left = callInfo(WindowLeft); base = callInfo(WindowBase);
    *xptr -= left; *yptr -= base; *xptr /= width; *yptr /= height;
}
void screenFromWindow(float *xptr, float *yptr)
{
    int width, height, left, base;
    width = callInfo(WindowWidth); height = callInfo(WindowHeight);
    left = callInfo(WindowLeft); base = callInfo(WindowBase);
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
struct Pierce *planePierce()
{
	if (found) return found;
	struct Center *ptr = callReady(Piercez);
	if (configure[ClosestFind]) {
	for (int i = 0; i < ptr->siz; i++) {
	struct Pierce *temp = ptr->pie + i;
	if (!found || !found->vld || (temp->vld && temp->fix[2] < found->fix[2])) found = temp;}} else {
	int index = configure[PierceIndex] - configure[PierceBase];
	if (index >= 0 && index < ptr->siz) found = ptr->pie + index;}
	if (!found) found = &unfound; else {pierce = *found; found = &pierce;}
	callDone(ptr);
	return found;
}
void planeString()
{
	sem_wait(&resource);
	configure[StringSize] = strsiz;
	sem_post(&resource);
}
void planeStage(enum Configure cfg)
{
	switch (cfg) {
	case (StringSize): planeString(); break;
	case (RegisterDone): configure[RegisterDone] = callInfo(RegisterDone); break;
	case (CenterMemory): configure[CenterMemory] = center->mem; break;
	case (CenterSize): configure[CenterSize] = center->siz; break;
	case (CenterIndex): configure[CenterIndex] = center->idx; break;
	case (CenterSelf): configure[CenterSelf] = center->slf; break;
	case (ClosestValid): configure[ClosestValid] = planePierce()->vld; break;
	case (ClosestIndex): configure[ClosestIndex] = planePierce()->idx; break;
	case (ClosestPoly): configure[ClosestPoly] = planePierce()->pol; break;
	case (ClosestLeft): configure[ClosestLeft] = planePierce()->fix[0]; break;
	case (ClosestBase): configure[ClosestBase] = planePierce()->fix[1]; break;
	case (ClosestNear): configure[ClosestNear] = planePierce()->fix[2]; break;
	case (NormalLeft): configure[NormalLeft] = planePierce()->nml[0]; break;
	case (NormalBase): configure[NormalBase] = planePierce()->nml[1]; break;
	case (NormalNear): configure[NormalNear] = planePierce()->nml[2]; break;
	case (CursorLeft): configure[CursorLeft] = callInfo(CursorLeft); break;
	case (CursorBase): configure[CursorBase] = callInfo(CursorBase); break;
	case (CursorAngle): configure[CursorAngle] = callInfo(CursorAngle); break;
	case (CursorPress): configure[CursorPress] = callInfo(CursorPress); break;
	case (WindowLeft): configure[WindowLeft] = callInfo(WindowLeft); break;
	case (WindowBase): configure[WindowBase] = callInfo(WindowBase); break;
	case (WindowWidth): configure[WindowWidth] = callInfo(WindowWidth); break;
	case (WindowHeight): configure[WindowHeight] = callInfo(WindowHeight); break;
	case (MonitorWidth): configure[MonitorWidth] = callInfo(MonitorWidth); break;
	case (MonitorHeight): configure[MonitorHeight] = callInfo(MonitorHeight); break;
	case (PhysicalWidth): configure[PhysicalWidth] = callInfo(PhysicalWidth); break;
	case (PhysicalHeight): configure[PhysicalHeight] = callInfo(PhysicalHeight); break;
	default: break;}
}
void *planeResize(void *ptr, int mod, int siz, int tmp)
{
	char *result = realloc(ptr,siz*mod);
	for (int i = tmp*mod; i < siz*mod; i++) result[i] = 0;
	return result;
}
void *planeRebase(void *ptr, int mod, int siz, int bas, int tmp)
{
	char *chrs = ptr;
	char *result = malloc(siz*mod);
	int ofs = tmp*mod-bas*mod; // location of old base in new base
	int lim = siz*mod-tmp*mod+bas*mod; // location of new limit in old base
	while (ofs < 0) ofs += siz*mod; ofs %= siz*mod;
	while (lim < 0) lim += siz*mod; lim %= siz*mod;
	for (int i = 0; i < lim; i++) result[i+ofs] = chrs[i];
	for (int i = 0; i < ofs; i++) result[i] = chrs[i+lim];
	free(ptr);
	return result;
}
void planeStarted(int tmp)
{
	int done = 0; int todo = 0;
	int started = configure[RegisterOpen];
	int init = configure[RegisterInit];
	todo = started & ~tmp; done = tmp & ~started;
	for (enum Thread bit = 0; bit < Threads; bit++)
	if (done & (1<<bit)) planeSafe(bit,Stop,Configures);
	for (enum Thread bit = 0; bit < Threads; bit++) {
	if (init & (1<<bit)) planeSafe(bit,Init,Configures);
	if (todo & (1<<bit)) planeSafe(bit,Start,Configures);}
}
void planeConfig(enum Configure cfg, int val)
{
	int tmp = 0;
	if (cfg < 0 || cfg >= Configures) ERROR();
	tmp = configure[cfg]; configure[cfg] = val;
	switch (cfg) {
	case (MatrixSize): matrix = planeResize(matrix,sizeof(struct Kernel),val,tmp); break;
	case (MatrixBase): matrix = planeRebase(matrix,sizeof(struct Kernel),configure[MatrixSize],val,tmp); break;
	case (MachineSize): machine = planeResize(machine,sizeof(struct Machine),val,tmp); break;
	case (RegisterOpen): planeStarted(tmp); break;
	case (ClosestFind): found = 0; break;
	default: break;}
}
void planeSync(enum Configure cfg, int val)
{
	struct Center *center = 0;
	allocCenter(&center,1);
	center->mem = Configurez;
	center->siz = 1;
	allocConfigure(&center->cfg,1);
	allocInt(&center->val,1);
	center->cfg[0] = cfg;
	center->val[0] = val;
	callCopy(&center);
	allocCenter(&center,0);
}
void planeCont()
{
	// TODO applies inverse of new transformation to local, so the switch to the new transformation is continuous.
}
void planePrep()
{
	// TODO applies local to to-send, and schedules send.
}
void planeSend()
{
	// TODO applies to-send to sent and writes composition of all but local.
}
void planeRecv()
{
	// TODO either applies part of sent to received, or replaces received and compensates sent such that its delta from received is unchanged.
}
void planeDisp()
{
	// TODO conjoins product of local, to-send, sent, received with window, project, maybe subject, maybe object
}
void planeCopy(struct Center **given)
{
	struct Center *center = *given;
	fprintf(stderr,"planeCopy %d %d\n",center->mem,center->siz);
	switch (center->mem) {
	case (Stackz): for (int i = 0; i < center->siz; i++) planeCall(dat0,center->str[i]); break;
	case (Machinez): for (int i = 0; i < center->siz; i++) {
		int index = center->idx+i;
		if (index < 0 || index >= configure[MachineSize]) ERROR();
		if (center->mch[i].xfr == Name) {void *dat = 0; datxInt(&dat,index+1);
		datxInserts("_",center->mch[i].str,dat,identType("Int")); free(dat);}
		copyMachine(&machine[index],&center->mch[i]);} break;
	case (Configurez): for (int i = 0; i < center->siz; i++)
		planeConfig(center->cfg[i],center->val[i]);
		fprintf(stderr,"callCopy\n");
		callCopy(given); break;
	default: callCopy(given); break;}
}
int planeEscape(int lvl, int nxt)
{
	int inc = (lvl > 0 ? 1 : (lvl == 0 ? 0 : -1)); lvl *= inc;
	for (nxt += inc; lvl > 0 && nxt < configure[MachineSize] && nxt >= 0; nxt += inc)
	if (machine[nxt].xfr == Nest) lvl += machine[nxt].lvl*inc;
	return nxt;
}
int planeIval(struct Express *exp)
{
	void *dat = 0; int val = 0; int typ = 0;
	typ = datxEval(&dat,exp,identType("Int"));
	if (typ != identType("Int")) ERROR();
	val = *datxIntz(0,dat); free(dat);
	return val;
}
void planeProduce(vftype func)
{
	freeCenter(center);
	allocCenter(&center,0);
	func();
	if (center == 0) allocCenter(&center,1);
}
void planeConsume(vftype func)
{
	func();
	center = 0;
	allocCenter(&center,1);
}
void planeRead()
{
	sem_wait(&resource);
	center = maybeCenterq(0,internal);
	sem_post(&resource);
}
void planeWrite()
{
	sem_wait(&resource);
	pushCenterq(center,response);
	writeInt(0,wakeup);
	sem_post(&resource);
}
void planeEcho()
{
	if (configure[ResultType] != identType("Center")) ERROR();
	readCenter(center,idx0);
}
void planeHide()
{
	// TODO planePopstr
	// TODO try hideArgument and open external
	// TODO try hideCenter
	// TODO try hideExpress and datxEval
	// TODO try hideMachine and planeSwitch
	// TODO otherwise planePutstr
}
int planeSwitch(struct Machine *mptr, int next)
{
	// {char *xfr = 0; showTransfer(mptr->xfr,&xfr);
	// printf("planeSwitch %d %s\n",next,xfr); free(xfr);}
	switch (mptr->xfr) {
	case (Read): planeProduce(planeRead); break;
	case (Write): planeConsume(planeWrite); break;
	case (Stage): for (int i = 0; i < mptr->siz; i++) planeStage(mptr->sav[i]); break;
	case (Force): for (int i = 0; i < mptr->num; i++) {
	planeConfig(mptr->cfg[i],mptr->val[i]); planeSync(mptr->cfg[i],mptr->val[i]);} break;
	case (Cont): planeCont(); break;
	case (Prep): planePrep(); break;
	case (Send): planeSend(); break;
	case (Recv): planeRecv(); break;
	case (Disp): planeDisp(); break;
	case (Copy): planeCopy(&center); break;
	case (Jump): next = planeEscape(planeIval(&mptr->exp[0]),next) - 1; break;
	case (Goto): next = next + planeIval(&mptr->exp[0]) - 1; break;
	case (Nest): break;
	case (Name): if (idxstk > 0) next = next - 1; else ERROR(); break;
	case (Eval): configure[ResultType] = datxEval(dat0,&mptr->exp[0],-1); break;
	case (Echo): planeProduce(planeEcho); break;
	case (Hide): planeHide(); break;
	default: break;}
	return next+1;
}
void planeMicro()
{
	while (configure[ResultLine] >= 0 && configure[ResultLine] < configure[MachineSize]) {
	struct Machine *mptr = machine+configure[ResultLine];
	int next = planeSwitch(mptr,configure[ResultLine]);
	if (next == configure[ResultLine]) break;
	configure[ResultLine] = next;}
}
int planeCall(void **dat, const char *str)
{
	void *nam = 0; int typ = 0;
	if (str == 0) {assignDat(dat,*dat0); return configure[ResultType];}
	typ = datxFinds(&nam,"_",str);
	if (typ != identType("Int")) ERROR();
	if (idxstk >= numstk) {
	intstk = realloc(intstk,(idxstk+1)*sizeof(int));
	while (idxstk >= numstk) intstk[numstk++] = 0;}
	intstk[idxstk++] = configure[ResultLine];
	configure[ResultLine] = *datxIntz(0,nam);
	planeMicro();
	configure[ResultLine] = intstk[--idxstk];
	assignDat(dat,*dat0);
	free(nam);
	return configure[ResultType];
}
void planeWake(enum Configure hint)
{
	configure[ResultHint] = hint;
	if (configure[ResultLine] < 0) configure[ResultLine] = 0;
	if (configure[ResultLine] >= configure[MachineSize]) configure[ResultLine] = 0;
	planeMicro();
}
void planeBoot()
{
	for (int i = 0; Bootstrap__Int__Str(i); i++) {
	struct Machine mptr = {0}; int len = 0;
	if (!hideMachine(&mptr,Bootstrap__Int__Str(i),&len)) ERROR();
	planeSwitch(&mptr,0);}
}
char *planePopstr() // non-const return means caller owns it
{
	char *ret;
	sem_wait(&resource);
	ret = strdup(string[0]);
	strsiz--;
	for (int i = 0; i < strsiz; i++) string[i] = string[i+1];
	string = realloc(string,strsiz*sizeof(char*));
	sem_post(&resource);
	return ret;
}
void planePutstr(const char *str) // const given means caller owns it
{
	sem_wait(&resource);
	strsiz++;
	string = realloc(string,strsiz*sizeof(char*));
	string[strsiz-1] = strdup(str);
	sem_post(&resource);
}
void planeOutstr(const char *str)
{
	write(STDIN_FILENO,str,strlen(str));
}
void planeSetcfg(int val, int sub)
{
	if (sub < 0 || sub >= Configures) ERROR();
	planeConfig(sub,val); planeSync(sub,val);
}
int planeGetcfg(int sub)
{
	if (sub < 0 || sub >= Configures) ERROR();
	return configure[sub];
}
void planeTerm(int sig)
{
}
void *planeSelect(void *ptr)
{
	char *str = 0; // FIXME let planeHide do this when planePopstr is Argument
	if ((external = identWrap(Planez,str)) < 0) exitErr(__FILE__,__LINE__); free(str);
	sem_post(&ready[Select]);
	while (1) {
	int sub = waitRead(0,(1<<external)|(1<<wakeup));
	if (sub == wakeup) {
	struct Center *center = 0;
	if (!checkRead(wakeup)) break;
	readInt(wakeup);
	sem_wait(&resource);
	center = maybeCenterq(0,response);
	sem_post(&resource);
	writeCenter(center,external);
	freeCenter(center);
	allocCenter(&center,0);}
	else if (sub == external) {
	struct Center *center = 0;
	if (!checkRead(external)) break;
	allocCenter(&center,1);
	readCenter(center,external);
	sem_wait(&resource);
	pushCenterq(center,internal);
	sem_post(&resource);
	planeSafe(Threads,Phases,CenterMemory); callSafe();}
	else break;}
	planeSafe(Select,Stop,Configures); callSafe();
	return 0;
}
void *planeConsole(void *ptr)
{
	char chr[2] = {0};
	int val = 0;
	int nfd = 0;
	char *str = 0;
	fd_set fds, ers;
	sem_post(&ready[Console]);
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
	// planeDupstr(&str,-1,2,0); planeInsstr(chr,1,2,strlen(str)); free(str); // FIXME
	}
	planeSafe(Console,Stop,Configures); callSafe();
	return 0;
}
void planePredo(enum Thread bit)
{
	switch (bit) {default: callPhase(bit,Init);
	break; case(Select): ERROR();
	break; case(Console): ERROR();}
}
void planeThread(enum Thread bit)
{
	if ((running & (1<<bit)) != 0) return; running |= (1<<bit);
	switch (bit) {default: sem_post(&ready[bit]); callPhase(bit,Start);
	break; case (Select): if (pthread_create(&thread[bit],0,planeSelect,0) != 0) ERROR();
	break; case (Console): if (pthread_create(&thread[bit],0,planeConsole,0) != 0) ERROR();}
	if ((configure[RegisterOpen] & (1<<bit)) == 0) {
		configure[RegisterOpen] |= (1<<bit); planeSafe(Threads,Phases,RegisterOpen);}
}
void planeFinish(enum Thread bit)
{
	if ((running & (1<<bit)) == 0) return; running &= ~(1<<bit);
	sem_wait(&ready[bit]); switch (bit) {default: callPhase(bit,Stop);
	break; case (Select): closeIdent(external); if (pthread_join(thread[bit],0) != 0) ERROR();
	break; case (Console): close(STDIN_FILENO); if (pthread_join(thread[bit],0) != 0) ERROR();}
	if ((configure[RegisterOpen] & (1<<bit)) != 0) {
		configure[RegisterOpen] &= ~(1<<bit); planeSafe(Threads,Phases,RegisterOpen);}
}
void planePhase(enum Thread bit, enum Phase phase)
{
	switch(phase) {default: ERROR();
	break; case (Init): planePredo(bit);
	break; case (Start): planeThread(bit);
	break; case (Stop): planeFinish(bit);}
}
void wrapPlane();
void planeInit(vftype init, vftype boot, vftype main, zftype loop, zftype block, yftype phase,//)
	vftype safe, uftype copy, rftype _ready, xftype done, sftype wake, tftype info)
{
	struct sigaction act;
	act.sa_handler = planeTerm;
	if (sigaction(SIGTERM,&act,0) < 0) ERROR();
	if (pthread_key_create(&retstr,free) != 0) ERROR();
	sem_init(&resource,0,1); sem_init(&pending,0,0);
	for (enum Thread bit = 0; bit < Threads; bit++) sem_init(&ready[bit],0,0);
	{struct Center tmp = {0}; allocCenter(&center,1); *center = tmp;}
	internal = allocCenterq(); response = allocCenterq();
	wrapPlane(); datxCaller(planeCall);
	sub0 = datxSub(); idx0 = puntInit(sub0,sub0,datxReadFp,datxWriteFp); dat0 = datxDat(sub0);
	callLoop = loop; callBlock = block; callPhase = phase;
	callSafe = safe; callCopy = copy; callReady = _ready;
	callDone = done; callWake = wake; callInfo = info;
	init(); // planePutstr on argv
	boot(); // initial planeEnque
	main(); // planeDeque vulkanChange
	while (sizeCenterq(internal)) planeRead();
	while (sizeCenterq(response)) {
	struct Center *ptr = maybeCenterq(0,response);
	freeCenter(ptr); allocCenter(&ptr,0);}
	freeCenterq(internal);
}
void planeReady(struct Center *ptr)
{
	// TODO handle pierce point in automatic mode
}
void planeDone(struct Center *ptr)
{
	freeCenter(ptr); allocCenter(&ptr,0);
}
void planeEnque(enum Thread thread, enum Phase phase, enum Configure hint)
{
	sem_wait(&resource);
	if (qfull == qsize) {qsize++;
	procs = realloc(procs,qsize*sizeof(enum Thread));
	waits = realloc(waits,qsize*sizeof(enum Phase));
	hints = realloc(hints,qsize*sizeof(enum Configure));
	for (int i = qsize-1; i > qhead; i--) {
	procs[i] = procs[i-1]; waits[i] = waits[i-1]; hints[i] = hints[i-1];}
	qhead++; if (qhead == qsize) qhead = 0;}
	procs[qtail] = thread; waits[qtail] = phase; hints[qtail] = hint;
	qtail++; if (qtail == qsize) qtail = 0;
	qfull++;
	if (qwait) {qwait = 0; sem_post(&pending);}
	sem_post(&resource);
}
void planeDeque(enum Thread *thread, enum Phase *phase, enum Configure *hint)
{
	sem_wait(&resource);
	if (qfull == 0) ERROR();
	*thread = procs[qhead]; *phase = waits[qhead]; *hint = hints[qhead];
	qhead++; if (qhead == qsize) qhead = 0;
	qfull--;
	sem_post(&resource);
}
int planeCheck()
{
	sem_wait(&resource);
	if (qfull == 0) {sem_post(&resource); return 0;}
	sem_post(&resource); return 1;
}
void planeBlock()
{
	sem_wait(&resource);
	if (qfull > 0) {sem_post(&resource); return;}
	qwait = 1; sem_post(&resource);
	sem_wait(&pending);
}
void planeSafe(enum Thread thread, enum Phase phase, enum Configure hint)
{
	planeEnque(thread,phase,hint);
}
int planeLoop()
{
	enum Thread thread = 0; enum Phase phase = 0; enum Configure hint = 0;
	if (!planeCheck()) return 0;
	planeDeque(&thread,&phase,&hint);
	if (phase != Phases && hint != Configures) ERROR();
	if (phase == Phases && hint == Configures && thread != Threads) ERROR();
	if (phase == Phases && hint != Configures) callWake(hint);
	if (phase != Phases && hint == Configures) planePhase(thread,phase);
	return planeCheck();
}
void planeMain()
{
	while (1) {
	int plane = planeLoop();
	int call = callLoop();
	if (!plane && !call) {
	callWake(ResultHint);
	sem_wait(&resource);
	if (!qfull && !running) {sem_post(&resource); break;}
	if (qfull) {sem_post(&resource); callWake(ResultHint); continue;}
	sem_post(&resource);
	if (!callBlock()) planeBlock();}}
}
