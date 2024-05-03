#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
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
#define sem_safe(S,F) {sem_wait(S);F;sem_post(S);}
#include <setjmp.h>
#include <signal.h>

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
struct Kernel *matrix = 0;
struct Pierce *pierce = 0;
struct Pierce *found = 0;
struct Pierce unfound = {0};
struct Machine *machine = 0;
int *intstk = 0;
int numstk = 0;
int idxstk = 0;
int configure[Configures] = {0};
struct Center center = {0};
int sub0 = 0;
int idx0 = 0;
void **dat0 = 0;
int running = 0;
void planeStarted(int val);
int planeRunning();
int planeCall(void **dat, const char *str);
// constant after other threads start:
int internal = 0;
int external = 0;
uftype callDma = 0;
vftype callSafe = 0;
yftype callMain = 0;
xftype callInfo = 0;
wftype callDraw = 0;
sftype callWake = 0;
pthread_t thread[Threads];
pthread_key_t retstr;
// owned by a single thread until joined:
int test = 0;
int check = 0;
// resource protected:
char **string = 0;
int strsiz = 0;
int strmsk = 0;
int numpipe = 0;
int calling = 0;
int qsize = 0;
int qfull = 0;
int qhead = 0;
int qtail = 0;
enum Configure *hints = 0;
enum Wait *waits = 0;
enum Thread *procs = 0;
// thread safe:
sem_t resource;
sem_t pending;
sem_t ready[Threads];
void planeRead();
void planeDupstr(char **ptr, int len, int idx, int loc);
void planeInsstr(const char *src, int len, int idx, int loc);
void planeDelstr(int len, int idx, int loc);
void planeOutstr(const char *str);
void planeAddarg(const char *str);
int planeEnque(enum Thread proc, enum Wait wait, enum Configure hint);
void planeDeque(enum Thread *proc, enum Wait *wait, enum Configure *hint);
void planeSafe(enum Thread proc, enum Wait wait, enum Configure hint);

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

float *planeCenter()
{
	int index = configure[MatrixIndex]-center.idx;
	if (index < 0 || index >= center.siz) ERROR();
	if (center.mem != Matrixz) ERROR();
	return center.mat[index].mat;
}
struct Kernel *planeKernel()
{
	int index = 0; int base = 0; int size = 0;
	base = configure[MatrixBase]; size = configure[MatrixSize];
	index = configure[MatrixIndex] - base;
	if (index < 0 || index >= size) ERROR();
	return matrix + index;
}
float *planeInverse()
{
	return planeKernel()->inverse.mat;
}
float *planeMaintain()
{
	struct Kernel *ptr = planeKernel();
	ptr->optimize = 0;
	return ptr->maintain.mat;
}
float *planeWritten()
{
	struct Kernel *ptr = planeKernel();
	ptr->optimize = 0;
	return ptr->written.mat;
}
float *planeTowrite()
{
	struct Kernel *ptr = planeKernel();
	ptr->optimize = 0;
	return ptr->towrite.mat;
}
float *planeCompose()
{
	struct Kernel *ptr = planeKernel();
	if (ptr->optimize) return ptr->compose.mat; else ptr->optimize = 1;
	return jumpmat(jumpmat(copymat(ptr->compose.mat,planeMaintain(),4),planeWritten(),4),planeTowrite(),4);
}
planeXform planeFunc()
{
	switch ((enum Tool)configure[ManipulateTool]) {
	case (Mouse): switch ((enum Effect)configure[ManipulateEffect]) {
	case (Slide): switch ((enum Fixed)configure[ManipulateFixed]) {
	case (Cursor): ERROR();
	case (Focal): ERROR();
	case (Ortho): return planeSlideOrthoMouse;
	case (Normal): ERROR();
	default: ERROR();}
	case (Rotate): switch ((enum Fixed)configure[ManipulateFixed]) {
	case (Cursor): ERROR();
	case (Focal): return planeRotateFocalMouse;
	case (Ortho): ERROR();
	case (Normal): ERROR();
	default: ERROR();}
	case (Scale): switch ((enum Fixed)configure[ManipulateFixed]) {
	case (Cursor): ERROR();
	case (Focal): ERROR();
	case (Ortho): ERROR();
	case (Normal): ERROR();
	default: ERROR();}
	default: ERROR();}
	case (Roller): switch ((enum Effect)configure[ManipulateEffect]) {
	case (Slide): switch ((enum Fixed)configure[ManipulateFixed]) {
	case (Cursor): ERROR();
	case (Focal): ERROR();
	case (Ortho): ERROR();
	case (Normal): ERROR();
	default: ERROR();}
	case (Rotate): switch ((enum Fixed)configure[ManipulateFixed]) {
	case (Cursor): return planeRotateCursorRoller;
	case (Focal): ERROR();
	case (Ortho): ERROR();
	case (Normal): ERROR();
	default: ERROR();}
	case (Scale): switch ((enum Fixed)configure[ManipulateFixed]) {
	case (Cursor): ERROR();
	case (Focal): ERROR();
	case (Ortho): ERROR();
	case (Normal): ERROR();
	default: ERROR();}
	default: ERROR();}
	default: ERROR();}
	return 0;
}
float *planeLocal()
{
	float fix[3]; float nrm[3]; float org[4]; float cur[4];
	// assume focal point is zero,zero,zero
	// Closest and Origin are on line with focal point
	// WindowNear OriginNear CursorNear are focal length
	fix[0] = configure[ClosestLeft];
	fix[1] = configure[ClosestBase];
	fix[2] = configure[ClosestNear];
	nrm[0] = configure[NormalLeft];
	nrm[1] = configure[NormalBase];
	nrm[2] = configure[NormalNear];
	org[0] = configure[OriginLeft];
	org[1] = configure[OriginBase];
	org[2] = configure[OriginNear];
	org[3] = configure[OriginAngle];
	cur[0] = configure[CursorLeft];
	cur[1] = configure[CursorBase];
	cur[2] = configure[CursorNear];
	cur[3] = configure[CursorAngle];
	return planeFunc()(planeKernel()->local.mat,fix,nrm,org,cur);
}
void *planeConjoin(float *mat, float *jct)
{
	float inv[4]; return timesmat(jumpmat(mat,jct,4),invmat(copymat(inv,jct,4),4),4);
}
float *planeProject(float *mat)
{
	float at0 = -configure[WindowLength];
	float at1 = (configure[WindowFar]+configure[WindowNear])/2.0;
	float den = at1-at0;
	mat[0] = configure[WindowWide]/2.0;
	mat[5] = configure[WindowHigh]/2.0;
	mat[10] = (configure[WindowFar]-configure[WindowNear])/2.0;
	mat[12] = configure[WindowLeft]+mat[0];
	mat[13] = configure[WindowBase]+mat[5];
	mat[14] = configure[WindowNear]+mat[10];
	mat[15] = 1.0; invmat(mat,4);
	mat[3] = 0.0; mat[7] = 0.0;
	mat[11] = 1.0/den; mat[15] = -at0/den;
	return mat;
}
struct Pierce *planePierce()
{
	if (found) return found;
	if (configure[ClosestFind]) {
	for (int i = 0; i < configure[PierceSize]; i++) {
	struct Pierce *temp = pierce + i;
	if (!found || !found->vld || (temp->vld && temp->fix[2] < found->fix[2])) found = temp;}} else {
	int index = configure[PierceIndex] - configure[PierceBase];
	if (index >= 0 && index < configure[PierceSize]) found = pierce + index;}
	if (!found) found = &unfound;
	return found;
}
void planeString()
{
	sem_safe(&resource,{configure[StringMask] &= strmsk; strmsk ^= configure[StringMask];});
}
void planeStage(enum Configure cfg)
{
	switch (cfg) {
	case (StringMask): planeString(); break;
	case (RegisterDone): configure[RegisterDone] = callInfo(RegisterDone); break;
	case (CenterMemory): configure[CenterMemory] = center.mem; break;
	case (CenterSize): configure[CenterSize] = center.siz; break;
	case (CenterIndex): configure[CenterIndex] = center.idx; break;
	case (CenterSelf): configure[CenterSelf] = center.slf; break;
	case (ClosestValid): configure[ClosestValid] = planePierce()->vld; break;
	case (ClosestFound): configure[ClosestFound] = planePierce()->idx; break;
	case (ClosestFile): configure[ClosestFile] = planePierce()->pol; break;
	case (ClosestLeft): configure[ClosestLeft] = planePierce()->fix[0]; break;
	case (ClosestBase): configure[ClosestBase] = planePierce()->fix[1]; break;
	case (ClosestNear): configure[ClosestNear] = planePierce()->fix[2]; break;
	case (NormalLeft): configure[NormalLeft] = planePierce()->nml[0]; break;
	case (NormalBase): configure[NormalBase] = planePierce()->nml[1]; break;
	case (NormalNear): configure[NormalNear] = planePierce()->nml[2]; break;
	case (WindowLeft): configure[WindowLeft] = callInfo(WindowLeft); break;
	case (WindowBase): configure[WindowBase] = callInfo(WindowBase); break;
	case (WindowWide): configure[WindowWide] = callInfo(WindowWide); break;
	case (WindowHigh): configure[WindowHigh] = callInfo(WindowHigh); break;
	case (CursorLeft): configure[CursorLeft] = callInfo(CursorLeft); break;
	case (CursorBase): configure[CursorBase] = callInfo(CursorBase); break;
	case (CursorNear): configure[CursorNear] = callInfo(CursorNear); break;
	case (CursorAngle): configure[CursorAngle] +=/*accumulate*/ callInfo(CursorAngle); break;
	case (CursorClick): configure[CursorClick] = callInfo(CursorClick); break;
	case (OriginLeft): configure[OriginLeft] = configure[CursorLeft]; break;
	case (OriginBase): configure[OriginBase] = configure[CursorBase]; break;
	case (OriginNear): configure[OriginNear] = configure[CursorNear]; break;
	case (OriginAngle): configure[OriginAngle] = configure[CursorAngle]; configure[CursorAngle] = 0; break;
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
	int done = 0; int todo = 0; int started = 0;
	started = configure[RegisterOpen]; todo = started & ~tmp; done = tmp & ~started;
	for (enum Thread bit = 0; bit < Threads; bit++) if (done & (1<<bit)) planeSafe(bit,Stop,Configures);
	for (enum Thread bit = 0; bit < Threads; bit++) if (todo & (1<<bit)) planeSafe(bit,Start,Configures);
}
void planeConfig(enum Configure cfg, int val)
{
	int tmp = 0;
	if (cfg < 0 || cfg >= Configures) ERROR();
	tmp = configure[cfg]; configure[cfg] = val;
	switch (cfg) {
	case (PierceSize): pierce = planeResize(pierce,sizeof(struct Pierce),val,tmp); break;
	case (PierceBase): pierce = planeRebase(pierce,sizeof(struct Pierce),configure[PierceSize],val,tmp); break;
	case (MatrixSize): matrix = planeResize(matrix,sizeof(struct Kernel),val,tmp); break;
	case (MatrixBase): matrix = planeRebase(matrix,sizeof(struct Kernel),configure[MatrixSize],val,tmp); break;
	case (MachineSize): machine = planeResize(machine,sizeof(struct Machine),val,tmp); break;
	case (RegisterOpen): planeStarted(tmp); break;
	case (ClosestFind): found = 0; break;
	default: break;}
}
void planeDma(enum Configure cfg, int val)
{
	struct Center tmp = {0};
	tmp.mem = Configurez;
	tmp.siz = 1;
	allocConfigure(&tmp.cfg,1);
	allocInt(&tmp.val,1);
	tmp.cfg[0] = cfg;
	tmp.val[0] = val;
	callDma(&tmp);
	freeCenter(&tmp);
}
void planeCopy(struct Center *ptr)
{
	switch (ptr->mem) {
	case (Piercez): for (int i = 0; i < ptr->siz; i++) {
		int index = ptr->idx+i-configure[PierceBase];
		if (index < 0 || index >= configure[PierceSize]) ERROR();
		copyPierce(&pierce[index],&ptr->pie[i]);}
		callDma(ptr); break;
	case (Stackz): for (int i = 0; i < ptr->siz; i++) planeCall(dat0,ptr->str[i]); break;
	case (Machinez): for (int i = 0; i < ptr->siz; i++) {
		int index = ptr->idx+i;
		if (index < 0 || index >= configure[MachineSize]) ERROR();
		if (ptr->mch[i].xfr == Name) {void *dat = 0; datxInt(&dat,index+1);
		datxInserts("_",ptr->mch[i].str,dat,identType("Int")); free(dat);}
		copyMachine(&machine[index],&ptr->mch[i]);} break;
	case (Configurez): for (int i = 0; i < ptr->siz; i++)
		planeConfig(ptr->cfg[i],ptr->val[i]);
		callDma(ptr); break;
	default: callDma(ptr); break;}
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
void planeFill()
{
	int src = 0; int dst = 0; int siz = 0;
	int idx = configure[PierceIndex];
	if (center.mem != Piercez) ERROR();
	src = idx-configure[PierceBase]; siz = configure[PierceSize]; dst = idx-center.idx;
	if (src < 0 || src >= siz || dst < 0 || dst >= center.siz) ERROR();
	copyPierce(&center.pie[dst],&pierce[src]);
}
int planeSwitch(struct Machine *mptr, int next)
{
	// {char *xfr = 0; showTransfer(mptr->xfr,&xfr);
	// printf("planeSwitch %d %s\n",next,xfr); free(xfr);}
	switch (mptr->xfr) {
	case (Read): planeRead(); break;
	case (Write): writeCenter(&center,external); break;
	case (Stage): for (int i = 0; i < mptr->siz; i++) planeStage(mptr->sav[i]); break;
	case (Force): for (int i = 0; i < mptr->num; i++) {
	planeConfig(mptr->cfg[i],mptr->val[i]); planeDma(mptr->cfg[i],mptr->val[i]);} break;
	case (Pose): copymat(planeCenter(),planeTowrite(),4); break;
	case (Other): copymat(planeCenter(),planeMaintain(),4); break;
	case (Prep): copymat(planeCenter(),planeLocal(),4);
	planeStage(OriginLeft); planeStage(OriginBase); planeStage(OriginAngle); break;
	case (Conj): planeConjoin(planeCenter(),planeCompose()); break;
	case (Glitch): copymat(planeMaintain(),planeCenter(),4); break;
	case (Check): jumpmat(planeMaintain(),planeCenter(),4);
	timesmat(planeWritten(),invmat(copymat(planeInverse(),planeCenter(),4),4),4); break;
	case (Compl): jumpmat(planeTowrite(),planeCenter(),4); identmat(planeCenter(),4); break;
	case (Apply): jumpmat(planeWritten(),planeTowrite(),4); identmat(planeTowrite(),4); break;
	case (Accum): jumpmat(planeMaintain(),planeWritten(),4); identmat(planeWritten(),4); break;
	case (Drop): copymat(planeCenter(),planeMaintain(),4); identmat(planeMaintain(),4); break;
	case (Proj): planeProject(planeCenter()); break;
	case (Copy): planeCopy(&center); break;
	case (Draw): callDraw(configure[ArgumentMicro],configure[ArgumentBase],configure[ArgumentLimit]); break;
	case (Jump): next = planeEscape(planeIval(&mptr->exp[0]),next) - 1; break;
	case (Goto): next = next + planeIval(&mptr->exp[0]) - 1; break;
	case (Nest): break;
	case (Name): if (idxstk > 0) next = next - 1; else ERROR(); break;
	case (Eval): configure[ResultType] = datxEval(dat0,&mptr->exp[0],-1); break;
	case (Echo): if (configure[ResultType] == identType("Center")) readCenter(&center,idx0); else ERROR(); break;
	case (Fill): planeFill(); break;
	default: break;}
	return next+1;
}
void planeLoop()
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
	planeLoop();
	configure[ResultLine] = intstk[--idxstk];
	assignDat(dat,*dat0);
	free(nam);
	return configure[ResultType];
}
void planeWake(enum Configure hint)
{
	configure[ResultHint] = hint;
	if (configure[ResultLine] < 0 || configure[ResultLine] >= configure[MachineSize]) configure[ResultLine] = 0;
	planeLoop();
}
void planeBoot()
{
	for (int i = 0; Bootstrap__Int__Str(i); i++) {
	struct Machine mptr = {0}; int len = 0;
	if (!hideMachine(&mptr,Bootstrap__Int__Str(i),&len)) ERROR();
	planeSwitch(&mptr,0);}
}
void planeRead()
{
	int num = 0; sem_safe(&resource,{if ((num = numpipe)) numpipe--;});
	if (num) readCenter(&center,internal);
	else {struct Center tmp = {0}; center = tmp;}
}
void planeDupstr(char **ptr, int len, int idx, int loc)
{
	sem_wait(&resource);
	while (strsiz <= idx) {strsiz++; string = realloc(string,strsiz*sizeof(char*)); string[strsiz-1] = strdup("");}
	if (len < 0) len = strlen(string[idx])+1+len; if (idx < 0) idx = strsiz+idx; if (loc < 0) loc = strlen(string[idx])+1+loc;
	// TODO check for errors in len idx loc
	*ptr = strndup(string[idx],len);
	sem_post(&resource);
}
void planeInsstr(const char *src, int len, int idx, int loc)
{
	int num = 0; for (int i = 0; i < len; i++) if (!src[i]) num++;
	sem_wait(&resource);
	while (strsiz <= idx) {strsiz++; string = realloc(string,strsiz*sizeof(char*)); string[strsiz-1] = strdup("");}
	if (len < 0) len = strlen(src)+1+len; if (idx < 0) idx = strsiz+idx; if (loc < 0) loc = strlen(string[idx])+1+loc;
	// TODO check for errors in len idx loc
	strsiz += num; string = realloc(string,strsiz*sizeof(char*));
	for (int i = strsiz-1; i > idx+num; i--) string[i] = string[i-num];
	for (int i = idx+1; i < idx+1+num; i++) string[i] = strdup("");
	if (num > 0) {
		char *str = 0;
		free(string[idx+num]); string[idx+num] = strdup(string[idx]+loc);
		string[idx][loc] = 0; str = strdup(string[idx]);
		free(string[idx]); string[idx] = str;}
	while (num > 0) {
		char *str = 0; char *tmp = 0;
		str = malloc(loc+strlen(src)+strlen(string[idx]+loc)+1); tmp = strdup(string[idx]+loc);
		strncpy(str,string[idx],loc); strcat(str,src); strcat(str,tmp);
		free(tmp); free(string[idx]); string[idx] = str;
		strmsk |= 1<<idx; src += strlen(src)+1; len -= strlen(src)+1; idx++; loc = 0; num--;}
	if (len > 0) {
		char *str = 0; char *tmp = 0;
		str = malloc(loc+strlen(src)+len+strlen(string[idx]+loc)+1); tmp = strdup(string[idx]+loc);
		strncpy(str,string[idx],loc); strncat(str,src,len); strcat(str,tmp);
		free(tmp); free(string[idx]); string[idx] = str;
		strmsk |= 1<<idx; src = 0; len = 0; idx++; loc = 0;}
	sem_post(&resource);
	planeSafe(Threads,Waits,StringMask);
}
void planeDelstr(int len, int idx, int loc)
{
	ERROR(); // TODO
}
void planeOutstr(const char *str)
{
	write(STDIN_FILENO,str,strlen(str));
}
void planeAddarg(const char *str)
{
	sem_wait(&resource);
	strsiz++;
	string = realloc(string,strsiz*sizeof(char*));
	string[strsiz-1] = strdup(str);
	sem_post(&resource);
}
void planeSetcfg(int val, int sub)
{
	if (sub < 0 || sub >= Configures) ERROR();
	planeConfig(sub,val);
	planeDma(sub,val);
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
	char *str = 0; planeDupstr(&str,-1,1,0); if ((external = identWrap(Planez,str)) < 0) exitErr(__FILE__,__LINE__); free(str);
	sem_post(&ready[Select]);
	while (1) {
	struct Center center = {0};
	int sub = waitRead(0,1<<external);
	if (sub != external) break;
	if (!checkRead(external)) break;
	if (!checkWrite(internal)) break;
	readCenter(&center,external);
	writeCenter(&center,internal);
	sem_safe(&resource,{numpipe++;});
	planeSafe(Threads,Waits,CenterMemory);}
	planeSafe(Select,Stop,Configures);
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
	planeDupstr(&str,-1,2,0); planeInsstr(chr,1,2,strlen(str)); free(str);}
	planeSafe(Console,Stop,Configures);
	return 0;
}
void *planeTest(void *ptr)
{
	test = check = 100000;
	sem_post(&ready[Test]);
	while (test--) {
	planeSafe(Threads,Waits,Configures);}
	planeSafe(Test,Stop,Configures);
	return 0;
}
void planeThread(enum Thread bit)
{
	if ((running & (1<<bit)) != 0) return; running |= (1<<bit);
	switch (bit) {
	case (Select): if (pthread_create(&thread[bit],0,planeSelect,0) != 0) ERROR(); break;
	case (Console): if (pthread_create(&thread[bit],0,planeConsole,0) != 0) ERROR(); break;
	case (Window): case (Graphics): case (Process): sem_post(&ready[bit]); break;
	case (Test): if (pthread_create(&thread[bit],0,planeTest,0) != 0) ERROR(); break;
	default: ERROR();}
	if ((configure[RegisterOpen] & (1<<bit)) == 0) {
		configure[RegisterOpen] |= (1<<bit); planeSafe(Threads,Waits,RegisterOpen);}
}
void planeFinish(enum Thread bit)
{
	if ((running & (1<<bit)) == 0) return; running &= ~(1<<bit);
	sem_wait(&ready[bit]); switch (bit) {
	case (Select): closeIdent(external); if (pthread_join(thread[bit],0) != 0) ERROR(); break;
	case (Console): close(STDIN_FILENO); if (pthread_join(thread[bit],0) != 0) ERROR(); break;
	case (Window): case (Graphics): case (Process): break;
	case (Test): if (pthread_join(thread[bit],0) != 0) ERROR(); break;
	default: ERROR();}
	if ((configure[RegisterOpen] & (1<<bit)) != 0) {
		configure[RegisterOpen] &= ~(1<<bit); planeSafe(Threads,Waits,RegisterOpen);}
}
void wrapPlane();
void planeInit(zftype init, uftype dma, vftype safe, yftype main, xftype info, wftype draw, rftype pierce, sftype wake, vftype boot)
{
	struct sigaction act;
	act.sa_handler = planeTerm;
	if (sigaction(SIGTERM,&act,0) < 0) ERROR();
	if (pthread_key_create(&retstr,free) != 0) ERROR();
	sem_init(&resource,0,1); sem_init(&pending,0,0);
	for (enum Thread bit = 0; bit < Threads; bit++) sem_init(&ready[bit],0,0);
	if ((internal = openPipe()) < 0) ERROR();
	wrapPlane();
	datxCaller(planeCall);
	sub0 = datxSub(); idx0 = puntInit(sub0,sub0,datxReadFp,datxWriteFp); dat0 = datxDat(sub0);
	callDma = dma; callSafe = safe; callMain = main; callInfo = info; callDraw = draw; callWake = wake;
	init(); boot(); while (1) {
	enum Wait wait = 0; enum Configure hint = 0;
	sem_safe(&resource,{if (!qfull && !running) break;});
	planeMain();} closeIdent(internal); if (check) ERROR();
}
int planeInfo(enum Configure cfg)
{
	return configure[cfg];
}
int planeEnque(enum Thread proc, enum Wait wait, enum Configure hint)
{
	int run = 0;
	sem_wait(&resource);
	run = calling;
	if (proc == Process && wait == Start) calling++;
	if (proc == Process && wait == Stop) calling--;
	if (qfull == qsize) {qsize++;
	procs = realloc(procs,qsize*sizeof(enum Thread));
	waits = realloc(waits,qsize*sizeof(enum Wait));
	hints = realloc(hints,qsize*sizeof(enum Configure));
	for (int i = qsize-1; i > qhead; i--) {
	procs[i] = procs[i-1]; waits[i] = waits[i-1]; hints[i] = hints[i-1];}
	qhead++; if (qhead == qsize) qhead = 0;}
	procs[qtail] = proc; waits[qtail] = wait; hints[qtail] = hint;
	qtail++; if (qtail == qsize) qtail = 0;
	qfull++;
	if (qfull == 1) sem_post(&pending);
	sem_post(&resource);
	return run;
}
void planeDeque(enum Thread *proc, enum Wait *wait, enum Configure *hint)
{
	int idle = 0;
	sem_wait(&pending);
	sem_wait(&resource);
	if (qfull == 0) ERROR();
	*proc = procs[qhead]; *wait = waits[qhead]; *hint = hints[qhead];
	qhead++; if (qhead == qsize) qhead = 0;
	qfull--;
	if (qfull > 0) sem_post(&pending);
	else if (*hint != ResultHint) idle = 1;
	sem_post(&resource);
	if (idle) planeSafe(Threads,Waits,ResultHint);
}
void planeSafe(enum Thread proc, enum Wait wait, enum Configure hint)
{
	if (planeEnque(proc,wait,hint)) callSafe();
}
void planeMain()
{
	enum Thread proc = 0; enum Wait wait = 0; enum Configure hint = 0;
	planeDeque(&proc,&wait,&hint);
	if (wait == Waits && hint == Configures && proc == Threads) {check--; return;}
	if (wait != Waits && hint != Configures) ERROR();
	if (wait == Waits && hint == Configures) ERROR();
	if (wait == Waits && hint != Configures) callWake(hint);
	if (wait == Start && hint == Configures) {planeThread(proc); callMain(proc,wait);}
	if (wait == Stop && hint == Configures) {planeFinish(proc); callMain(proc,wait);}
}
void planeReady(struct Pierce *given)
{
	// TODO remove this, and add callReady to get latest pierce
	for (int i = 0; i < configure[PierceSize]; i++) pierce[i] = given[i]; found = 0;
}

int planraOnce;
int planraDone;
pthread_t planraThread;
struct timeval planraTime;
void planraExit(int enb)
{
	struct Center center;
	enum Configure cfg = RegisterOpen; int val = enb;
	center.mem = Configurez; center.idx = 0; center.siz = 1; center.slf = 1;
	center.cfg = &cfg; center.val = &val; callDma(&center);
}
float *planraMatrix(float *mat)
{
	struct timeval stop; gettimeofday(&stop, NULL);
	float time = (stop.tv_sec - planraTime.tv_sec) + (stop.tv_usec - planraTime.tv_usec) / (double)MICROSECONDS;
	float fix[3]; fix[0] = 0.0; fix[1] = 0.0; fix[2] = 0.0;
	float nml[3]; nml[0] = 0.0; nml[1] = 0.0; nml[2] = -1.0;
	float org[3]; org[0] = 0.0; org[1] = 0.0; org[2] = 0.0;
	float cur[3]; cur[0] = 0.2*sinf(time*2.0944);
	if (time > 0.5) planraExit(0);
	cur[1] = 0.2*cosf(time*2.0944);
	// cur[0] = cur[1] = 0.0;
	cur[2] = time*1.5708;
	identmat(mat,4);
	planeSlideOrthoMouse(mat,fix,nml,org,cur);
	planeRotateFocalMouse(mat,fix,nml,org,cur);
	planeRotateCursorRoller(mat,fix,nml,org,cur);
	return mat;
}
int planraCenter(int num, struct Center ptr[2])
{
	if (num < 1 || ptr[0].siz != 0 || ptr[0].vtx != 0) ERROR();
	if (num < 2 || ptr[1].siz != 0 || ptr[1].vtx != 0) ERROR();
	num = 0;
	int len = 0; char *str; char *tmp;
	float mat[16]; planraMatrix(mat);
	if (planraOnce) {
		int len = 0; char *str; char *tmp;
		ptr[num].mem = Vertexz; ptr[num].siz = 6; ptr[num].idx = 0; ptr[num].slf = 0;
		allocVertex(&ptr[num].vtx,6);
		asprintf(&str,"Vertex(");
		asprintf(&str,"%svec[0]:Old(0.5)vec[1]:Old(-0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
		asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
		len = 0; hideVertex(&ptr[num].vtx[0],str,&len); free(str);
		asprintf(&str,"Vertex(");
		asprintf(&str,"%svec[0]:Old(0.5)vec[1]:Old(0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
		asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
		len = 0; hideVertex(&ptr[num].vtx[1],str,&len); free(str);
		asprintf(&str,"Vertex(");
		asprintf(&str,"%svec[0]:Old(-0.5)vec[1]:Old(-0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
		asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
		len = 0; hideVertex(&ptr[num].vtx[2],str,&len); free(str);
		asprintf(&str,"Vertex(");
		asprintf(&str,"%svec[0]:Old(-0.5)vec[1]:Old(0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
		asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
		len = 0; hideVertex(&ptr[num].vtx[3],str,&len); free(str);
		asprintf(&str,"Vertex(");
		asprintf(&str,"%svec[0]:Old(-0.5)vec[1]:Old(-0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
		asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
		len = 0; hideVertex(&ptr[num].vtx[4],str,&len); free(str);
		asprintf(&str,"Vertex(");
		asprintf(&str,"%svec[0]:Old(0.5)vec[1]:Old(0.5)vec[2]:Old(0.5)vec[3]:Old(1.0)",tmp = str); free(tmp);
		asprintf(&str,"%sref[0]:Int32(0)ref[1]:Int32(0)ref[2]:Int32(0)ref[3]:Int32(0))",tmp = str); free(tmp);
		len = 0; hideVertex(&ptr[num].vtx[5],str,&len); free(str);
		num++;}
	ptr[num].mem = Matrixz; ptr[num].siz = 1; ptr[num].idx = 0; ptr[num].slf = 0;
	allocMatrix(&ptr[num].mat,1);
	asprintf(&str,"Matrix(");
	for (int j = 0; j < 16; j++) asprintf(&str,"%smat[%d]:Old(%f)",tmp = str,j,mat[j]); free(tmp);
	asprintf(&str,"%s)",tmp = str); free(tmp);
	len = 0; hideMatrix(&ptr[num].mat[0],str,&len); free(str);
	num++;
	planraOnce = 0;
	return num;
}
void *planraTest(void *arg)
{
    int done = 0;
    while (!done) {
        usleep(10000);
        callSafe();
    	sem_safe(&resource,{done = planraDone;});}
    return 0;
}
struct timeval planraTime;
float planraDebug()
{
	struct timeval tempTime;
	gettimeofday(&tempTime, NULL);
	float time = (tempTime.tv_sec - planraTime.tv_sec) + (tempTime.tv_usec - planraTime.tv_usec) / (double)MICROSECONDS;
	planraTime = tempTime;
	return time;
}
void planraWake(enum Configure hint)
{
	if (planraDone) return;
	if (callInfo(RegisterOpen) == 0) {
		sem_safe(&resource,{planraDone = 1;});
    	if (pthread_join(planraThread,0) != 0) ERROR();
		planeSafe(Process,Stop,Configures);
		planeSafe(Graphics,Stop,Configures);
		planeSafe(Window,Stop,Configures);
		return;
	}
	if (hint == KeyboardPress) {
		int key1 = callInfo(KeyboardPress);
		int key2 = callInfo(KeyboardPress);
		if (key1 == 256 && key2 == 257) planraExit(1);
		if (key1 == 256 && key2 == 0) {struct Center center;
		enum Configure cfg = KeyboardPress; int val = 256;
		center.mem = Configurez; center.idx = 0; center.siz = 1; center.slf = 1;
		center.cfg = &cfg; center.val = &val; callDma(&center);}
	}
	if (hint == ResultHint) {
		struct Center testCenter[2];
		for (int i = 0; i < 2; i++) memset(testCenter+i,0,sizeof(struct Center));
		int num = planraCenter(2,testCenter); // TODO allocat and free
		for (int i = 0; i < num; i++) callDma(testCenter+i);
		callDraw(MicroPRB,0,6);
	}
}
void planraBoot()
{
	gettimeofday(&planraTime, NULL);
	planeSafe(Window,Start,Configures);
	planeSafe(Graphics,Start,Configures);
	planeSafe(Process,Start,Configures);
	planraDone = 0; planraOnce = 1;
    if (pthread_create(&planraThread,0,planraTest,0) != 0) ERROR();
}

