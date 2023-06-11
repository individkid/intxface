#include "plane.h"
#include "face.h"
#include "metx.h"
#include "datx.h"
#include "luax.h"
#include "type.h"
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
#include <math.h>
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
	struct Matrix compose;
	struct Matrix maintain;
	struct Matrix written;
	struct Matrix towrite;
	struct Matrix local;
	struct Matrix inverse;
};
// owned by main thread:
struct Kernel *subject = 0;
struct Kernel *object = 0;
struct Kernel *element = 0;
struct Pierce *pierce = 0;
struct Pierce *found = 0;
struct Pierce unfound = {0};
struct Machine *machine = 0;
float *floats = 0;
int configure[Configures] = {0};
struct Center center = {0};
int sub0 = 0;
int idx0 = 0;
void **dat0 = 0;
int started = 0;
int running = 0;
void planeStarted(int val);
int planeRunning();
// constant after other threads start:
int internal = 0;
int external = 0;
uftype callDma = 0;
vftype callSafe = 0;
yftype callMain = 0;
xftype callInfo = 0;
wftype callDraw = 0;
pthread_t thread[Procs];
pthread_key_t retstr;
// resource protected:
int numpipe = 0;
char **strings = 0;
int numstr = 0;
int rspidx = 0;
int calling = 0;
int qsize = 0;
int qfull = 0;
int qhead = 0;
int qtail = 0;
enum Configure *hints = 0;
enum Wait *waits = 0;
enum Proc *procs = 0;
// thread safe:
sem_t resource;
sem_t pending;
sem_t ready[Procs];
void planeRead();
const char *planeGet(int idx);
int planeSet(int idx, const char *str);
int planeCat(int idx, const char *str);
int planeEnque(enum Proc proc, enum Wait wait, enum Configure hint);
void planeDeque(enum Proc *proc, enum Wait *wait, enum Configure *hint);
void planeSafe(enum Proc proc, enum Wait wait, enum Configure hint);

typedef float *(*planeXform)(float *mat, float *fix, float *nrm, float *org, float *cur);
// mat:current-matrix pic:focal-point fix:pierce-point org:pierce-cursor-roller cur:current-cursor-roller
float *planeSlideOrthoMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// distance to perpendicular to ortho fixed; cursor mapped
	identmat(mat,4);
	for (int i = 0; i < 3; i++) mat[12+i] = cur[i]-org[i];
	return mat;
}
float *planeSlideFocalMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// distance to perpendicular to cursor fixed; cursor mapped
	// <P-F,N> is distance from P to plane
	// P-<P-F,N>N is projection P' onto plane
	// offset is sQ such that <sQ-F,N> = 0
	// s = <F,N>/<Q,N>
	float neg[3]; scalevec(copyvec(neg,fix,3),-1.0,3);
	float nrg[3]; normvec(plusvec(copyvec(nrg,org,3),neg,3),3);
	float nur[3]; normvec(plusvec(copyvec(nur,cur,3),neg,3),3);
	float num = dotvec(fix,nrg,3);
	float den = dotvec(nur,nrg,3);
	float ofs[3]; scalevec(copyvec(ofs,nur,3),num/den,3);
	identmat(mat,4);
	for (int i = 0; i < 3; i++) mat[12+i] = ofs[i];
	return mat;
}
float *planeSlideNormalMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// distance to perpendicular to normal fixed; cursor mapped
	float neg[3]; scalevec(copyvec(neg,fix,3),-1.0,3);
	float nnm[3]; normvec(copyvec(nnm,nrm,3),3);
	float nur[3]; normvec(plusvec(copyvec(nur,cur,3),neg,3),3);
	float num = dotvec(fix,nnm,3);
	float den = dotvec(nur,nnm,3);
	float ofs[3]; scalevec(copyvec(ofs,nur,3),num/den,3);
	identmat(mat,4);
	for (int i = 0; i < 3; i++) mat[12+i] = ofs[i];
	return mat;
}
float *planeRotate(float *mat, float *axs, int ang)
{
	float cmp = cosf(ang);
	float smp = sinf(ang);
	float tmp = 1-cmp;
	mat[0] = cmp+axs[0]*axs[0]*tmp;
	mat[1] = axs[1]*axs[0]*tmp+axs[2]*smp;
	mat[2] = axs[2]*axs[0]*tmp-axs[1]*smp;
	mat[3] = 0.0;
	mat[4] = axs[0]*axs[1]*tmp-axs[2]*smp;
	mat[5] = cmp+axs[1]*axs[1]*tmp;
	mat[6] = axs[2]*axs[1]*tmp+axs[0]*smp;
	mat[7] = 0.0;
	mat[8] = axs[0]*axs[2]*tmp+axs[1]*smp;
	mat[9] = axs[1]*axs[2]*tmp-axs[0]*smp;
	mat[10] = cmp+axs[2]*axs[2]*tmp;
	mat[11] = 0.0;
	mat[12] = 0.0; mat[13] = 0.0; mat[14] = 0.0; mat[15] = 1.0;
	return mat;
}
float *planeRotateOrthoMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// perpendicular to ortho, parallel to picture, fixed; cursor mapped
	float neg[3]; scalevec(copyvec(neg,fix,3),-1.0,3);
	float ncl[3]; normvec(copyvec(ncl,neg,3),3);
	float nrg[3]; normvec(plusvec(copyvec(nrg,org,3),neg,3),3);
	float nur[3]; normvec(plusvec(copyvec(nur,cur,3),neg,3),3);
	float xrg[3]; crossvec(ncl,nrg);
	float xur[3]; crossvec(ncl,nur);
	float ang = asinf(sqrtf(dotvec(xur,xur,3)))-asinf(sqrtf(dotvec(xrg,xrg,3)));
	float axs[3]; normvec(xur,3);
	return planeRotate(mat,axs,ang);
}
float *planeRotateFocalMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// perpendicular to cursor, parallel to picture, fixed; cursor mapped
	float neg[3]; scalevec(copyvec(neg,fix,3),-1.0,3);
	float nrg[3]; normvec(plusvec(copyvec(nrg,org,3),neg,3),3);
	float nur[3]; normvec(plusvec(copyvec(nur,cur,3),neg,3),3);
	float xss[3]; crossvec(nrg,nur);
	float ang = asinf(sqrtf(dotvec(xss,xss,3)));
	float axs[3]; normvec(xss,3);
	return planeRotate(mat,axs,ang);
}
float *planeRotateNormalMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// perpendicular to normal, parallel to picture, fixed; cursor mapped
	float neg[3]; scalevec(copyvec(neg,fix,3),-1.0,3);
	float nnm[3]; normvec(copyvec(nnm,nrm,3),3);
	float nrg[3]; normvec(plusvec(copyvec(nrg,org,3),neg,3),3);
	float nur[3]; normvec(plusvec(copyvec(nur,cur,3),neg,3),3);
	float xrg[3]; crossvec(nnm,nrg);
	float xur[3]; crossvec(nnm,nur);
	float ang = asinf(sqrtf(dotvec(xur,xur,3)))-asinf(sqrtf(dotvec(xrg,xrg,3)));
	float axs[3]; normvec(xur,3);
	return planeRotate(mat,axs,ang);
}
float *planeScaleOrthoMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to ortho fixed; cursor mapped
	return mat;
}
float *planeScaleFocalMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to cursor fixed; cursor mapped
	return mat;
}
float *planeScaleNormalMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to normal fixed; cursor mapped
	return mat;
}
float *planeSlideOrthoRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to ortho offset
	return mat;
}
float *planeSlideFocalRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to cursor offset
	return mat;
}
float *planeSlideNormalRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to normal offset
	return mat;
}
float *planeRotateOrthoRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to ortho fixed
	return mat;
}
float *planeRotateFocalRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to cursor fixed
	return mat;
}
float *planeRotateNormalRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to normal fixed
	return mat;
}
float *planeScaleOrthoRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to normal scaled
	return mat;
}
float *planeScaleFocalRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to cursor scaled
	return mat;
}
float *planeScaleNormalRoller(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// TODO distance to perpendicular to normal scaled
	return mat;
}
float *planeCenter()
{
	int index = configure[RegisterIndex] - center.idx;
	if (index < 0 || index >= center.siz) return 0;
	if (center.mem != (enum Memory)configure[RegisterMemory]) return 0;
	switch(center.mem) {
	case (Allmatz): return center.all[index].mat;
	case (Fewmatz): return center.few[index].mat;
	case (Onematz): return center.one[index].mat;
	default: break;}
	return 0;
}
struct Kernel *planeKernel()
{
	int index = configure[RegisterIndex];
	switch ((enum Memory)configure[RegisterMemory]) {
	case (Allmatz): return subject + index % configure[SubjectSize];
	case (Fewmatz): return object + index % configure[ObjectSize];
	case (Onematz): return element + index % configure[ElementSize];
	default: break;}
	return 0;
}
float *planeInverse()
{
	return planeKernel()->inverse.mat;
}
float *planeMaintain()
{
	return planeKernel()->maintain.mat;
}
float *planeWritten()
{
	return planeKernel()->written.mat;
}
float *planeTowrite()
{
	return planeKernel()->towrite.mat;
}
float *planeCompose()
{
	// TODO check if can use planeKernel()->compose as is
	return jumpmat(jumpmat(copymat(planeKernel()->compose.mat,planeMaintain(),4),planeWritten(),4),planeTowrite(),4);
}
planeXform planeFunc()
{
	switch ((enum Tool)configure[RegisterTool]) {
	case (Mouse): switch ((enum Trans)configure[RegisterTransM]) {
	case (Slide): switch ((enum Form)configure[RegisterFormM]) {
	case (Ortho): return planeSlideOrthoMouse;
	case (Focal): return planeSlideFocalMouse;
	case (Normal): return planeSlideNormalMouse;
	default: ERROR();}
	case (Rotate): switch ((enum Form)configure[RegisterFormM]) {
	case (Ortho): return planeRotateOrthoMouse;
	case (Focal): return planeRotateFocalMouse;
	case (Normal): return planeRotateNormalMouse;
	default: ERROR();}
	case (Scale): switch ((enum Form)configure[RegisterFormM]) {
	case (Ortho): return planeScaleOrthoMouse;
	case (Focal): return planeScaleFocalMouse;
	case (Normal): return planeScaleNormalMouse;
	default: ERROR();}
	default: ERROR();}
	case (Roller): switch ((enum Trans)configure[RegisterTransR]) {
	case (Slide): switch ((enum Form)configure[RegisterFormR]) {
	case (Ortho): return planeSlideOrthoRoller;
	case (Focal): return planeSlideFocalRoller;
	case (Normal): return planeSlideNormalRoller;
	default: ERROR();}
	case (Rotate): switch ((enum Form)configure[RegisterFormR]) {
	case (Ortho): return planeRotateOrthoRoller;
	case (Focal): return planeRotateFocalRoller;
	case (Normal): return planeRotateNormalRoller;
	default: ERROR();}
	case (Scale): switch ((enum Form)configure[RegisterFormR]) {
	case (Ortho): return planeScaleOrthoRoller;
	case (Focal): return planeScaleFocalRoller;
	case (Normal): return planeScaleNormalRoller;
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
	for (int i = 0; i < configure[PierceSize]; i++) {
	struct Pierce *temp = pierce + i%configure[PierceSize];
	if (!found || !found->vld || (temp->vld && temp->fix[2] < found->fix[2])) found = temp;}
	if (!found) found = &unfound; // TODO set unfound to picture plane
	return found;
}
void planeStage(enum Configure cfg)
{
	switch (cfg) {
	case (RegisterDone): configure[RegisterDone] = callInfo(RegisterDone); break;
	case (RegisterOpen): configure[RegisterOpen] = planeRunning(); break;
	case (CenterRequest): configure[CenterRequest] = center.req; break;
	case (CenterMemory): configure[CenterMemory] = center.mem; break;
	case (CenterSize): configure[CenterSize] = center.siz; break;
	case (CenterIndex): configure[CenterIndex] = center.idx; break;
	case (CenterSelf): configure[CenterSelf] = center.slf; break;
	case (ClosestFound): configure[ClosestFound] = planePierce()->idx; break;
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
void *planeRealloc(void *ptr, int siz, int tmp, int mod)
{
	char *result = realloc(ptr,siz*mod);
	for (int i = tmp*mod; i < siz*mod; i++) result[i] = 0;
	return result;
}
void planeConfig(enum Configure cfg, int val)
{

	int tmp = configure[cfg];
	configure[cfg] = val;
	switch (cfg) {
	case (PierceSize): pierce = planeRealloc(pierce,val,tmp,sizeof(struct Pierce)); break;
	case (SubjectSize): subject = planeRealloc(subject,val,tmp,sizeof(struct Kernel)); break;
	case (ObjectSize): object = planeRealloc(object,val,tmp,sizeof(struct Kernel)); break;
	case (ElementSize): element = planeRealloc(element,val,tmp,sizeof(struct Kernel)); break;
	case (MachineSize): machine = planeRealloc(machine,val,tmp,sizeof(struct Machine)); break;
	case (FloatSize): floats = planeRealloc(floats,val,tmp,sizeof(float)); break;
	case (RegisterResponse): sem_safe(&resource,{rspidx = val;});
	case (RegisterOpen): planeStarted(val); break;
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
	case (Piercez): for (int i = 0; i < ptr->siz; i++) copyPierce(&pierce[(ptr->idx+i)%configure[PierceSize]],&ptr->pie[i]); break;
	case (Stringz): for (int i = 0; i < ptr->siz; i++) if (ptr->idx < 0) ptr->idx = planeSet(-1,ptr->str[i]); else planeSet(ptr->idx+i,ptr->str[i]); break;
	case (Machinez): for (int i = 0; i < ptr->siz; i++) copyMachine(&machine[(ptr->idx+i)%configure[MachineSize]],&ptr->mch[i]); break;
	case (Floatz): for (int i = 0; i < ptr->siz; i++) floats[(ptr->idx+i)%configure[FloatSize]] = ptr->flt[i]; break;
	case (Configurez): for (int i = 0; i < ptr->siz; i++) planeConfig(ptr->cfg[i],ptr->val[i]); callDma(ptr); break;
	default: callDma(ptr); break;}
}
int planeEscape(int lvl, int nxt)
{
	int level = configure[RegisterNest];
	int inc = (lvl > 0 ? 1 : -1); lvl *= inc;
	while (lvl > 0 && (nxt += inc) < configure[MachineSize]) if (machine[nxt].xfr == Nest) {
	lvl += machine[nxt].lvl*inc; configure[RegisterNest] += machine[nxt].lvl*inc;}
	return nxt;
}
int planeIval(struct Express *exp)
{
	void *dat = 0;
	int typ = datxEval(&dat,exp,identType("Int"));
	if (typ != identType("Int")) ERROR();
	return *datxIntz(0,dat);
}
int planeSwitch(struct Machine *mptr, int next)
{
	switch (mptr->xfr) {
	case (Read): planeRead(); break;
	case (Write): writeCenter(&center,external); break;
	case (Stage): for (int i = 0; i < mptr->siz; i++)
	planeStage(mptr->sav[i]); break;
	case (Force): for (int i = 0; i < mptr->siz; i++) {
	planeDma(mptr->cfg[i],mptr->val[i]); planeConfig(mptr->cfg[i],mptr->val[i]);} break;
	case (Comp): jumpmat(copymat(planeCenter(),planeCompose(),4),planeLocal(),4); break;
	case (Pose): copymat(planeCenter(),planeTowrite(),4); break;
	case (Other): copymat(planeCenter(),planeMaintain(),4); break;
	case (Glitch): copymat(planeMaintain(),planeCenter(),4); break;
	case (Check): jumpmat(planeMaintain(),planeCenter(),4);
	timesmat(planeWritten(),invmat(copymat(planeInverse(),planeCenter(),4),4),4); break;
	case (Local): jumpmat(planeTowrite(),planeLocal(),4);
	planeStage(OriginLeft); planeStage(OriginBase); planeStage(OriginAngle); break;
	case (Apply): jumpmat(planeWritten(),planeTowrite(),4);
	identmat(planeTowrite(),4); break;
	case (Accum): jumpmat(planeMaintain(),planeWritten(),4);
	identmat(planeWritten(),4); break;
	case (Proj): planeProject(planeCenter()); break;
	case (Copy): planeCopy(&center); break;
	case (Draw): callDraw(configure[ArgumentMicro],configure[ArgumentStart],configure[ArgumentStop]); break;
	case (Jump): {int tmp = planeIval(&mptr->loc[0]) ? mptr->nxt : configure[RegisterNest];
	next = planeEscape(tmp,next);} break;
	case (Goto): next = (planeIval(&mptr->loc[0]) ? mptr->nxt : next); break;
	case (Nest): configure[RegisterNest] += mptr->lvl; break;
	case (Aval): {void *dat = 0;
	datxStr(&dat,mptr->avl); datxNone(dat0); writeCenter(&center,idx0);
	datxInsert(dat,*dat0,identType("Center"));} break;
	case (Bval): for (int i = 0; i < mptr->siz; i++)
	planeCopy(&mptr->bvl[i]); break;
	case (Cval): for (int i = 0; i < mptr->siz; i++) {
	struct Center tmp = {0}; datxEval(dat0,&mptr->cvl[i],identType("Center"));
	readCenter(&tmp,idx0); planeCopy(&tmp);} break;
	case (Dval): datxNone(dat0); writeCenter(mptr->dvl,idx0); readCenter(&center,idx0); break;
	case (Eval): datxEval(dat0,mptr->evl,identType("Center")); readCenter(&center,idx0); break;
	default: break;}
	return next;
}
void planeWake(enum Configure hint)
{
	configure[RegisterHint] = hint;
	if (configure[RegisterLine] < 0 || configure[RegisterLine] >= configure[MachineSize]) configure[RegisterLine] = 0;
	while (configure[RegisterLine] >= 0 && configure[RegisterLine] < configure[MachineSize]) {
	struct Machine *mptr = machine+configure[RegisterLine];
	int next = planeSwitch(mptr,configure[RegisterLine]+1);
	if (next == configure[RegisterLine]) {configure[RegisterLine] += 1; break;}
	configure[RegisterLine] = next;}
}
void planeBoot()
{
	for (int i = 0; Bootstrap__Int__Str(i); i++) {
	struct Machine mptr = {0};
	int len = 0;
	if (!hideMachine(&mptr,Bootstrap__Int__Str(i),&len)) ERROR();
	configure[RegisterLine] = planeSwitch(&mptr,configure[RegisterLine]);
	}
}
void planeRead()
{
	int num = 0; sem_safe(&resource,{if ((num = numpipe)) numpipe--;});
	if (num) readCenter(&center,internal);
	else {struct Center tmp = {0}; center = tmp;}
}
const char *planeGet(int idx)
{
	const char *ret = 0;
	sem_wait(&resource);
	free(pthread_getspecific(retstr)); pthread_setspecific(retstr,0);
	if (idx >= numstr) {
	strings = realloc(strings,(idx+1)*sizeof(char*));
	while (idx >= numstr) strings[numstr++] = strdup("");}
	if (idx < 0) {
	numstr--;
	pthread_setspecific(retstr,strdup(strings[numstr]));
	free(strings[numstr]); strings[numstr] = 0;
	strings = realloc(strings,numstr*sizeof(char*));}
	else pthread_setspecific(retstr,strdup(strings[idx]));
	ret = pthread_getspecific(retstr);
	sem_post(&resource);
	return ret;
}
int planeSet(int idx, const char *str)
{
	int ret = 0;
	sem_wait(&resource);
	if (idx >= numstr) {
	strings = realloc(strings,(idx+1)*sizeof(char*));
	while (idx >= numstr) strings[numstr++] = strdup("");}
	if (idx < 0) {
	numstr++;
	strings = realloc(strings,numstr*sizeof(char*));
	idx = numstr-1;
	strings[numstr-1] = strdup("");}
	free(strings[idx]); strings[idx] = strdup(str);
	if (idx == rspidx && strings[idx][strlen(strings[idx])-1] == '\n') {
	char *ptr = strings[idx];
	while(strchr(ptr,'\n') != strings[idx]+strlen(strings[idx])-1) ptr = strchr(ptr,'\n')+1;
	write(STDOUT_FILENO,ptr,strlen(ptr));}
	ret = numstr;
	sem_post(&resource);
	return ret;
}
int planeCat(int idx, const char *str)
{
	const char *src = planeGet(idx);
	char *dst = malloc(strlen(src)+strlen(str)+1);
	int ret = planeSet(idx,strcat(strcpy(dst,src),str));
	free(dst);
	return ret;
}
void planeSetter(void *dat, int mem, int sub)
{
	switch ((enum Etter)mem) {
	case (Configurey):
	if (sub < 0 || sub >= Configures) ERROR();
	planeDma(sub,*datxIntz(0,dat));
	planeConfig(sub,*datxIntz(0,dat));
	break; case (Stringy):
	planeSet(sub,datxChrz(0,dat));
	break; case (Floaty):
	if (sub < 0 || sub >= configure[FloatSize]) ERROR();
	floats[sub] = *datxOldz(0,dat);
	break; default: ERROR();}
}
void planeGetter(void **dat, int mem, int sub)
{
	switch ((enum Etter)mem) {
	case (Configurey):
	if (sub < 0 || sub >= Configures) ERROR();
	datxInt(dat,configure[sub]);
	break; case (Stringy):
	datxStr(dat,planeGet(sub));
	break; case (Floaty):
	if (sub < 0 || sub >= configure[FloatSize]) ERROR();
	datxOld(dat,floats[sub]);
	break; default: ERROR();}
}
void planeFind(char **val, const char *key)
{
	void *src = 0; void *dst = 0; int typ = 0;
	datxStr(&src,key);
	typ = datxFind(&dst,src);
	if (typ != identType("Str")) ERROR();
	assignStr(val,datxChrz(0,dst));
	free(src); free(dst);
}
void planeInsert(const char *key, const char *val)
{
	void *src = 0; void *dst = 0;
	datxStr(&src,key);
	datxStr(&dst,val);
	datxInsert(src,dst,identType("Str"));
	free(src); free(dst);
}
void planeTerm(int sig)
{
}
void *planeExternal(void *ptr)
{
	struct Argument arg = {0};
	if ((external = wrapIdent(Planez,planeGet(1))) < 0) exitErr(__FILE__,__LINE__);
	sem_post(&ready[External]);
	while (1) {
	struct Center center = {0};
	int sub = waitRead(0,1<<external);
	if (sub != external) break;
	if (!checkRead(external)) break;
	if (!checkWrite(internal)) break;
	readCenter(&center,external);
	writeCenter(&center,internal);
	sem_safe(&resource,{numpipe++;});
	planeSafe(Procs,Waits,CenterRequest);}
	planeSafe(External,Done,Configures);
	return 0;
}
void *planeConsole(void *ptr)
{
	char chr[2] = {0};
	int val = 0;
	int nfd = 0;
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
	sem_safe(&resource,{val = configure[RegisterPrompt];});
	planeCat(val,chr);
	planeSafe(Procs,Waits,RegisterPrompt);}
	planeSafe(Console,Done,Configures);
	return 0;
}
void planeThread(enum Proc bit)
{
	switch (bit) {
	case (External): if (pthread_create(&thread[bit],0,planeExternal,0) != 0) ERROR(); break;
	case (Console): if (pthread_create(&thread[bit],0,planeConsole,0) != 0) ERROR(); break;
	case (Window): planeSafe(Window,Start,Configures); sem_post(&ready[bit]); break;
	case (Graphics): planeSafe(Graphics,Start,Configures); sem_post(&ready[bit]); break;
	case (Process): planeSafe(Process,Start,Configures); sem_post(&ready[bit]); break;
	default: ERROR();}
}
void planeFinish(enum Proc bit)
{
	switch (bit) {
	case (External): sem_wait(&ready[bit]); closeIdent(external); if (pthread_join(thread[bit],0) != 0) ERROR(); break;
	case (Console): sem_wait(&ready[bit]); close(STDIN_FILENO); if (pthread_join(thread[bit],0) != 0) ERROR(); break;
	case (Window): sem_wait(&ready[bit]); planeSafe(Window,Stop,Configures); break;
	case (Graphics): sem_wait(&ready[bit]); planeSafe(Graphics,Stop,Configures); break;
	case (Process): sem_wait(&ready[bit]); planeSafe(Process,Stop,Configures); break;
	default: ERROR();}
}
void planeStarted(int val)
{
	int done = 0; int todo = 0;
	done = started & ~val; todo = val & ~started;
	for (enum Proc bit = 0; bit < Procs; bit++) if (done & (1<<bit)) planeFinish(bit);
	for (enum Proc bit = 0; bit < Procs; bit++) if (todo & (1<<bit)) planeThread(bit);
	started = val;
}
int planeRunning()
{
	return running;
}
void planeInit(zftype init, uftype dma, vftype safe, yftype main, xftype info, wftype draw)
{
	struct sigaction act;
	act.__sigaction_u.__sa_handler = planeTerm;
	if (sigaction(SIGTERM,&act,0) < 0) ERROR();
	if (pthread_key_create(&retstr,free) != 0) ERROR();
	sem_init(&resource,0,1); sem_init(&pending,0,0);
	for (enum Proc bit = 0; bit < Procs; bit++) sem_init(&ready[bit],0,0);
	datxSetter(planeSetter); datxGetter(planeGetter);
	sub0 = datxSub(); idx0 = puntInit(sub0,sub0,datxReadFp,datxWriteFp); dat0 = datxDat(sub0);
	luaxAdd("planeGet",protoTypeRj(planeGet)); luaxAdd("planeSet",protoTypeFh(planeSet)); luaxAdd("planeCat",protoTypeFh(planeCat));
	luaxAdd("planeFind",protoTypeRm(planeFind)); luaxAdd("planeInsert",protoTypeRn(planeInsert));
	datxEmbed(luaxSide);
	callDma = dma;
	callSafe = safe;
	callMain = main;
	callInfo = info;
	callDraw = draw;
	if ((internal = openPipe()) < 0) ERROR();
	init(); planeBoot();
	while (1) {
	enum Wait wait = 0;
	enum Configure hint = 0;
	sem_safe(&resource,{if (!qfull && !started) break;});
	planeMain();}
	closeIdent(internal);
}
int planeInfo(enum Configure cfg)
{
	return configure[cfg];
}
void planeDebug(const char *str, enum Proc proc, enum Wait wait, enum Configure hint, int run)
{
	char *ptr = 0; char *wtr = 0; char *htr = 0;
	showProc(proc,&ptr); showWait(wait,&wtr); showConfigure(hint,&htr);
	printf("%s %s %s %s %d\n",str,ptr,wtr,htr,run);
	free(ptr); free(wtr); free(htr);
}
int planeEnque(enum Proc proc, enum Wait wait, enum Configure hint)
{
	int run = 0;
	sem_wait(&resource);
	run = calling;
	if (proc == Process && wait == Start) calling++;
	if (proc == Process && wait == Stop) calling--;
	if (qfull == qsize) {qsize++;
	procs = realloc(procs,qsize*sizeof(enum Proc));
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
void planeDeque(enum Proc *proc, enum Wait *wait, enum Configure *hint)
{
	int idle = 0;
	sem_wait(&pending);
	sem_wait(&resource);
	if (qfull == 0) ERROR();
	*proc = procs[qhead]; *wait = waits[qhead]; *hint = hints[qhead];
	qhead++; if (qhead == qsize) qhead = 0;
	qfull--;
	if (qfull > 0) sem_post(&pending)
	else if (*hint != RegisterHint) idle = 1;
	sem_post(&resource);
	if (idle) {
	planeSafe(Procs,Waits,RegisterHint);
	}
}
void planeSafe(enum Proc proc, enum Wait wait, enum Configure hint)
{
	if (planeEnque(proc,wait,hint)) callSafe();
}
void planeMain()
{
	enum Proc proc = 0; enum Wait wait = 0; enum Configure hint = 0;
	planeDeque(&proc,&wait,&hint);
	if (wait != Waits && hint != Configures) ERROR();
	if (wait == Waits && hint == Configures) ERROR();
	if (wait == Waits && hint != Configures) planeWake(hint);
	if (wait != Waits && wait != Done && hint == Configures) callMain(proc,wait);
	if (wait == Done && hint == Configures) {running &= ~(1<<proc);
	if ((started & ~running) != 0) planeSafe(Procs,Waits,RegisterOpen);}
}
void planeReady(struct Pierce *given)
{
	for (int i = 0; i < configure[PierceSize]; i++) pierce[i] = given[i]; found = 0;
}
