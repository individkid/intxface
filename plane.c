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
#define sem_sync(S) {sem_wait(S);sem_post(S);}
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
int configure[Configures] = {0};
struct Center center = {0};
int sub0 = 0;
int idx0 = 0;
void **dat0 = 0;
// constant after other threads start:
int internal = 0;
int external = 0;
int started = 0;
uftype callDma = 0;
vftype callSafe = 0;
yftype callMain = 0;
xftype callInfo = 0;
wftype callDraw = 0;
pthread_t thread[Threads];
pthread_key_t retstr;
// resource protected:
int numpipe = 0;
char **strings = 0;
int numstr = 0;
int running = 0;
int qsize = 0;
int qfull = 0;
int qhead = 0;
int qtail = 0;
enum Configure *hints = 0;
enum Wait *waits = 0;
int perpend[Waits] = {0};
// thread safe:
sem_t resource;
sem_t pending;
sem_t ready[Threads];
void planeRead();
const char *planeGet(int idx);
int planeSet(int idx, const char *str);
int planeCat(int idx, const char *str);
int planeRunning();
void planeStarted(int val);

void planeAlize(float *dir, const float *vec)
{
}
void planeCross(float *axe, const float *fix, const float *cur)
{
}
typedef float *(*planeXform)(float *mat, const float *pic, const float *fix, const float *org, const float *cur);
// mat:current-matrix pic:focal-point fix:pierce-point org:pierce-cursor cur:current-cursor ang:roller-change
float *planeSlideOrthoMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to ortho fixed; cursor mapped
	return 0;
}
float *planeSlideFocalMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to cursor fixed; cursor mapped
	return 0;
}
float *planeSlideNormalMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to normal fixed; cursor mapped
	return 0;
}
float *planeRotateOrthoMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO perpendicular to ortho, parallel to picture, fixed; cursor mapped
	return 0;
}
float *planeRotateFocalMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO perpendicular to cursor, parallel to picture, fixed; cursor mapped
	return 0;
}
float *planeRotateNormalMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO perpendicular to normal, parallel to picture, fixed; cursor mapped
	return 0;
}
float *planeScaleOrthoMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to ortho fixed; cursor mapped
	return 0;
}
float *planeScaleFocalMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to cursor fixed; cursor mapped
	return 0;
}
float *planeScaleNormalMouse(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to normal fixed; cursor mapped
	return 0;
}
float *planeSlideOrthoRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to ortho offset
	return 0;
}
float *planeSlideFocalRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to cursor offset
	return 0;
}
float *planeSlideNormalRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to normal offset
	return 0;
}
float *planeRotateOrthoRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to ortho fixed
	return 0;
}
float *planeRotateFocalRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to cursor fixed
	return 0;
}
float *planeRotateNormalRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to normal fixed
	return 0;
}
float *planeScaleOrthoRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to normal scaled
	return 0;
}
float *planeScaleFocalRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to cursor scaled
	return 0;
}
float *planeScaleNormalRoller(float *mat, const float *pic, const float *fix, const float *org, const float *cur)
{
	// TODO distance to perpendicular to normal scaled
	return 0;
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
	case (Ortho): return planeSlideOrthoRoller;
	case (Focal): return planeRotateFocalRoller;
	case (Normal): return planeRotateNormalRoller;
	default: ERROR();}
	case (Scale): switch ((enum Form)configure[RegisterFormR]) {
	case (Ortho): return planeSlideOrthoRoller;
	case (Focal): return planeScaleFocalRoller;
	case (Normal): return planeScaleNormalRoller;
	default: ERROR();}
	default: ERROR();}
	default: ERROR();}
	return 0;
}
float *planeLocal()
{
	float pic[2]; float fix[2]; float org[3]; float cur[3];
	pic[0] = configure[WindowLeft];
	pic[1] = configure[WindowBase];
	fix[0] = configure[ClosestLeft];
	fix[1] = configure[ClosestBase];
	org[0] = configure[OriginLeft];
	org[1] = configure[OriginBase];
	org[2] = configure[OriginAngle];
	cur[0] = configure[CursorLeft];
	cur[1] = configure[CursorBase];
	cur[2] = configure[CursorAngle];
	return planeFunc()(planeKernel()->local.mat,pic,fix,org,cur);
}
struct Pierce *planePierce()
{
	if (found) return found;
	for (int i = 0; i < configure[PierceSize]; i++) {
	struct Pierce *temp = pierce + i%configure[PierceSize];
	if (!found || !found->vld || (temp->vld && temp->fix[2] < found->fix[2])) found = temp;}
	if (!found) {
	found = &unfound;
	unfound.fix[0] = configure[ClosestLeft];
	unfound.fix[1] = configure[ClosestBase];
	unfound.fix[2] = configure[ClosestNear];
	unfound.idx = configure[ClosestFound];}
	return found;
}
void planeStage(const enum Configure *cfg, int siz)
{
	for (int i = 0; i < siz; i++) switch (cfg[i]) {
	case (RegisterDone): configure[RegisterDone] = callInfo(RegisterDone); break;
	case (RegisterOpen): configure[RegisterOpen] = planeRunning(); break;
	case (CenterRequest): configure[CenterRequest] = center.req; break;
	case (CenterMemory): configure[CenterMemory] = center.mem; break;
	case (CenterSize): configure[CenterSize] = center.siz; break;
	case (CenterIndex): configure[CenterIndex] = center.idx; break;
	case (CenterSelf): configure[CenterSelf] = center.slf; break;
	case (ClosestLeft): configure[ClosestLeft] = planePierce()->fix[0]; break;
	case (ClosestBase): configure[ClosestBase] = planePierce()->fix[1]; break;
	case (ClosestNear): configure[ClosestNear] = planePierce()->fix[2]; break;
	case (ClosestFound): configure[ClosestFound] = planePierce()->idx; break;
	case (WindowLeft): configure[WindowLeft] = callInfo(WindowLeft); break;
	case (WindowBase): configure[WindowBase] = callInfo(WindowBase); break;
	case (WindowWide): configure[WindowWide] = callInfo(WindowWide); break;
	case (WindowHigh): configure[WindowHigh] = callInfo(WindowHigh); break;
	case (CursorLeft): configure[CursorLeft] = callInfo(CursorLeft); break;
	case (CursorBase): configure[CursorBase] = callInfo(CursorBase); break;
	case (CursorAngle): configure[CursorAngle] +=/*accumulate*/ callInfo(CursorAngle); break;
	case (CursorClick): configure[CursorClick] = callInfo(CursorClick); break;
	case (OriginLeft): configure[OriginLeft] = configure[CursorLeft]; break;
	case (OriginBase): configure[OriginBase] = configure[CursorBase]; break;
	case (OriginAngle): configure[OriginAngle] = configure[CursorAngle]; configure[CursorAngle] = 0; break;
	default: break;}
}
void *planeRealloc(void *ptr, int siz, int tmp, int mod)
{
	char *result = realloc(ptr,siz*mod);
	for (int i = tmp*mod; i < siz*mod; i++) result[i] = 0;
	return result;
}
void planeThrough(struct Center *ptr)
{
	if (ptr->mem != Configurez) ERROR();
	for (int i = 0; i < ptr->siz; i++) {
	int tmp = configure[ptr->cfg[i]];
	configure[ptr->cfg[i]] = ptr->val[i];
	switch (ptr->cfg[i]) {
	case (PierceSize): pierce = planeRealloc(pierce,ptr->val[i],tmp,sizeof(struct Pierce)); break;
	case (SubjectSize): subject = planeRealloc(subject,ptr->val[i],tmp,sizeof(struct Kernel)); break;
	case (ObjectSize): object = planeRealloc(object,ptr->val[i],tmp,sizeof(struct Kernel)); break;
	case (ElementSize): element = planeRealloc(element,ptr->val[i],tmp,sizeof(struct Kernel)); break;
	case (MachineSize): machine = planeRealloc(machine,ptr->val[i],tmp,sizeof(struct Machine)); break;
	case (RegisterOpen): planeStarted(ptr->val[i]); break;
	default: break;}}
	callDma(ptr);
}
void planeForce(enum Configure *cfg, int *val, int siz)
{
	struct Center tmp = {0};
	tmp.mem = Configurez;
	tmp.siz = siz;
	allocConfigure(&tmp.cfg,siz);
	allocInt(&tmp.val,siz);
	for (int i = 0; i < siz; i++) {
	tmp.cfg[i] = cfg[i];
	tmp.val[i] = val[i];}
	planeThrough(&tmp);
	freeCenter(&tmp);
}
void planeAlloc()
{
	freeCenter(&center);
	center.req = (enum Request)configure[CenterRequest];
	center.mem = (enum Memory)configure[CenterMemory];
	center.siz = configure[CenterSize];
	center.idx = configure[CenterIndex];
	center.slf = configure[CenterSelf];
	switch (center.mem) {
	case (Trianglez): allocTriangle(&center.tri,center.siz); break;
	case (Numericz): allocNumeric(&center.num,center.siz); break;
	case (Vertexz): allocVertex(&center.vtx,center.siz); break;
	case (Allmatz): allocMatrix(&center.all,center.siz); break;
	case (Fewmatz): allocMatrix(&center.few,center.siz); break;
	case (Onematz): allocMatrix(&center.one,center.siz); break;
	case (Swarmz): allocVector(&center.swa,center.siz); break;
	case (Texturez): allocVector(&center.tex,center.siz); break;
	case (Basisz): allocBasis(&center.bas,center.siz); break;
	case (Piercez): allocPierce(&center.pie,center.siz); break;
	case (Slicez): allocSlice(&center.rng,center.siz); break;
	case (Stringz): allocStr(&center.str,center.siz); break;
	case (Machinez): allocMachine(&center.mch,center.siz); break;
	case (Configurez): allocConfigure(&center.cfg,center.siz); allocInt(&center.val,center.siz); break;
	default: center.siz = 0; break;}
}
void planeMachine(struct Machine *dst, struct Machine *src)
{
	datxNone(dat0);
	writeMachine(src,idx0);
	readMachine(dst,idx0);
}
void planeBuffer()
{
	switch (center.mem) {
	case (Piercez): for (int i = 0; i < center.siz; i++) memcpy(&pierce[(center.idx+i)%configure[PierceSize]],&center.pie[i],sizeof(struct Pierce)); break;
	case (Stringz): if (center.idx < 0) for (int i = 0; i < center.siz; i++) center.idx = planeSet(-1,center.str[i]);
	else for (int i = 0; i < center.siz; i++) planeSet(center.idx+i,center.str[i]); break;
	case (Machinez): for (int i = 0; i < center.siz; i++) planeMachine(&machine[(center.idx+i)%configure[MachineSize]],&center.mch[i]); break;
	case (Configurez): planeThrough(&center); break;
	default: callDma(&center); break;}
}
int planeEscape(int lvl, int nxt)
{
	int level = configure[RegisterNest];
	int inc = (lvl > 0 ? 1 : -1); lvl *= inc;
	while (lvl > 0 && (nxt += inc) < configure[MachineSize]) if (machine[nxt].xfr == Nest) {
	lvl += machine[nxt].idx*inc; configure[RegisterNest] += machine[nxt].idx*inc;}
	return nxt;
}
void planeGval(struct Generic *gen, int idx)
{
	datxNone(dat0); writeUnion(gen,idx0);
	if (idx < 0 || idx >= center.siz) ERROR();
	switch (center.mem) {
	case (Trianglez): if (identUnion(gen) != identType("Triangle")) ERROR(); readTriangle(&center.tri[idx],idx0); break;
	case (Numericz): if (identUnion(gen) != identType("Numeric")) ERROR(); readNumeric(&center.num[idx],idx0); break;
	case (Vertexz): if (identUnion(gen) != identType("Vertex")) ERROR(); readVertex(&center.vtx[idx],idx0); break;
	case (Allmatz): if (identUnion(gen) != identType("Matrix")) ERROR(); readMatrix(&center.all[idx],idx0); break;
	case (Fewmatz): if (identUnion(gen) != identType("Matrix")) ERROR(); readMatrix(&center.few[idx],idx0); break;
	case (Onematz): if (identUnion(gen) != identType("Matrix")) ERROR(); readMatrix(&center.one[idx],idx0); break;
	case (Swarmz): if (identUnion(gen) != identType("Vector")) ERROR(); readVector(&center.swa[idx],idx0); break;
	case (Texturez): if (identUnion(gen) != identType("Vector")) ERROR(); readVector(&center.tex[idx],idx0); break;
	case (Basisz): if (identUnion(gen) != identType("Basis")) ERROR(); readBasis(&center.bas[idx],idx0); break;
	case (Piercez): if (identUnion(gen) != identType("Pierce")) ERROR(); readPierce(&center.pie[idx],idx0); break;
	case (Slicez): if (identUnion(gen) != identType("Slice")) ERROR(); readSlice(&center.rng[idx],idx0); break;
	case (Stringz): if (identUnion(gen) != identType("Str")) ERROR(); readStr(&center.str[idx],idx0); break;
	case (Machinez): if (identUnion(gen) != identType("Machine")) ERROR(); readMachine(&center.mch[idx],idx0); break;
	case (Configurez): if (identUnion(gen) != identType("Int")) ERROR(); center.cfg[idx] = readInt(idx0); break;
	default: ERROR();}
}
void planeEval(struct Express *exp, struct Express *sub)
{
	void *dat = 0; int idx = 0;
	datxEval(&dat,sub,identType("Int")); idx = *datxIntz(0,dat); datxStr(&dat,""); datxNone(dat0);
	if (idx < 0 || idx >= center.siz) ERROR();
	switch (center.mem) {
	case (Trianglez): writeTriangle(&center.tri[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Triangle")); readTriangle(&center.tri[idx],idx0); break;
	case (Numericz): writeNumeric(&center.num[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Numeric")); readNumeric(&center.num[idx],idx0); break;
	case (Vertexz): writeVertex(&center.vtx[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Vertex")); readVertex(&center.vtx[idx],idx0); break;
	case (Allmatz): writeMatrix(&center.all[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Matrix")); readMatrix(&center.all[idx],idx0); break;
	case (Fewmatz): writeMatrix(&center.few[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Matrix")); readMatrix(&center.few[idx],idx0); break;
	case (Onematz): writeMatrix(&center.one[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Matrix")); readMatrix(&center.one[idx],idx0); break;
	case (Swarmz): writeVector(&center.swa[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Vector")); readVector(&center.swa[idx],idx0); break;
	case (Texturez): writeVector(&center.tex[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Vector")); readVector(&center.tex[idx],idx0); break;
	case (Basisz): writeBasis(&center.bas[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Basis")); readBasis(&center.bas[idx],idx0); break;
	case (Piercez): writePierce(&center.pie[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Pierce")); readPierce(&center.pie[idx],idx0); break;
	case (Slicez): writeSlice(&center.rng[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Slice")); readSlice(&center.rng[idx],idx0); break;
	case (Stringz): writeStr(center.str[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Str")); readStr(&center.str[idx],idx0); break;
	case (Machinez): writeMachine(&center.mch[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Machine")); readMachine(&center.mch[idx],idx0); break;
	case (Configurez): writeInt(center.cfg[idx],idx0); datxInsert(dat,dat0); datxEval(dat0,exp,identType("Int")); center.cfg[idx] = readInt(idx0); break;
	default: ERROR();}
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
	case (Save): planeStage(mptr->sav,mptr->siz); break;
	case (Force): planeForce(mptr->cfg,mptr->val,mptr->siz); break;
	case (Alloc): planeAlloc(); break;
	case (Comp): jumpmat(copymat(planeCenter(),planeCompose(),4),planeLocal(),4); break;
	case (Pose): copymat(planeCenter(),planeTowrite(),4); break;
	case (Other): copymat(planeCenter(),planeMaintain(),4); break;
	case (Glitch): copymat(planeMaintain(),planeCenter(),4); break;
	case (Check): jumpmat(planeMaintain(),planeCenter(),4); timesmat(planeWritten(),invmat(copymat(planeInverse(),planeCenter(),4),4),4); break;
	case (Local): jumpmat(planeTowrite(),planeLocal(),4); planeStage((const enum Configure []){OriginLeft,OriginBase,OriginAngle},3); break;
	case (Apply): jumpmat(planeWritten(),planeTowrite(),4); identmat(planeTowrite(),4); break;
	case (Accum): jumpmat(planeMaintain(),planeWritten(),4); identmat(planeWritten(),4); break;
	case (Share): planeBuffer(); break;
	case (Draw): callDraw((enum Micro)configure[ArgumentMicro],configure[ArgumentStart],configure[ArgumentStop]); break;
	case (Jump): next = planeEscape((planeIval(&mptr->loc[0]) ? mptr->idx : configure[RegisterNest]),next); break;
	case (Goto): next = (planeIval(&mptr->loc[0]) ? mptr->idx : next); break;
	case (Nest): configure[RegisterNest] += mptr->idx; break;
	case (Eval): for (int i = 0; i < mptr->siz; i++) planeEval(&mptr->exp[0],&mptr->var[0]); break;
	case (Gval): for (int i = 0; i < mptr->siz; i++) planeGval(&mptr->gen[i],mptr->num[i]); break;
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
	configure[RegisterLine] = planeSwitch(&mptr,configure[RegisterLine]);}
}
void planeRead()
{
	int num = 0;
	sem_safe(&resource,{if ((num = numpipe)) numpipe--;});
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
void planeSetter(void *dat, int sub)
{
	if (sub < 0 || sub >= Configures) ERROR();
	planeForce((enum Configure *)&sub,datxIntz(0,dat),1);
}
void planeGetter(void **dat, int sub)
{
	if (sub < 0 || sub >= Configures) ERROR();
	datxInt(dat,configure[sub]);
}
void planeNamer(void *dat, int sub)
{
	planeSet(sub,datxChrz(0,dat));
}
void planeRefer(void **dat, int sub)
{
	datxStr(dat,planeGet(sub));
}
void planeSettee(int val, int sub)
{
	void *dat = 0;
	datxInt(&dat,val);
	planeSetter(dat,sub);
	free(dat);
}
int planeGettee(int sub)
{
	void *dat = 0; int val = 0;
	planeGetter(&dat,sub);
	val = *datxIntz(0,dat);
	free(dat);
	return val;
}
void planeFind(char **val, const char *key)
{
	void *src = 0; void *dst = 0;
	datxStr(&src,key);
	datxFind(&dst,src);
	assignStr(val,datxChrz(0,dst));
	free(src); free(dst);
}
void planeInsert(const char *key, const char *val)
{
	void *src = 0; void *dst = 0;
	datxStr(&src,key); datxStr(&dst,val);
	datxInsert(src,dst);
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
	planeSafe(Waits,RegisterHint);}
	sem_safe(&resource,{running &= ~(1<<External);});
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
	planeCat(configure[RegisterPrompt],chr);
	planeSafe(Waits,RegisterPrompt);}
	sem_safe(&resource,{running &= ~(1<<Console);});
	return 0;
}
void planeThread(enum Thread bit)
{
	switch (bit) {
	case (External): if (pthread_create(&thread[bit],0,planeExternal,0) != 0) ERROR(); break;
	case (Console): if (pthread_create(&thread[bit],0,planeConsole,0) != 0) ERROR(); break;
	case (Window): planeSafe(Open,Configures); sem_post(&ready[bit]); break;
	case (Process): planeSafe(Start,Configures); sem_post(&ready[bit]); break;
	default: ERROR();}
}
void planeFinish(enum Thread bit)
{
	switch (bit) {
	case (External): sem_wait(&ready[bit]); closeIdent(external); if (pthread_join(thread[bit],0) != 0) ERROR(); break;
	case (Console): sem_wait(&ready[bit]); close(STDIN_FILENO); if (pthread_join(thread[bit],0) != 0) ERROR(); break;
	case (Window): sem_wait(&ready[bit]); planeSafe(Close,Configures); break;
	case (Process): sem_wait(&ready[bit]); planeSafe(Stop,Configures); break;
	default: ERROR();}
}
void planeStarted(int val)
{
	int done = 0; int todo = 0;
	sem_safe(&resource,{running &= val; done = started & ~running; started &= ~done; todo = val & ~started; started = running = val;});
	for (enum Thread bit = 0; bit < Threads; bit++) if (done & (1<<bit)) planeFinish(bit);
	for (enum Thread bit = 0; bit < Threads; bit++) if (todo & (1<<bit)) planeThread(bit);
}
int planeRunning()
{
	int val = 0; sem_safe(&resource,{val = running;}); return val;
}
void planeEnque(enum Wait wait, enum Configure hint)
{
	sem_wait(&resource);
	if (wait < Waits) perpend[wait]++;
	if (qfull == qsize) {qsize++;
	waits = realloc(waits,qsize*sizeof(enum Wait));
	hints = realloc(hints,qsize*sizeof(enum Configure));
	for (int i = qsize-1; i > qhead; i--) {
	waits[i] = waits[i-1]; hints[i] = hints[i-1];}
	qhead++; if (qhead == qsize) qhead = 0;}
	waits[qtail] = wait; hints[qtail] = hint;
	qtail++; if (qtail == qsize) qtail = 0;
	qfull++;
	sem_post(&resource);
}
void planeDeque(enum Wait *wait, enum Configure *hint)
{
	sem_wait(&resource);
	if (*wait < Waits) perpend[*wait]--;
	if (qfull == 0) ERROR();
	*wait = waits[qhead]; *hint = hints[qhead];
	qhead++; if (qhead == qsize) qhead = 0;
	qfull--;
	sem_post(&resource);
}
void planeInit(zftype init, uftype dma, vftype safe, yftype main, xftype info, wftype draw)
{
	struct sigaction act;
	act.__sigaction_u.__sa_handler = planeTerm;
	if (sigaction(SIGTERM,&act,0) < 0) ERROR();
	if (pthread_key_create(&retstr,free) != 0) ERROR();
	sem_init(&resource,0,1); sem_init(&pending,0,0);
	for (enum Thread bit = 0; bit < Threads; bit++) sem_init(&ready[bit],0,0);
	datxSetter(planeSetter); datxGetter(planeGetter);
	datxNamer(planeNamer); datxRefer(planeRefer);
	sub0 = datxSub(); idx0 = puntInit(sub0,sub0,datxReadFp,datxWriteFp); dat0 = datxDat(sub0);
	luaxAdd("planeGet",protoTypeRj(planeGet)); luaxAdd("planeSet",protoTypeFh(planeSet)); luaxAdd("planeCat",protoTypeFh(planeCat));
	luaxAdd("datxSet",protoTypeLj(planeSettee)); luaxAdd("datxGet",protoTypeSj(planeGettee));
	luaxAdd("datxFind",protoTypeRk(planeFind)); luaxAdd("datxInsert",protoTypeRl(planeInsert));
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
	sem_safe(&pending,{sem_safe(&resource,{wait = waits[qhead]; hint = hints[qhead];});});
	planeMain(wait,hint);}
	closeIdent(internal);
}
int planeConfig(enum Configure cfg)
{
	return configure[cfg];
}
void planeSafe(enum Wait wait, enum Configure hint)
{
	sem_safe(&resource,{if ((callInfo(RegisterOpen) & (1<<Process)) != 0 && perpend[Stop] == 0) callSafe(wait,hint);});
	planeEnque(wait,hint);
	sem_safe(&resource,{if (qfull == 1) sem_post(&pending);});
}
void planeMain(enum Wait wait, enum Configure hint)
{
	enum Wait wval = 0;
	enum Configure hval = 0;
	sem_wait(&pending);
	planeDeque(&wval,&hval);
	sem_safe(&resource,{if (qfull > 0) sem_post(&pending);});
	if (wval != wait || hval != hint) ERROR();
	if (wait == Waits && hint != Configures) planeWake(hint);
	if (wait != Waits && hint == Configures) callMain(wait);
	if (wait == Close && hint == RegisterOpen) {sem_safe(&resource,{running &= ~(1<<Window);}); planeWake(hint);}
	if (wait == Stop && hint == RegisterOpen) {sem_safe(&resource,{running &= ~(1<<Process);}); planeWake(hint);}
	if (wait != Waits && hint != Configures && hint != RegisterOpen) ERROR();
	if (wait != Waits && hint != Configures && wait != Close && wait != Stop) ERROR();
	if (wait == Waits && hint == Configures) ERROR();
}
void planeReady(struct Pierce *given)
{
	for (int i = 0; i < configure[PierceSize]; i++) pierce[i] = given[i]; found = 0;
}
