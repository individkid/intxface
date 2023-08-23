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
	int optimize;
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
int *intstk = 0;
int numstk = 0;
int idxstk = 0;
int configure[Configures] = {0};
struct Center center = {0};
int sub0 = 0;
int idx0 = 0;
void **dat0 = 0;
int started = 0;
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
pthread_t thread[Procs];
pthread_key_t retstr;
// resource protected:
char **string = 0;
int strsiz = 0;
int numpipe = 0;
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
int planeEnque(enum Proc proc, enum Wait wait, enum Configure hint);
void planeDeque(enum Proc *proc, enum Wait *wait, enum Configure *hint);
void planeSafe(enum Proc proc, enum Wait wait, enum Configure hint);

float *planeXform4(float *mat, float *org0, float *org1, float *org2, float *org3, float *mov0, float *mov1, float *mov2, float *mov3)
{
	// X such that X*org=mov
	float org[16]; float mov[16];
	if (org0[3] != 1.0 || org1[3] != 1.0 || org2[3] != 1.0 || org3[3] != 1.0) ERROR();
	if (mov0[3] != 1.0 || mov1[3] != 1.0 || mov2[3] != 1.0 || mov3[3] != 1.0) ERROR();
	for (int i = 0; i < 4; i++) org[i+0] = org0[i];
	for (int i = 0; i < 4; i++) org[i+4] = org1[i];
	for (int i = 0; i < 4; i++) org[i+8] = org2[i];
	for (int i = 0; i < 4; i++) org[i+12] = org3[i];
	for (int i = 0; i < 4; i++) mov[i+0] = mov0[i];
	for (int i = 0; i < 4; i++) mov[i+4] = mov1[i];
	for (int i = 0; i < 4; i++) mov[i+8] = mov2[i];
	for (int i = 0; i < 4; i++) mov[i+12] = mov3[i];
	return timesmat(copymat(mat,mov,4),invmat(org,4),4);
}
float *planeXform3(float *mat, float *fix0, float *org0, float *org1, float *org2, float *mov0, float *mov1, float *mov2)
{
	return planeXform4(mat,fix0,org0,org1,org2,fix0,mov0,mov1,mov2);
}
float *planeXform2(float *mat, float *fix0, float *fix1, float *org0, float *org1, float *mov0, float *mov1)
{
	return planeXform3(mat,fix0,fix1,org0,org1,fix1,mov0,mov1);
}
float *planeXform1(float *mat, float *fix0, float *fix1, float *fix2, float *org0, float *mov0)
{
	return planeXform2(mat,fix0,fix1,fix2,org0,fix2,mov0);
}
typedef float *(*planeXform)(float *mat, float *fix, float *nrm, float *org, float *cur);
float *planeSlideOrthoMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// distance to perpendicular to ortho fixed; cursor mapped
	float bas[4][4]; float mov[4][4]; float dif[4]; float neg[4];
	for (int i = 0; i < 4; i++) unitvec(unitvec(bas[i],4,3),3,i);
	plusvec(copyvec(zerovec(dif,4),cur,3),scalevec(copyvec(neg,org,3),-1.0,3),3);
	for (int i = 0; i < 4; i++) plusvec(copyvec(mov[i],bas[i],4),dif,4);
	return planeXform4(mat,bas[0],bas[1],bas[2],bas[3],mov[0],mov[1],mov[2],mov[3]);
}
float *planeSlideFocalMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// distance to perpendicular to cursor fixed; cursor mapped
	return mat;
}
float *planeSlideNormalMouse(float *mat, float *fix, float *nrm, float *org, float *cur)
{
	// distance to perpendicular to normal fixed; cursor mapped
	return mat;
}
float *planeRotate2(float *mat, float *fix, float *dif0, float *dif1, float *org, float *cur)
{
	float fix0[4]; float fix1[4]; float fix2[4];
	float nrm0[4]; float nrm1[4]; float neg[3];
	scalevec(copyvec(neg,copyvec(fix0,fix,3),3),-1.0,3);
	plusvec(crossvec(copyvec(fix1,dif0,3),dif1),fix,3);
	plusvec(normvec(plusvec(copyvec(nrm0,org,3),neg,3),3),fix,3);
	plusvec(normvec(plusvec(copyvec(nrm1,cur,3),neg,3),3),fix,3);
	fix0[3] = fix1[3] = nrm0[3] = nrm1[3] = 1.0;
	scalevec(unitvec(fix2,4,3),2.0,4);
	return planeXform1(mat,fix0,fix1,fix2,nrm0,nrm1);
}
float *planeRotate1(float *mat, float *fix, float *pnt0, float *dif1, float *org, float *cur)
{
	float neg[3]; float dif0[3];
	scalevec(copyvec(neg,fix,3),-1.0,3);
	plusvec(copyvec(dif0,pnt0,3),neg,3);
	return planeRotate2(mat,fix,dif0,dif1,org,cur);
}
float *planeRotate0(float *mat, float *fix, float *pnt0, float *pnt1, float *org, float *cur)
{
	float neg[3]; float dif0[3]; float dif1[3];
	scalevec(copyvec(neg,fix,3),-1.0,3);
	plusvec(copyvec(dif0,pnt0,3),neg,3);
	plusvec(copyvec(dif1,pnt1,3),neg,3);
	return planeRotate2(mat,fix,dif0,dif1,org,cur);
}
float *planeRotateOrthoMouse(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// perpendicular to ortho and original fixed; cursor mapped
	float uni[3]; return planeRotate1(mat,fix,org,unitvec(uni,3,2),org,cur);
}
float *planeRotateFocalMouse(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// perpendicular to cursor and original fixed; cursor mapped
	return planeRotate0(mat,fix,org,cur,org,cur);
}
float *planeRotateNormalMouse(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// perpendicular to normal and original fixed; cursor mapped
	return planeRotate1(mat,fix,org,nml,org,cur);
}
float *planeScaleOrthoMouse(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to ortho fixed; cursor mapped
	return mat;
}
float *planeScaleFocalMouse(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to cursor fixed; cursor mapped
	return mat;
}
float *planeScaleNormalMouse(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to normal fixed; cursor mapped
	return mat;
}
float *planeSlideOrthoRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to ortho offset
	return mat;
}
float *planeSlideFocalRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to cursor offset
	return mat;
}
float *planeSlideNormalRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to normal offset
	return mat;
}
float *planeAngle(float *mat, float *pnt0, float *pnt1, float ang)
{
	float fix0[4]; float fix1[4]; float hyp0[4];
	float dif1[3]; float dif2[3]; float dif3[3];
	float nrm2[4]; float nrm3[4]; float neg0[3];
	float leg2[3]; float leg3[3];
	// TODO convert roller to radians
	copyvec(fix0,pnt0,3); fix0[3] = 1.0;
	copyvec(fix1,pnt1,3); fix1[3] = 1.0;
	copyvec(hyp0,fix0,3); hyp0[3] = -1.0;
	// fix0 fix1 and hyp0 are independent
	scalevec(copyvec(neg0,fix0,3),-1.0,3);
	plusvec(copyvec(dif1,fix1,3),neg0,3);
	// dif1 is from fix0 to fix1
	dif2[0] = dif1[0]; dif2[1] = dif1[1];
	dif2[2] = -(dif1[0]*dif1[0]+dif1[1]*dif1[1])/dif1[2];
	// dif2 is any perpendicular to dif1
	crossvec(copyvec(dif3,dif1,3),dif2);
	// dif3 is any perpendicular to dif1 and dif2
	scalevec(copyvec(leg2,normvec(dif2,3),3),cos(ang),3);
	scalevec(copyvec(leg3,normvec(dif3,3),3),sin(ang),3);
	// dif2 and dif3 are now unit length
	plusvec(copyvec(dif3,leg2,3),leg3,3);
	// dif3 is now angle between dif2 and prior dif3
	plusvec(copyvec(nrm2,dif2,3),fix0,3); nrm2[3] = 1.0;
	// nrm2 is independent from fix0 fix0 and hyp0
	plusvec(copyvec(nrm3,dif3,3),fix0,3); nrm3[3] = 1.0;
	// nrm3 is rotation from nrm2
	return planeXform1(mat,fix0,fix1,hyp0,nrm2,nrm3);
}
float *planeRotateOrthoRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// distance to ortho fixed
	float tmp[3]; copyvec(tmp,fix,3); tmp[2] -= 1.0;
	return planeAngle(mat,fix,tmp,cur[3]-org[3]);
}
float *planeRotateFocalRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// distance to cursor fixed
	return planeAngle(mat,fix,cur,cur[3]-org[3]);
}
float *planeRotateNormalRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// distance to normal fixed
	float tmp[3]; plusvec(copyvec(tmp,fix,3),nml,3);
	return planeAngle(mat,fix,tmp,cur[3]-org[3]);
}
float *planeScaleOrthoRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to normal scaled
	return mat;
}
float *planeScaleFocalRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to cursor scaled
	return mat;
}
float *planeScaleNormalRoller(float *mat, float *fix, float *nml, float *org, float *cur)
{
	// TODO distance to perpendicular to normal scaled
	return mat;
}
void planeCast(struct Center *ptr, enum Memory mem, int idx)
{
	// usage: Prep [Cast Conj [Cast Conj] Cast] Write Compl
	struct Matrix *tmp = 0;
	int index = configure[RegisterIndex]-ptr->idx;
	ptr->idx = idx-index; configure[RegisterIndex] = idx;
	switch (ptr->mem) {
	case (Allmatz): tmp = ptr->all; ptr->all = 0; break;
	case (Fewmatz): tmp = ptr->few; ptr->few = 0; break;
	case (Onematz): tmp = ptr->one; ptr->one = 0; break;
	default: ERROR();}
	ptr->mem = mem; configure[RegisterMemory] = mem;
	switch (ptr->mem) {
	case (Allmatz): ptr->all = tmp; break;
	case (Fewmatz): ptr->few = tmp; break;
	case (Onematz): ptr->one = tmp; break;
	default: ERROR();}
}
float *planeCenter()
{
	int index = configure[RegisterIndex]-center.idx;
	if (index < 0 || index >= center.siz) ERROR();
	if (center.mem != configure[RegisterMemory]) ERROR();
	switch (center.mem) {
	case (Allmatz): return center.all[index].mat;
	case (Fewmatz): return center.few[index].mat;
	case (Onematz): return center.one[index].mat;
	default: ERROR();}
	return 0;
}
struct Kernel *planeKernel()
{
	int index = 0; int base = 0; int size = 0;
	switch ((enum Memory)configure[RegisterMemory]) {
	case (Allmatz): base = configure[SubjectBase]; size = configure[SubjectSize]; break;
	case (Fewmatz): base = configure[ObjectBase]; size = configure[ObjectSize]; break;
	case (Onematz): base = configure[ElementBase]; size = configure[ElementSize]; break;
	default: ERROR();}
	index = configure[RegisterIndex] - base;
	if (index < 0 || index >= size) ERROR();
	switch ((enum Memory)configure[RegisterMemory]) {
	case (Allmatz): return subject + index;
	case (Fewmatz): return object + index;
	case (Onematz): return element + index;
	default: ERROR();}
	return 0;
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
	switch ((enum Tool)configure[RegisterTool]) {
	case (Mouse): switch ((enum Change)configure[RegisterChange]) {
	case (Slide): switch ((enum Fixed)configure[RegisterFixed]) {
	case (Ortho): return planeSlideOrthoMouse;
	case (Focal): return planeSlideFocalMouse;
	case (Normal): return planeSlideNormalMouse;
	default: ERROR();}
	case (Rotate): switch ((enum Fixed)configure[RegisterFixed]) {
	case (Ortho): return planeRotateOrthoMouse;
	case (Focal): return planeRotateFocalMouse;
	case (Normal): return planeRotateNormalMouse;
	default: ERROR();}
	case (Scale): switch ((enum Fixed)configure[RegisterFixed]) {
	case (Ortho): return planeScaleOrthoMouse;
	case (Focal): return planeScaleFocalMouse;
	case (Normal): return planeScaleNormalMouse;
	default: ERROR();}
	default: ERROR();}
	case (Roller): switch ((enum Change)configure[RegisterChange]) {
	case (Slide): switch ((enum Fixed)configure[RegisterFixed]) {
	case (Ortho): return planeSlideOrthoRoller;
	case (Focal): return planeSlideFocalRoller;
	case (Normal): return planeSlideNormalRoller;
	default: ERROR();}
	case (Rotate): switch ((enum Fixed)configure[RegisterFixed]) {
	case (Ortho): return planeRotateOrthoRoller;
	case (Focal): return planeRotateFocalRoller;
	case (Normal): return planeRotateNormalRoller;
	default: ERROR();}
	case (Scale): switch ((enum Fixed)configure[RegisterFixed]) {
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
	if (configure[RegisterFind]) {
	for (int i = 0; i < configure[PierceSize]; i++) {
	struct Pierce *temp = pierce + i;
	if (!found || !found->vld || (temp->vld && temp->fix[2] < found->fix[2])) found = temp;}} else {
	int index = configure[RegisterIndex] - configure[PierceBase];
	if (index >= 0 && index < configure[PierceSize]) found = pierce + index;}
	if (!found) found = &unfound;
	return found;
}
void planeStage(enum Configure cfg)
{
	switch (cfg) {
	case (RegisterDone): configure[RegisterDone] = callInfo(RegisterDone); break;
	case (RegisterOpen): configure[RegisterOpen] = planeRunning(); break;
	case (CenterMemory): configure[CenterMemory] = center.mem; break;
	case (CenterSize): configure[CenterSize] = center.siz; break;
	case (CenterIndex): configure[CenterIndex] = center.idx; break;
	case (CenterSelf): configure[CenterSelf] = center.slf; break;
	case (RegisterMemory): configure[RegisterMemory] = center.mem; break;
	case (RegisterIndex): configure[RegisterIndex] = center.idx; break;
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
void planeConfig(enum Configure cfg, int val)
{
	int tmp = 0;
	if (cfg < 0 || cfg >= Configures) ERROR();
	tmp = configure[cfg]; configure[cfg] = val;
	switch (cfg) {
	case (PierceSize): pierce = planeResize(pierce,sizeof(struct Pierce),val,tmp); break;
	case (PierceBase): pierce = planeRebase(pierce,sizeof(struct Pierce),configure[PierceSize],val,tmp); break;
	case (SubjectSize): subject = planeResize(subject,sizeof(struct Kernel),val,tmp); break;
	case (SubjectBase): subject = planeRebase(subject,sizeof(struct Kernel),configure[SubjectSize],val,tmp); break;
	case (ObjectSize): object = planeResize(object,sizeof(struct Kernel),val,tmp); break;
	case (ObjectBase): object = planeRebase(object,sizeof(struct Kernel),configure[ObjectSize],val,tmp); break;
	case (ElementSize): element = planeResize(element,sizeof(struct Kernel),val,tmp); break;
	case (ElementBase): element = planeRebase(element,sizeof(struct Kernel),configure[ElementSize],val,tmp); break;
	case (MachineSize): machine = planeResize(machine,sizeof(struct Machine),val,tmp); break;
	case (RegisterOpen): planeStarted(val); break;
	case (RegisterFind): found = 0; break;
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
	int idx = configure[RegisterIndex];
	if (center.mem != configure[RegisterMemory]) ERROR();
	switch (center.mem) {
	case (Piercez): src = idx-configure[PierceBase]; siz = configure[PierceSize]; dst = idx-center.idx; break;
	default: ERROR();}
	if (src < 0 || src >= siz || dst < 0 || dst >= center.siz) ERROR();
	switch (center.mem) {
	case (Piercez): copyPierce(&center.pie[dst],&pierce[src]); break;
	default: ERROR();}
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
	case (Cast): planeCast(&center,mptr->mem,mptr->idx); break;
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
void planeDupstr(char **ptr, int idx)
{
	sem_wait(&resource);
	if (strsiz == 0) planeCatstr("");
	*ptr = strdup(string[idx]);
	sem_post(&resource);
}
void planeInsstr(const char *src, int len, int idx, int loc)
{
	sem_wait(&resource);
	sem_post(&resource);
}
void planeDelstr(int len, int idx, int loc)
{
	sem_wait(&resource);
	sem_post(&resource);
}
void planeOutstr(const char *str)
{
	write(STDIN_FILENO,str,strlen(str));
}
void planeClrstr(char **ptr)
{
	sem_wait(&resource);
	if (strsiz == 0) ERROR();
	printf("planeClrstr %s\n",string[0]);
	*ptr = strdup(string[0]);
	free(string[0]);
	for (int i = 1; i < strsiz; i++) string[i-1] = string[i];
	strsiz--;
	string = realloc(string,strsiz*sizeof(char*));
	printf("planeClrstr\n");
	sem_post(&resource);
}
void planeCatstr(const char *str)
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
int planeSide(const char *exp, const char *arg)
{
	const struct Closure *fnc = protoCloseFf(arg);
	if (luaxExpr(exp,fnc) != 0) ERROR();
	return protoResultFf();
}
void planeTerm(int sig)
{
}
void *planeExternal(void *ptr)
{
	struct Argument arg = {0}; char *str = 0;
	planeClrstr(&str); free(str); planeClrstr(&str);
	if ((external = wrapIdent(Planez,str)) < 0) exitErr(__FILE__,__LINE__); free(str);
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
	planeSafe(Procs,Waits,CenterMemory);}
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
	planeCatstr(chr);}
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
	if ((internal = openPipe()) < 0) ERROR();
	luaxAdd("planeDupstr",protoTypeSf(planeDupstr)); luaxAdd("planeOutstr",protoTypeHf(planeOutstr));
	luaxAdd("planeInsstr",protoTypeRp(planeInsstr)); luaxAdd("planeDelstr",protoTypeRq(planeDelstr));
	luaxAdd("planeSetcfg",protoTypeCg(planeSetcfg)); luaxAdd("planeGetcfg",protoTypeTl(planeGetcfg));
	datxDupstr(planeDupstr); datxOutstr(planeOutstr);
	datxInsstr(planeInsstr); datxDelstr(planeDelstr);
	datxSetcfg(planeSetcfg); datxGetcfg(planeGetcfg);
	datxEmbed(planeSide); datxCaller(planeCall);
	sub0 = datxSub(); idx0 = puntInit(sub0,sub0,datxReadFp,datxWriteFp); dat0 = datxDat(sub0);
	callDma = dma; callSafe = safe; callMain = main; callInfo = info; callDraw = draw;
	init(); planeBoot(); while (1) {
	enum Wait wait = 0; enum Configure hint = 0;
	sem_safe(&resource,{if (!qfull && !started) break;});
	planeMain();} closeIdent(internal);
}
int planeInfo(enum Configure cfg)
{
	return configure[cfg];
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
	else if (*hint != ResultHint) idle = 1;
	sem_post(&resource);
	if (idle) planeSafe(Procs,Waits,ResultHint);
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
