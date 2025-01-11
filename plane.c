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
struct Kernel *matrix = 0;
struct Machine *machine = 0;
struct Center *center = 0;
sem_t switchSem = {0};
int *intstk = 0;
int numstk = 0;
int idxstk = 0;
int sub0 = 0;
int idx0 = 0;
void **dat0 = 0;
int external = 0;
int wakeup = 0;
void *internal = 0;
void *response = 0;
sem_t pipeSem = {0};
char **string = 0;
int strsiz = 0;
wftype callPass;
nftype callCopy;
vftype callFork;
zftype callInfo;
sem_t testSem = {0};

DECLARE_DEQUE(struct Center *,Centerq)

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
    int tmp; int cfg = callInfo(ManipFixed,0,planeRcfg);
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

void planeStage(enum Configure cfg)
{
	// TODO callInfo(cfg,val,planeWcfg);
}
void *planeResize(void *ptr, int mod, int siz, int tmp) // TODO called by callback
{
	char *result = realloc(ptr,siz*mod);
	for (int i = tmp*mod; i < siz*mod; i++) result[i] = 0;
	return result;
}
void *planeRebase(void *ptr, int mod, int siz, int bas, int tmp) // TODO called by callback
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
	callCopy(center,callPass);
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
void planeCall(void **, const char *); // TODO
void planeCopy(struct Center *center)
{
	switch (center->mem) {
	case (Stackz): for (int i = 0; i < center->siz; i++) planeCall(dat0,center->str[i]); break;
	case (Machinez): for (int i = 0; i < center->siz; i++) {
		int index = center->idx+i;
		if (index < 0 || index >= callInfo(MachineSize,0,planeRcfg)) ERROR();
		if (center->mch[i].xfr == Name) {void *dat = 0; datxInt(&dat,index+1);
		datxInserts("_",center->mch[i].str,dat,identType("Int")); free(dat);}
		copyMachine(&machine[index],&center->mch[i]);} break;
	default: callCopy(center,callPass); break;}
}
int planeEscape(int lvl, int nxt)
{
	int inc = (lvl > 0 ? 1 : (lvl == 0 ? 0 : -1)); lvl *= inc;
	for (nxt += inc; lvl > 0 && nxt < callInfo(MachineSize,0,planeRcfg) && nxt >= 0; nxt += inc)
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
void planePass(); // TODO
void planeEcho()
{
	if (callInfo(RegisterType,0,planeRcfg) != identType("Center")) ERROR();
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
void planeShow(); // TODO
void planeRead(); // TODO
void planeWrite(); // TODO
void planeSwitch(enum Thread tag, int idx)
{
	for (int next = 0; next >= 0 && next < callInfo(MachineSize,0,planeRcfg); next++) {
	struct Machine *mptr = &machine[next];
	// {char *xfr = 0; showTransfer(mptr->xfr,&xfr);
	// printf("planeSwitch %d %s\n",next,xfr); free(xfr);}
	switch (mptr->xfr) {
	case (Read): planeRead(); break; // TODO pop center from pipe thread
	case (Write): planeWrite(); break; // TODO push center to pipe thread
	// TODO way to pop/push from/to console thread
	case (Stage): for (int i = 0; i < mptr->siz; i++) planeStage(mptr->sav[i]); break;
	case (Force): for (int i = 0; i < mptr->num; i++) planeSync(mptr->cfg[i],mptr->val[i]); break;
	case (Cont): planeCont(); break;
	case (Prep): planePrep(); break;
	case (Send): planeSend(); break;
	case (Recv): planeRecv(); break;
	case (Disp): planeDisp(); break;
	case (Copy): planeCopy(center); break;
	case (Jump): next = planeEscape(planeIval(&mptr->exp[0]),next) - 1; break;
	case (Goto): next = next + planeIval(&mptr->exp[0]) - 1; break;
	case (Nest): break;
	case (Name): if (idxstk > 0) next = next - 1; else ERROR(); break;
	case (Eval): planeSync(RegisterType,datxEval(dat0,&mptr->exp[0],-1)); break;
	case (Pass): planePass(); break; // TODO eval expression given center
	case (Echo): planeEcho(); break; // TODO get center from expression side effect
	case (Hide): planeHide(); break; // TODO get center from console thread
	case (Show): planeShow(); break; // TODO put center to console thread
	default: break;}
	if (next == callInfo(MachineSize,0,planeRcfg)) {next = 0;
	if (sem_wait(&switchSem) != 0) exitErr(__FILE__,__LINE__);}}
}
void planeSwitched(enum Thread tag, int idx)
{
	/*configure[MachineSize] = 0;*/
	free(machine);
	if (sem_post(&switchSem) != 0) exitErr(__FILE__,__LINE__);
}
void planeSwitcher(enum Configure cfg, int sav, int val)
{
	if (cfg != RegisterMask) exitErr(__FILE__,__LINE__);
	if (sem_post(&switchSem) != 0) exitErr(__FILE__,__LINE__);
}
void planeSelect(enum Thread tag, int idx)
{
	char *str = 0; // FIXME let planeHide do this when planePopstr is Argument
	if ((external = identWrap(Planez,str)) < 0) exitErr(__FILE__,__LINE__); free(str);
	while (1) {
	int sub = waitRead(0,(1<<external)|(1<<wakeup));
	if (sub == wakeup) {
	struct Center *center = 0;
	if (!checkRead(wakeup)) break;
	readInt(wakeup);
	sem_wait(&pipeSem);
	center = maybeCenterq(0,response);
	sem_post(&pipeSem);
	writeCenter(center,external);
	freeCenter(center);
	allocCenter(&center,0);}
	else if (sub == external) {
	struct Center *center = 0;
	if (!checkRead(external)) break;
	allocCenter(&center,1);
	readCenter(center,external);
	sem_wait(&pipeSem);
	pushCenterq(center,internal);
	sem_post(&pipeSem);}
	else break;}
}
void planeSelected(enum Thread tag, int idx)
{
	closeIdent(external);
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
	// TODO hide string and let machine thread process it
	}
}
void planeConsoled(enum Thread tag, int idx)
{
	close(STDIN_FILENO);
}
void planeNoop(enum Thread tag, int idx)
{
}
void planeBack(enum Configure cfg, int sav, int val)
{
    if (cfg == RegisterOpen && (val & (1<<PipeThd)) && !(sav & (1<<PipeThd)))
    callFork(PipeThd,0,planeSelect,planeSelected,planeNoop);
    if (cfg == RegisterOpen && !(val & (1<<PipeThd)) && (sav & (1<<PipeThd)))
    planeSelected(PipeThd,0);
    if (cfg == RegisterOpen && (val & (1<<StdioThd)) && !(sav & (1<<StdioThd)))
    callFork(StdioThd,0,planeConsole,planeConsoled,planeNoop);
    if (cfg == RegisterOpen && !(val & (1<<StdioThd)) && (sav & (1<<StdioThd)))
    planeConsoled(StdioThd,0);
    if (cfg == RegisterOpen && (val & (1<<SwitchThd)) && !(sav & (1<<SwitchThd)))
    callFork(SwitchThd,0,planeSwitch,planeSwitched,planeNoop);
    if (cfg == RegisterOpen && !(val & (1<<SwitchThd)) && (sav & (1<<SwitchThd)))
    planeSwitched(SwitchThd,0);
}
void planeInit(nftype copy, oftype call, vftype fork, wftype pass, zftype info)
{
	callCopy = copy;
	callFork = fork;
	callPass = pass;
	callInfo = info;
    call(RegisterOpen,planeBack);
    call(RegisterMask,planeSwitcher);
    info(RegisterPoll,1,planeWcfg);
    info(RegisterOpen,(1<<FenceThd),planeWots);
	if (sem_init(&testSem, 0, 0) != 0) exitErr(__FILE__,__LINE__);
    info(RegisterOpen,(1<<TestThd),planeWots);
    if (sem_wait(&testSem) != 0) exitErr(__FILE__,__LINE__);
	if (sem_destroy(&testSem) != 0) exitErr(__FILE__,__LINE__);
    info(RegisterOpen,(1<<TestThd),planeWotc);
    info(RegisterOpen,(1<<FenceThd),planeWotc);
}
int count = 0;
void planeLoop()
{
	if (count++ < 1000) return;
	if (sem_post(&testSem) != 0) exitErr(__FILE__,__LINE__);
}
