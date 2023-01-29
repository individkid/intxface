#include "plane.h"
#include "face.h"
#include "luax.h"
#include "metx.h"
#include "argx.h"
#include "memx.h"
#include "type.h"
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/errno.h>
#include <string.h>
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
#include <setjmp.h>
#include <signal.h>
#include <regex.h>

struct Kernel {
	struct Matrix compose; // optimization
	struct Matrix maintain; // change to points
	struct Matrix written; // portion written
	struct Matrix towrite; // portion to write
};
// owned by main thread:
struct Kernel *subject = {0};
struct Kernel *object = 0;
struct Kernel *element = {0};
struct Pierce *pierce = 0;
struct Pierce *found = 0;
struct Machine *machine = 0;
int configure[Configures] = {0};
struct Center center = {0};
regex_t *pattern = 0;
int numpat = 0;
char **result = 0;
// constant after other threads start:
int internal = 0;
int external = 0;
int started = 0;
uftype callDma = 0;
vftype callSafe = 0;
yftype callUser = 0;
xftype callInfo = 0;
wftype callDraw = 0;
pthread_t thread[Concurs];
pthread_key_t retstr;
// owned by argx memx luax thread:
jmp_buf jmpbuf;
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
void *runmask[Waits] = {0};
sem_t resource;
sem_t pending;
sem_t ready[Concurs];
sem_t finish[Concurs];
void planeMemx(void **mem, void *giv);
void planeRead();
const char *planeGet(int idx);
int planeSet(int idx, const char *str);
int planeCat(int idx, const char *str);
int planeRunning();
void planeStarted(int val);

void planeAlize(float *dir, const float *vec) // normalize
{
}
void planeCross(float *axe, const float *fix, const float *cur)
{
}
typedef void (*planeXform)(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang);
void planeXtate(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang) // rotate
{
}
void planeXlate(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang) // translate
{
}
void planeScale(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang)
{
}
void planeFocal(float *mat, const float *pic, const float *cor, const float *fix, const float *cur, float ang)
{
}
void *planeRealloc(void *ptr, int siz, int tmp, int mod)
{
	char *result = realloc(ptr,siz*mod);
	for (int i = tmp*mod; i < siz*mod; i++) result[i] = 0;
	return result;
}
void planeCopy(struct Machine *dst, struct Machine *src)
{
	char *str = 0;
	int len = 0;
	showMachine(src,&str,&len);
	len = 0;
	hideMachine(dst,str,&len);
	free(str);
}
struct Matrix *planePointer()
{
	int index = configure[RegisterIndex] - center.idx;
	if (index < 0 || index >= center.siz) return 0;
	if (center.mem != (enum Memory)configure[RegisterMemory]) return 0;
	switch(center.mem) {
		case (Allmatz): return center.all + index;
		case (Fewmatz): return center.few + index;
		case (Onematz): return center.one + index;
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
planeXform planeFunc()
{
	switch ((enum Transform)configure[RegisterXform]) {
		case (Translate): return planeXlate;
		case (Rotate): return planeXtate;
		case (Scale): return planeScale;
		case (Zoom): return planeFocal;
		default: break;}
	return 0;
}
struct Matrix *planeMatrix(enum Accumulate accumulate)
{
	switch (accumulate){
		case (Compose): return &planeKernel()->compose;
		case (Maintain): return &planeKernel()->maintain;
		case (Written): return &planeKernel()->written;
		case (Towrite): return &planeKernel()->towrite;
		case (OfCenter): {
			struct Matrix *matrix = planePointer();
			if (matrix == 0) return 0;
			return matrix;}
		default: break;}
	return 0;
}
void planeCalculate(struct Matrix *matrix)
{
	struct Vector picture = {0};
	struct Vector corner = {0};
	struct Vector fixed = {0};
	struct Vector cursor = {0};
	float angle = 0;
	picture.vec[0] = configure[WindowLeft]; picture.vec[1] = configure[WindowBase];
	corner.vec[0] = configure[WindowWide]; corner.vec[1] = configure[WindowHigh];
	fixed.vec[0] = configure[ClosestLeft]; fixed.vec[1] = configure[ClosestBase];
	cursor.vec[0] = configure[CursorLeft]; cursor.vec[1] = configure[CursorBase];
	angle = configure[CursorAngle];
	planeFunc()(matrix->mat,picture.vec,corner.vec,fixed.vec,cursor.vec,angle);
}
void planePattern(int idx, const char *str)
{
	char buf[128] = {0};
	int val = 0;
	if (idx >= numpat) {numpat = idx+1;
	pattern = realloc(pattern,numpat*sizeof(regex_t));}
	regfree(pattern+idx);
	if ((val = regcomp(pattern+idx,str,0))) {
	regerror(val,pattern+idx,buf,128);
	fprintf(stderr,"regcomp error: %s\n",buf);
	regfree(pattern+idx);}
}
void planeMatch()
{
	int str = configure[CompareString];
	int pat = configure[ComparePattern];
	int num = configure[CompareNumber];
	for (int i = 0; i < configure[CompareSize]; i++) free(result[i]);
	free(result); result = 0; configure[CompareSize] = 0;
	if (numpat == 0 || numstr == 0) return;
	while (num) {
	regex_t *ptr = pattern+(pat%numpat);
	int siz = ptr->re_nsub+1;
	regmatch_t pmatch[siz];	
	if (regexec(ptr,planeGet(str),siz,pmatch,0) != 0) {
	if (num > 0) {num--; str++;} else {num++; pat++;} continue;}
	result = malloc(siz*sizeof(char*));
	for (int i = 0; i < siz; i++) {
	if (pmatch[i].rm_so < 0) result[i] = strdup("");
	else result[i] = strndup(planeGet(str)+pmatch[i].rm_so,pmatch[i].rm_eo-pmatch[i].rm_so);}
	configure[CompareSize] = siz;
	break;}
}
struct Pierce *planePierce()
{
	if (found) return found;
	for (int i = 0; i < configure[TriangleSize]; i++) {
		struct Pierce *temp = pierce + i%configure[TriangleSize];
		if (!found || (temp->vld && temp->fix[2] < found->fix[2])) found = temp;}
	return found;
}
void planePreconfig(enum Configure cfg)
{
	switch (cfg) {
		case (RegisterDone): configure[RegisterDone] = callInfo(RegisterDone); break;
		case (CompareSize): planeMatch(); break;
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
		default: break;}
}
void planePostconfig(enum Configure cfg, int idx)
{
	if (center.mem != Configurez || idx < 0 || idx >= center.siz) return;
	center.cfg[idx] = cfg;
	switch (cfg) {
	case (RegisterOpen): configure[RegisterOpen] = planeRunning(); break;
	default: break;}
	center.val[idx] = configure[cfg];
}
void planeReconfig(enum Configure cfg, int val)
{
	int tmp = configure[cfg];
	configure[cfg] = val;
	switch (cfg) {
		case (TriangleSize): pierce = planeRealloc(pierce,val,tmp,sizeof(struct Pierce)); break;
		case (SubjectSize): object = planeRealloc(object,val,tmp,sizeof(struct Kernel)); break;
		case (ObjectSize): object = planeRealloc(object,val,tmp,sizeof(struct Kernel)); break;
		case (ElementSize): object = planeRealloc(object,val,tmp,sizeof(struct Kernel)); break;
		case (MachineSize): machine = planeRealloc(machine,val,tmp,sizeof(struct Machine)); break;
		case (RegisterOpen): planeStarted(val); break;
		default: break;}
}
void planeAlloc()
{
	freeCenter(&center);
	center.req = (enum Request)configure[CenterRequest];
	center.mem = (enum Memory)configure[CenterMemory];
	center.idx = configure[CenterIndex];
	center.siz = configure[CenterSize];
	center.slf = configure[CenterSelf];
	switch (center.mem) {
		case (Allmatz): allocMatrix(&center.all,center.siz); break;
		case (Fewmatz): allocMatrix(&center.few,center.siz); break;
		case (Onematz): allocMatrix(&center.one,center.siz); break;
		case (Piercez): allocPierce(&center.pie,center.siz); break;
		case (Stringz): allocStr(&center.str,center.siz); break;
		case (Resultz): allocStr(&center.str,center.siz); break;
		case (Configurez): allocConfigure(&center.cfg,center.siz); allocInt(&center.val,center.siz); break;
		default: center.siz = 0; break;}
}
void planeEcho()
{
	switch (center.mem) {
		case (Piercez): {
			int index = center.idx%configure[TriangleSize];
			while (index < 0) index += configure[TriangleSize];
			for (int i = 0; i < center.siz; i++, index++) center.pie[i] = pierce[index%configure[TriangleSize]];
			break;}
		case (Stringz): {
			int index = center.idx;
			for (int i = 0; i < center.siz; i++, index++) assignStr(center.str+i,planeGet(index));
			break;}
		case (Resultz): {
			int index = center.idx%configure[CompareSize];
			while (index < 0) index += configure[CompareSize];
			for (int i = 0; i < center.siz; i++, index++) assignStr(center.str+i,result[index%configure[CompareSize]]);
			break;}
		case (Configurez): {
			int index = center.idx%Configures;
			while (index < 0) index += Configures;
			for (int i = 0; i < center.siz; i++, index++) planePostconfig((enum Configure)(index%Configures),i);
			break;}
		default: break;}
}
void planeBuffer()
{
	switch (center.mem) {
		case (Stringz): if (center.idx < 0) for (int i = 0; i < center.siz; i++) center.idx = planeSet(-1,center.str[i]);
		else for (int i = 0; i < center.siz; i++) planeSet(center.idx+i,center.str[i]); break;
		case (Patternz): for (int i = 0; i < center.siz; i++) planePattern(center.idx+i,center.str[i]); break;
		case (Machinez): for (int i = 0; i < center.siz; i++) planeCopy(&machine[(center.idx+i)%configure[MachineSize]],&center.mch[i]); break;
		case (Configurez): for (int i = 0; i < center.siz; i++) planeReconfig(center.cfg[i],center.val[i]); callDma(&center); break;
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
int planeCompare(enum Configure cfg, int val, enum Compare cmp)
{
	switch (cmp) {
		case (Less): return (configure[cfg] < val);
		case (More): return (configure[cfg] > val);
		case (Equal): return (configure[cfg] == val);
		case (Nless): return (configure[cfg] >= val);
		case (Nmore): return (configure[cfg] <= val);
		case (Nequal): return (configure[cfg] != val);
		default: break;}
	return 0;
}
int planeCondition(int sum, int siz, enum Condition cnd)
{
	switch (cnd) {
		case (Every): return (sum == siz);
		case (None): return (sum == 0);
		case (Both): return (sum > 0 && sum < siz);
		case (Eorb): return (sum > 0);
		case (Norb): return (sum < siz);
		default: break;}
	return 0;
}
void planeExchange(int cal, int ret)
{
	struct Machine temp = machine[cal%configure[MachineSize]];
	machine[cal%configure[MachineSize]] = machine[ret%configure[MachineSize]];
	machine[ret%configure[MachineSize]] = temp;
}
void planeWake(enum Configure hint)
{
	configure[RegisterHint] = hint;
	if (configure[RegisterLine] < 0 || configure[RegisterLine] >= configure[MachineSize]) configure[RegisterLine] = 0;
	while (configure[RegisterLine] >= 0 && configure[RegisterLine] < configure[MachineSize]) {
		struct Machine *mptr = machine+configure[RegisterLine];
		int next = configure[RegisterLine]+1;
		int accum = 0;
		int size = 0;
		switch (mptr->xfr) {
			case (Save): case (Copy): case (Force): case (Setup): case (Jump): case (Goto): size = mptr->siz; break;
			default: break;}
		for (int i = 0; i < size; i++) switch (mptr->xfr) {
			case (Save): planePreconfig(mptr->cfg[i]); break; // kernel, center, pierce, or info to configure -- siz cfg
			case (Copy): planeReconfig(mptr->cfg[i],configure[mptr->oth[i]]); break; // configure to configure -- siz cfg oth
			case (Force): planeReconfig(mptr->cfg[i],mptr->val[i]); break; // machine to configure -- siz cfg val
			case (Setup): planePostconfig(mptr->cfg[i],mptr->val[i]); break; // configure to center -- siz cfg val
			case (Jump): accum += planeCompare(mptr->cfg[i],mptr->val[i],mptr->cmp[i]); break; // skip if true -- siz cfg val cmp cnd idx
			case (Goto): accum += planeCompare(mptr->cfg[i],mptr->val[i],mptr->cmp[i]); break; // jump if true -- siz cfg val cmp cnd idx
			default: break;}
		switch (mptr->xfr) {
			case (Read): planeRead(); break; // read internal pipe
			case (Write): writeCenter(&center,external); break; // write external pipe --
			case (Alloc): planeAlloc(); break; // configure to center --
			case (Echo): planeEcho(); break; // memory to center --
			case (Clear): identmat(planeMatrix(mptr->dst)->mat,4); break; // identity to matrix -- dst
			case (Invert): invmat(planeMatrix(mptr->dst)->mat,4); break; // invert in matrix -- dst
			case (Manip): planeCalculate(planeMatrix(mptr->dst)); break; // manip to matrix -- dst
			case (Follow): jumpmat(planeMatrix(mptr->dst)->mat,planeMatrix(mptr->src)->mat,4); break; // multiply to matrix -- src dst
			case (Precede): timesmat(planeMatrix(mptr->dst)->mat,planeMatrix(mptr->src)->mat,4); break; // multiply by matrix -- src dst
			case (Share): planeBuffer(); break; // dma to cpu or gpu --
			case (Draw): callDraw((enum Shader)configure[ArgumentShader],configure[ArgumentStart],configure[ArgumentStop]); break; // start shader --
			case (Jump): next = planeEscape((planeCondition(accum,size,mptr->cnd) ? mptr->idx : configure[RegisterNest]),next); break; // skip if true -- siz cfg val cmp cnd idx
			case (Goto): next = (planeCondition(accum,size,mptr->cnd) ? mptr->idx : next); break; // jump if true -- siz cfg val cmp cnd idx
			case (Nest): configure[RegisterNest] += mptr->idx; break; // nest to level -- idx
			case (Swap): planeExchange(mptr->idx,mptr->ret); break; // exchange machine lines -- idx ret
			default: break;}
		if (next == configure[RegisterLine]) {configure[RegisterLine] = next+1; break;}
		configure[RegisterLine] = next;}
}
void planeMemx(void **mem, void *giv)
{
	memxCopy(mem,giv);
	planeSet(-1,memxStr(*mem));
}
void planeRead()
{
	int num = 0;
	sem_wait(&resource); if ((num = numpipe)) numpipe--; sem_post(&resource);
	if (num) readCenter(&center,internal);
	else {struct Center tmp = {0}; center = tmp;}
}
const char *planeGet(int idx)
{
	const char *ret = 0;
	void *ptr = 0;
	sem_wait(&resource);
	ptr = pthread_getspecific(retstr);
	free(ptr);
	pthread_setspecific(retstr,0);
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
	if (idx < 0) {
		numstr++;
		strings = realloc(strings,numstr*sizeof(char*));
		idx = numstr-1;
		strings[numstr-1] = strdup("");}
	if (idx >= numstr) {
		strings = realloc(strings,(idx+1)*sizeof(char*));
		while (idx >= numstr) strings[numstr++] = strdup("");}
	free(strings[idx]); strings[idx] = strdup(str);
	ret = numstr;
	if (numstr == 3) {sem_post(&ready[Program]);}
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
void planeIntr()
{
	if (pthread_equal(pthread_self(),thread[Program])) longjmp(jmpbuf,1);
}
void planeTerm(int sig)
{
}
void planeExternal()
{
	char *inp = 0;
	char *out = 0;
	inp = strdup(planeGet(1)); out = strdup(planeGet(2));
	external = pipeInit(inp,out);
	free(inp); free(out);
	if (external < 0) ERROR();
	sem_post(&ready[External]);
	while (1) {
	struct Center center = {0};
	int sub = waitRead(0,1<<external);
	if (sub != external) break;
	if (!checkRead(external)) break;
	if (!checkWrite(internal)) break;
	readCenter(&center,external);
	writeCenter(&center,internal);
	sem_wait(&resource); numpipe++; sem_post(&resource);
	planeSafe(Waits,RegisterHint);}
}
void planeConsole()
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
		planeCat(configure[CompareConsole],chr);
		planeSafe(Waits,CompareConsole);}
}
void planeProgram()
{
	if (setjmp(jmpbuf) == 0) runProgram();
}
void planeBoot()
{
	for (int i = 0; Bootstrap__Int__Str(i); i++) {
	int len = 0;
	if (!hideCenter(&center,Bootstrap__Int__Str(i),&len)) ERROR();
	planeBuffer();}
}
void planeWrap(enum Concur bit, enum Wait pre, enum Wait post)
{
	planeSafe(pre,Configures); sem_post(&ready[bit]);
	sem_wait(&finish[bit]); planeSafe(post,Configures);	
}
void *planeThread(void *arg)
{
	enum Concur bit = (enum Concur)(uintptr_t)arg;
	switch (bit) {
	case (External): sem_sync(&ready[Program]); planeExternal(); break;
	case (Console): planeConsole(); break;
	case (Program): planeProgram(); break;
	case (Window): planeWrap(Window,Open,Close); break;
	case (Process): planeWrap(Process,Start,Stop); break;
	default: ERROR();}
	sem_wait(&resource); running &= ~(1<<bit); sem_post(&resource);
	return 0;
}
void planeFinish(enum Concur bit)
{
	switch (bit) {
	case (External): closeIdent(external); break;
	case (Console): close(STDIN_FILENO); break;
	case (Program): kill(getpid(),SIGTERM); break;
	case (Window): sem_post(&finish[Window]); break;
	case (Process): sem_post(&finish[Process]); break;
	default: ERROR();}
}
void planeStarted(int val)
{
	int dif = 0;
	sem_wait(&resource); dif = started & ~running; sem_post(&resource); // join each in started but not running
	for (enum Concur bit = 0; bit < Concurs; bit++) if (dif & (1<<bit)) {
	sem_wait(&ready[bit]); planeFinish(bit); if (pthread_join(thread[bit],0) != 0) ERROR(); sem_post(&ready[bit]);}
	sem_wait(&resource); started = running; sem_post(&resource);
	dif = started & ~val; // join each in started but not val
	for (enum Concur bit = 0; bit < Concurs; bit++) if (dif & (1<<bit)) {
	sem_wait(&ready[bit]); planeFinish(bit); if (pthread_join(thread[bit],0) != 0) ERROR(); sem_post(&ready[bit]);}
	dif = val & ~started; // create each in val but not started
	for (enum Concur bit = 0; bit < Concurs; bit++) if (dif & (1<<bit)) {
	sem_wait(&ready[bit]); if (pthread_create(&thread[bit],0,planeThread,(void*)(uintptr_t)bit) != 0) ERROR();}
	sem_wait(&resource); started = running = val; sem_post(&resource);
}
int planeRunning()
{
	int val = 0; sem_wait(&resource); val = running; sem_post(&resource); return val;
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
	qfull--; sem_post(&resource);
}
void planePeek(vftype user)
{
	enum Wait wait = 0;
	enum Configure hint = 0;
	sem_wait(&resource);
	wait = waits[qhead];
	hint = hints[qhead];
	sem_post(&resource);
	user(wait,hint);
}
int planeTodo()
{
	int val = 0; sem_wait(&resource); val = (qfull || running); sem_post(&resource); return val;
}
void planeInit(zftype init, uftype dma, vftype safe, yftype user, xftype info, wftype draw)
{
	struct sigaction act;
	act.__sigaction_u.__sa_handler = planeTerm;
	if (sigaction(SIGTERM,&act,0) < 0) ERROR();
	if (pthread_key_create(&retstr,free) != 0) ERROR();
	luaxAdd("planeGet",protoTypeRh(planeGet));
	luaxAdd("planeSet",protoTypeFh(planeSet));
	luaxAdd("planeCat",protoTypeFh(planeCat));
	intrFunc(planeIntr);
	sem_init(&resource,0,1);
	sem_init(&pending,0,0);
	for (enum Concur bit = 0; bit < Concurs; bit++) sem_init(&ready[bit],0,1);
	for (enum Concur bit = 0; bit < Concurs; bit++) sem_init(&finish[bit],0,0);
	callDma = dma;
	callSafe = safe;
	callUser = user;
	callInfo = info;
	callDraw = draw;
	addFlow("",protoTypeNf(memxInit),protoTypeMf(planeMemx));
	if ((internal = openPipe()) < 0) ERROR();
	init(); planeBoot();
	while (planeTodo()) planePeek(planeUser);
	closeIdent(internal);
}
int planeConfig(enum Configure cfg)
{
	return configure[cfg];
}
void planeSafe(enum Wait wait, enum Configure hint)
{
	sem_wait(&resource); if (callInfo(RegisterOpen) && perpend[Stop] == 0) callSafe(wait,hint); sem_post(&resource);
	planeEnque(wait,hint); sem_post(&pending);
}
void planeUser(enum Wait wait, enum Configure hint)
{
	enum Wait wval = 0;
	enum Configure hval = 0;
	sem_wait(&pending);
	planeDeque(&wval,&hval);
	if (wval != wait || hval != hint) ERROR();
	if (wait == Waits && hint != Configures) planeWake(hint);
	if (wait != Waits && hint == Configures) callUser(wait);
	// TODO wait != Waits && hint == RegisterOpen to clear running bit, and call planeWake(hint)
	if (wait != Waits && hint != Configures) ERROR();
}
void planeReady(struct Pierce *given)
{
	for (int i = 0; i < configure[TriangleSize]; i++) pierce[i] = given[i];
}
