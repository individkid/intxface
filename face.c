#include "face.h"
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <pthread.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <lua.h>

// per identifier state
int inp[NUMOPEN] = {0};
int out[NUMOPEN] = {0};
enum {None, // unused
	Wait, // pselect-able pipe
	Poll, // fifo not meant for pselect
	Seek, // non-blocking file
	Inet, // pselect-able meta-pipe
	Sock, // pselect-able received pipe
	Punt, // given stream function
} fdt[NUMOPEN] = {0};
int lim = 0;

// process identifier for waiting for child to finish
pid_t pid[NUMOPEN] = {0};

// function pointers to use instead of read and write
pftype rfn[NUMOPEN] = {0};
qftype wfn[NUMOPEN] = {0};

// write buffers
char *atom[NUMOPEN] = {0};
int atoms[NUMOPEN] = {0};
int atomz[NUMOPEN] = {0};

// whether and how to check for already open
enum {Off,Fwd,Rev} cache = 0;

// error function pointers
eftype inpexc[NUMOPEN] = {0};
eftype inperr[NUMOPEN] = {0};
eftype outerr[NUMOPEN] = {0};
char *exclua[NUMOPEN] = {0};
char *inplua[NUMOPEN] = {0};
char *outlua[NUMOPEN] = {0};
char *fnclua[NUMOPEN] = {0};
lua_State *luaerr = 0;

// server address for checking if already connected
struct sockaddr_in6 addr[NUMINET] = {0};
int mad = 0;
int idnt[NUMOPEN] = {0};

// thread to call function when pipe is readable
pthread_t cbpth[NUMOPEN] = {0};
cftype cbfnc[NUMOPEN] = {0};

// garbage collection
int bufsize = BUFSIZE;
int memmrkz = 0;
int memmrks = 0;
int *memmrk = 0;
int memptrz = 0;
int memptrs = 0;
void ***memptr = 0;

void debugStr(const char *str)
{
	fprintf(stderr,"%s\n",str); fflush(stderr);
}
void readNote(eftype exc, int idx)
{
	if (idx < 0 || idx >= lim) ERROR(exitErr,0)
	inpexc[idx] = exc;
}
void readJump(eftype err, int idx)
{
	if (idx < 0 || idx >= lim) ERROR(exitErr,0)
	inperr[idx] = err;
}
void writeJump(eftype err, int idx)
{
	if (idx < 0 || idx >= lim) ERROR(exitErr,0)
	outerr[idx] = err;
}
void closeIdent(int idx)
{
	if (fdt[idx] != None) {
		close(inp[idx]);
		if (inp[idx] != out[idx]) close(out[idx]);
		fdt[idx] = None;}
	inpexc[idx] = 0;
	inperr[idx] = 0;
	outerr[idx] = 0;
	while (lim > 0 && fdt[lim-1] == None) lim--;
}
void moveIdent(int idx0, int idx1)
{
	if (idx1 < 0 || idx1 >= lim) ERROR(inperr[idx1],idx1);
	if (idx0 < 0 || idx0 >= lim) ERROR(inperr[idx1],idx1);
	closeIdent(idx1);
	inp[idx1] = inp[idx0];
	out[idx1] = out[idx0];
	fdt[idx1] = fdt[idx0];
	pid[idx1] = pid[idx0];
	rfn[idx1] = rfn[idx0];
	wfn[idx1] = wfn[idx0];
	inpexc[idx1] = inpexc[idx0];
	inperr[idx1] = inperr[idx0];
	outerr[idx1] = outerr[idx0];
	closeIdent(idx0);
}
int findIdent(const char *str)
{
	struct stat old;
	struct stat new;
	if (cache == Off || stat(str,&new) != 0) return -1;
	for (int i = 0; i < lim; i++) { // TODO search backwards if cache == Rev
		if (fdt[i] == Wait || fdt[i] == Sock || fdt[i] == Punt || fdt[i] == None) continue;
		if (fstat(inp[i],&old) != 0) return -1;
		if (new.st_dev == old.st_dev && new.st_ino == old.st_ino) return i;}
	return -1;
}
struct sockaddr_in6 *scanInet6(struct sockaddr_in6 *in6, const char *adr, const char *num)
{
	struct sockaddr_in6 init = {0};
	memcpy(in6,&init,sizeof(init));
	in6->sin6_family = AF_INET6;
	if (adr == 0) in6->sin6_addr = in6addr_any;
	else inet_pton(AF_INET6, adr, &in6->sin6_addr);
	int port = 0;
	if (sscanf(num,"%d",&port) != 1) return 0;
	in6->sin6_port = htons(port);
	return in6;
}
int inetIdent(const char *adr, const char *num)
{
	struct sockaddr_in6 comp = {0};
	if (adr == 0) adr = "127.0.0.1";
	if (cache == Off || scanInet6(&comp,adr,num) == 0) return -1;
	for (int i = 0; i <= lim; i++)
	if (fdt[i] == Sock && memcmp(&addr[idnt[i]],&comp,sizeof(comp)) == 0)
	return i;
	return -1;
}
int openPipe()
{
	int fd[2] = {0};
	if (lim == NUMOPEN) return -1;
	if (pipe(fd) < 0) return -1;
	inp[lim] = fd[0];
	out[lim] = fd[1];
	fdt[lim] = Wait;
	pid[lim] = 0;
	rfn[lim] = 0;
	wfn[lim] = 0;
	return lim++;
}
int openFifo(const char *str)
{
	int fi = 0;
	int fo = 0;
	int idx = 0;
	if ((idx = findIdent(str)) != -1) return idx;
	if (lim == NUMOPEN) return -1;
	if ((mkfifo(str,0666) < 0) && errno != EEXIST) return -1;
	if ((fi = open(str,O_RDONLY | O_NONBLOCK)) < 0) return -1;
	if ((fo = open(str,O_WRONLY | O_NONBLOCK)) < 0) return -1;
	if (fcntl(fi,F_SETFL,0) < 0) return -1;
	if (fcntl(fo,F_SETFL,0) < 0) return -1;
	inp[lim] = fi;
	out[lim] = fo;
	fdt[lim] = Poll;
	pid[lim] = 0;
	rfn[lim] = 0;
	wfn[lim] = 0;
	return lim++;
}
int openFile(const char *str)
{
	int fd = 0;
	int idx = 0;
	if ((idx = findIdent(str)) != -1) return idx;
	if (lim == NUMOPEN) return -1;
	if ((fd = open(str,O_RDWR|O_CREAT,0666)) < 0) return -1;
	inp[lim] = fd;
	out[lim] = fd;
	fdt[lim] = Seek;
	pid[lim] = 0;
	rfn[lim] = 0;
	wfn[lim] = 0;
	return lim++;
}
int openInet(const char *adr, const char *num)
{
	int fd = 0;
	int idx = 0;
	if ((inetIdent(adr, num)) != -1) return idx;
	if (lim == NUMOPEN) return -1;
	if (adr == 0) {
	if ((fd = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP)) < 0) return -1;
	int flag = 1;
	if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &flag, sizeof(flag)) < 0) return -1;
	struct sockaddr_in6 adr = {0};
	if (scanInet6(&adr,0,num) == 0) return -1;
	if (bind(fd, (struct sockaddr*)&adr, sizeof(adr)) < 0) return -1;
	if (listen(fd, NUMPEND) < 0) return -1;
	fdt[lim] = Inet;
	pid[lim] = 0;
	rfn[lim] = 0;
	wfn[lim] = 0;} else {
	if (mad == NUMINET) return -1;
	if ((fd = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP)) < 0) return -1;
	if (scanInet6(&addr[mad],adr,num) == 0) return -1;
	if (connect(fd, (struct sockaddr*)&addr[mad], sizeof(addr[mad])) < 0) return -1;
	idnt[lim] = mad;
	fdt[lim] = Sock;
	pid[lim] = 0;
	rfn[lim] = 0;
	wfn[lim] = 0;
	mad++;}
	inp[lim] = fd;
	out[lim] = fd;
	return lim++;
}
int forkExec(const char *exe)
{
	int c2p[2], p2c[2], val;
	if (lim == NUMOPEN) return -1;
	val = pipe(c2p); if (val < 0) return -1;
	val = pipe(p2c); if (val < 0) return -1;
	pid[lim] = fork(); if (pid[lim] < 0) return -1;
	if (pid[lim] == 0) {
		char ist[33], ost[33], idt[33];
		val = close(c2p[0]); if (val < 0) return -1;
		val = close(p2c[1]); if (val < 0) return -1;
		val = snprintf(ost,32,"%d",c2p[1]); if (val < 0 || val > 32) return -1;
		val = snprintf(ist,32,"%d",p2c[0]); if (val < 0 || val > 32) return -1;
		val = snprintf(idt,32,"%d",lim); if (val < 0 || val > 32) return -1;
		val = execl(exe,exe,ist,ost,idt,0); if (val < 0) return -1;
		return -1;}
        if (pid[lim] == -1) return -1;
	val = close(c2p[1]); if (val < 0) return -1;
	val = close(p2c[0]); if (val < 0) return -1;
	inp[lim] = c2p[0];
	out[lim] = p2c[1];
	rfn[lim] = 0;
	wfn[lim] = 0;
	fdt[lim] = Wait;
	val = lim++;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) ERROR(exitErr,0)
	return val;
}
int pipeInit(const char *av1, const char *av2)
{
	int val, rfd, wfd;
	if (!av1 || !av2) return -1;
	val = sscanf(av1,"%d",&rfd); if (val != 1) return -1;
	val = sscanf(av2,"%d",&wfd); if (val != 1) return -1;
	// TODO here and below search backwards if cache == Rev
	// TODO allow unused descriptor to be negative incremented when skipped
	for (val = 0; val < lim; val++) if (fdt[val] == Wait &&
	(wfd == 0 || out[val] == 0 || out[val] == wfd) &&
	(rfd == 0 || inp[val] == 0 || inp[val] == rfd)) break;
	if (val < lim && rfd == 0) rfd = inp[val];
	if (val < lim && wfd == 0) wfd = out[val];
	if (val == lim && lim == NUMOPEN) return -1;
	if (val == lim) lim++;
	inp[val] = rfd;
	out[val] = wfd;
	fdt[val] = Wait;
	pid[val] = 0;
	rfn[val] = 0;
	wfn[val] = 0;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) ERROR(exitErr,0)
	return val;
}
int rdfdInit(int rdfd, int hint)
{
	return 0; // TODO
}
int wrfdInit(int wrfd, int hint)
{
	return 0; // TODO
}
int puntInit(int rfd, int wfd, pftype rpf, qftype wpf)
{
	int val;
	for (val = 0; val < lim; val++) if (fdt[val] == Punt &&
	(wfd == 0 || out[val] == 0 || out[val] == wfd) &&
	(rfd == 0 || inp[val] == 0 || inp[val] == rfd)) break;
	if (val < lim && rfd == 0) {rfd = inp[val]; rpf = rfn[val];}
	if (val < lim && wfd == 0) {wfd = out[val]; wpf = wfn[val];}
	if (val == lim && lim == NUMOPEN) return -1;
	if (val == lim) lim++;
	fdt[val] = Punt;
	inp[val] = rfd;
	out[val] = wfd;
	pid[val] = 0;
	rfn[val] = rpf;
	wfn[val] = wpf;
	return val;
}
int pselectAny(struct timespec *dly, int msk)
{
	while (1) {
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	for (int i = 0; i < lim; i++) if (((msk < 0) && (1<<i) == 0) || (msk & (1<<i))) {
		if (fdt[i] == Wait && nfd <= inp[i]) nfd = inp[i]+1;
		if (fdt[i] == Wait) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}
		if (fdt[i] == Sock && nfd <= inp[i]) nfd = inp[i]+1;
		if (fdt[i] == Sock) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}
		if (fdt[i] == Inet && nfd <= inp[i]) nfd = inp[i]+1;
		if (fdt[i] == Inet) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}}
	if (nfd == 0) return -1;
	val = -1; errno = EINTR;
	while (val < 0 && errno == EINTR) val = pselect(nfd,&fds,0,&ers,dly,0);
	if (val < 0) ERROR(exitErr,0);
	if (val == 0) return -1;
	nfd = 0; for (int i = 0; i < lim; i++) if (((msk < 0) && (1<<i) == 0) || (msk & (1<<i))) {
		if (fdt[i] == Wait && FD_ISSET(inp[i],&ers)) {closeIdent(i); nfd++;}
		if (fdt[i] == Sock && FD_ISSET(inp[i],&ers)) {closeIdent(i); nfd++;}
		if (fdt[i] == Inet && FD_ISSET(inp[i],&ers)) {closeIdent(i); nfd++;}}
	if (nfd == 0) for (int i = 0; i < lim; i++) if (((msk < 0) && (1<<i) == 0) || (msk & (1<<i))) {
		if (fdt[i] == Wait && FD_ISSET(inp[i],&fds)) return i;
		if (fdt[i] == Sock && FD_ISSET(inp[i],&fds)) return i;
		if (fdt[i] == Inet && FD_ISSET(inp[i],&fds)) {
		struct sockaddr_in6 adr = {0};
		socklen_t siz = sizeof(adr);
		if (siz >= NUMOPEN) {closeIdent(i); continue;}
		inp[lim] = out[lim] = accept(inp[i],(struct sockaddr*)&adr,&siz);
		if (inp[lim] < 0) continue;
		fdt[lim] = Wait;
		pid[lim] = 0;
		return lim++;}}
	} return -1;
}
int waitAny()
{
	return pselectAny(0,-1);
}
int waitMask(int msk)
{
	return pselectAny(0,msk);
}
int pauseMask(double dly, int msk)
{
	struct timespec delay = {0};
	if (dly == 0.0) return waitAny();
	delay.tv_sec = (long long)dly;
	delay.tv_nsec = (dly-(long long)dly)*SEC2NANO;
	return pselectAny(&delay,msk);
}
int pauseAny(double dly)
{
	return pauseMask(dly,-1);
}
void waitAll()
{
	int val;
	while (wait(&val) != -1);
	if (errno != ECHILD) ERROR(exitErr,0)
}
void *callCall(void *arg)
{
	int idx = (int)(size_t)arg;
	while (1) {
		int sub = pselectAny(0,idx);
		if (sub == idx) cbfnc[idx](idx);
	}
}
void callInit(cftype fnc, int idx)
{
	if (idx < 0 || idx >= lim || (fdt[idx] != Wait && fdt[idx] != Sock)) ERROR(exitErr,0);
	cbfnc[idx] = fnc;
	if (pthread_create(&cbpth[idx],0,callCall,(void*)(size_t)idx) != 0) ERROR(exitErr,0);
}
int pollPipe(int idx)
{
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	if (idx < 0 || idx >= lim || fdt[idx] != Poll) return 0;
	if (nfd <= inp[idx]) nfd = inp[idx]+1;
	FD_SET(inp[idx],&fds); FD_SET(inp[idx],&ers);
	val = -1; while (val < 0 && errno == EINTR) val = pselect(nfd,&fds,0,&ers,0,0);
	if (val <= 0) ERROR(inperr[idx],idx)
	if (FD_ISSET(inp[idx],&fds)) return 1;
	return 0;
}
int pollFile(int idx)
{
	off_t pos, siz;
	if (idx < 0 || idx >= lim || fdt[idx] != Seek) return 0;
	if ((pos = lseek(inp[idx],0,SEEK_CUR)) < 0) ERROR(inperr[idx],idx);
	if ((siz = lseek(inp[idx],0,SEEK_END)) < 0) ERROR(inperr[idx],idx);
	if (lseek(inp[idx],pos,SEEK_SET) < 0) ERROR(inperr[idx],idx);
	return (siz > pos);
}
void seekFile(long long arg, int idx)
{
	off_t pos = arg;
	if (lseek(inp[idx],pos,SEEK_SET) < 0) ERROR(inperr[idx],idx);
	if (lseek(out[idx],pos,SEEK_SET) < 0) ERROR(outerr[idx],idx);
}
void truncFile(int idx)
{
	seekFile(0,idx);
	if (ftruncate(inp[idx],0) < 0) ERROR(inperr[idx],idx);
	if (ftruncate(out[idx],0) < 0) ERROR(outerr[idx],idx);
}
long long checkFile(int idx)
{
	off_t pos, siz;
	if (idx < 0 || idx >= lim || fdt[idx] != Seek) return 0;
	if ((pos = lseek(inp[idx],0,SEEK_CUR)) < 0) ERROR(inperr[idx],idx);
	if ((siz = lseek(inp[idx],0,SEEK_END)) < 0) ERROR(inperr[idx],idx);
	if (lseek(inp[idx],pos,SEEK_CUR) < 0) ERROR(inperr[idx],idx);
	return siz;
}
int rdlkFile(long long loc, long long siz, int idx)
{
	struct flock lock = {0};
	lock.l_start = loc;
	lock.l_len = siz;
	lock.l_type = F_RDLCK;
	lock.l_whence = SEEK_SET;
	int val = 0;
	if ((val = fcntl(inp[idx],F_SETLK,&lock)) < 0 && errno != EAGAIN) ERROR(inperr[idx],idx);
	return (val==0);
}
int wrlkFile(long long loc, long long siz, int idx)
{
	struct flock lock = {0};
	lock.l_start = loc;
	lock.l_len = siz;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	int val = 0;
	if ((val = fcntl(inp[idx],F_SETLK,&lock)) < 0 && errno != EAGAIN) ERROR(inperr[idx],idx);
	return (val==0);
}
void unlkFile(long long loc, long long siz, int idx)
{
	struct flock lock = {0};
	lock.l_start = loc;
	lock.l_len = siz;
	lock.l_type = F_UNLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLK,&lock) < 0) ERROR(inperr[idx],idx);
}
void rdlkwFile(long long loc, long long siz, int idx)
{
	struct flock lock = {0};
	lock.l_start = loc;
	lock.l_len = siz;
	lock.l_type = F_RDLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLKW,&lock) < 0) ERROR(inperr[idx],idx);
}
void wrlkwFile(long long loc, long long siz, int idx)
{
	struct flock lock = {0};
	lock.l_start = loc;
	lock.l_len = siz;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLKW,&lock) < 0) ERROR(inperr[idx],idx);
}
int checkRead(int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) return 0;
	return 1;
}
int checkWrite(int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) return 0;
	return 1;
}
void sleepSec(double sec)
{
	struct timespec delay = {0};
	delay.tv_sec = (long long)sec;
	delay.tv_nsec = (sec-(long long)sec)*SEC2NANO;
	if (pselect(0,0,0,0,&delay,0) < 0 && errno != EINTR) ERROR(exitErr,0)
}
void allocMark()
{
	if (memmrks == memmrkz) memmrk = realloc(memmrk,(memmrkz+=bufsize)*sizeof(*memmrk));
	memmrk[memmrks++] = memptrs;
}
void allocKeep()
{
	if (memmrks == 0) ERROR(exitErr,-1)
	memmrks--;
}
void allocDrop()
{
	if (memmrks == 0) ERROR(exitErr,-1)
	while (memptrs > memmrk[memmrks-1]) {
		free(*memptr[memptrs]);
		*memptr[memptrs] = 0;
		memptrs--;}
	memmrks--;
}
void allocMem(void **ptr, int siz)
{
	if (memptrs == memptrz) memptr = realloc(memptr,(memptrz+=bufsize)*sizeof(*memptr));
	memptr[memptrs] = ptr;
	*ptr = realloc(*ptr,siz);
}
void allocChr(char **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	allocMem((void**)ptr,siz*sizeof(char));
	if (*ptr == 0) ERROR(exitErr,-1)
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocInt(int **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	allocMem((void**)ptr,siz*sizeof(int));
	if (*ptr == 0) ERROR(exitErr,-1)
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocNew(long long **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	allocMem((void**)ptr,siz*sizeof(long long));
	if (*ptr == 0) ERROR(exitErr,-1)
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocNum(double **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	allocMem((void**)ptr,siz*sizeof(double));
	if (*ptr == 0) ERROR(exitErr,-1)
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocOld(float **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	allocMem((void**)ptr,siz*sizeof(float));
	if (*ptr == 0) ERROR(exitErr,-1)
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocStr(char* **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	allocMem((void**)ptr,siz*sizeof(char*));
	if (*ptr == 0) ERROR(exitErr,-1)
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void assignStr(char **ptr, const char *str)
{
	if (*ptr && str == 0) {free(*ptr); *ptr = 0;}
	if (str == 0) return;
	allocMem((void**)ptr,strlen(str)+1);
	if (*ptr == 0) ERROR(exitErr,-1)
	strcpy(*ptr,str);
}
void callStr(const char *str, int trm, int idx, void *arg)
{
	char **ptr = arg;
	if (trm == 0) NOTICE(inpexc[idx],idx)
	assignStr(ptr,str);
}
void textStr(const char *str, int trm, int idx, void *arg)
{
	struct Text *text = arg;
	text->trm = trm;
	assignStr(text->str,str);
}
void readStr(sftype fnc, void *arg, int idx)
{
	char *buf = 0;
	int size = 0; // num valid
	ssize_t val = 1/*bufsize*/; // num read
	int num = 1/*bufsize*/; // num nonzero
	int trm = 0; // num zero
	if (idx < 0 || idx >= lim || fdt[idx] == None/*!= Seek*/) ERROR(exitErr,0)
	while (num == 1/*bufsize*/ && val == 1/*bufsize*/) {
		if ((size % bufsize) == 0) buf = realloc(buf,size+bufsize+1);
		if (buf == 0) ERROR(outerr[idx],idx)
		if (fdt[idx] == Punt) val = rfn[idx](inp[idx],buf+size,1);
		else val = /*p*/read(inp[idx],buf+size,1/*bufsize,loc+size*/);
		if (val < 0) ERROR(outerr[idx],idx)
		for (num = 0; num != val && buf[size+num]; num++);
		size += num;
	}
	if (val == num) buf[size] = 0; else trm = 1;
	fnc(buf,trm,idx,arg);
	free(buf);
}
void preadStr(sftype fnc, void *arg, long long loc, int idx)
{
	char *buf = 0;
	int size = 0; // num valid
	ssize_t val = bufsize; // num read
	int num = bufsize; // num nonzero
	int trm = 0; // num zero
	if (idx < 0 || idx >= lim || fdt[idx] != Seek) ERROR(exitErr,0)
	while (num == bufsize && val == bufsize) {
		/*if ((size % bufsize) == 0) */buf = realloc(buf,size+bufsize+1);
		if (buf == 0) ERROR(outerr[idx],idx)
		val = pread(inp[idx],buf+size,bufsize,loc+size);
		if (val < 0) ERROR(outerr[idx],idx)
		for (num = 0; num != val && buf[size+num]; num++);
		size += num;
	}
	if (val == num) buf[size] = 0; else trm = 1;
	fnc(buf,trm,idx,arg);
	free(buf);
}
void readStrHsFnc(const char *buf, int trm, int idx, void *arg)
{
	hftype fnc = arg;
	fnc(buf,trm);
}
void readStrHs(hftype fnc, int idx)
{
	readStr(readStrHsFnc,fnc,idx);
}
void readEof(int idx)
{
	char arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)	int val = 0;
	if (fdt[idx] == Punt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(char));
	else val = read(inp[idx],(char *)&arg,sizeof(char));
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val != 0) NOTICE(inpexc[idx],idx)
}
char readChr(int idx)
{
	char arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int val = 0;
	if (fdt[idx] == Punt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(char));
	else val = read(inp[idx],(char *)&arg,sizeof(char));
	if (val != 0 && val < (int)sizeof(char)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(inpexc[idx],idx)}
	return arg;
}
int readInt(int idx)
{
	int arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int val = 0;
	if (fdt[idx] == Punt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(int));
	else val = read(inp[idx],(char *)&arg,sizeof(int));
	if (val != 0 && val < (int)sizeof(int)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(inpexc[idx],idx)}
	return arg;
}
double readNum(int idx)
{
	double arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int val = 0;
	if (fdt[idx] == Punt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(double));
	else val = read(inp[idx],(char *)&arg,sizeof(double));
	if (val != 0 && val < (int)sizeof(double)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0.0; NOTICE(inpexc[idx],idx)}
	return arg;
}
long long readNew(int idx)
{
	long long arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	if (inp[idx] < 0) {arg = 0; return arg;}
	int val = 0;
	if (fdt[idx] == Punt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(long long));
	else val = read(inp[idx],(char *)&arg,sizeof(long long));
	if (val != 0 && val < (int)sizeof(long long)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(inpexc[idx],idx)}
	return arg;
}
float readOld(int idx)
{
	float arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	if (inp[idx] < 0) {arg = 0.0; return arg;}
	int val = 0;
	if (fdt[idx] == Punt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(float));
	else val = read(inp[idx],(char *)&arg,sizeof(float));
	if (val != 0 && val < (int)sizeof(float)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0.0; NOTICE(inpexc[idx],idx)}
	return arg;
}
int writeBuf(const void *arg, long long siz, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	if (fdt[idx] == Poll) {
		while (atoms[idx]+siz > atomz[idx]) atom[idx] = realloc(atom[idx],atomz[idx]+=bufsize);
		if (atom[idx] == 0) ERROR(outerr[idx],idx)
		memcpy(atom[idx]+atoms[idx],arg,siz);
		atoms[idx] += siz;
		return siz;}
	if (fdt[idx] == Punt) return wfn[idx](out[idx],arg,siz);
	return write(out[idx],arg,siz);
}
void flushBuf(int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] != Poll) ERROR(exitErr,0)
	if (write(out[idx],atom[idx],atoms[idx]) < 0) ERROR(outerr[idx],idx)
	atoms[idx] = 0;
}
void writeStr(const char *arg, int trm, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int siz = strlen(arg)+trm;
	int val = writeBuf(/*write(out[idx],*/arg,siz,idx);
	if (val < siz) ERROR(outerr[idx],idx)
}
void pwriteStr(const char *arg, int trm, long long loc, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] != Seek) ERROR(exitErr,0)
	int siz = strlen(arg)+trm;
	int val = pwrite(out[idx],arg,siz,loc);
	if (val < siz) ERROR(outerr[idx],idx)
}
void writeChr(char arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(char), idx);
	if (val < (int)sizeof(char)) ERROR(outerr[idx],idx)
}
void writeInt(int arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(int), idx);
	if (val < (int)sizeof(int)) ERROR(outerr[idx],idx)
}
void writeNum(double arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(double), idx);
	if (val < (int)sizeof(double)) ERROR(outerr[idx],idx)
}
void writeNew(long long arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(long long), idx);
	if (val < (int)sizeof(long long)) ERROR(outerr[idx],idx)
}
void writeOld(float arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERROR(exitErr,0)
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(float), idx);
	if (val < (int)sizeof(float)) ERROR(outerr[idx],idx)
}
void showEnum(const char *typ, const char* val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"%s(%s)",typ,val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showStruct(const char* bef, int val, const char *aft, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"%s%d%s",bef,val,aft) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showField(const char* val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"%s:",val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showOpen(const char* val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"%s(",val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showClose(char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,")") < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showChr(char val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Chr(%c)",val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showInt(int val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Int(%d)",val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showNew(long long val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"New(%lld)",val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showNum(double val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Num(%lf)",val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showOld(float val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Old(%f)",val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showStr(const char* val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Str(%s)",val) < 0) ERROR(exitErr,-1)
	num = strlen(tmp);
	allocMem((void**)str,*siz+num+1);
	if (*str == 0) ERROR(exitErr,-1)
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
int hideIdent(const char *val, const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," %s %%n",val) < 0) ERROR(exitErr,-1)
	sscanf(str+*siz,tmp,&num);
	free(tmp);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideEnum(const char* typ, const char *val, const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," %s ( %s ) %%n",typ,val) < 0) ERROR(exitErr,-1)
	sscanf(str+*siz,tmp,&num);
	free(tmp);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideStruct(const char* bef, int val, const char *aft, const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," %s %d %s%%n",bef,val,aft) < 0) ERROR(exitErr,-1)
	sscanf(str+*siz,tmp,&num);
	free(tmp);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideField(const char *val, const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," %s : %%n",val) < 0) ERROR(exitErr,-1)
	sscanf(str+*siz,tmp,&num);
	free(tmp);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideOpen(const char *val, const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," %s ( %%n",val) < 0) ERROR(exitErr,-1)
	sscanf(str+*siz,tmp,&num);
	free(tmp);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideClose(const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," ) %%n") < 0) ERROR(exitErr,-1)
	sscanf(str+*siz,tmp,&num);
	free(tmp);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideChr(char *val, const char *str, int *siz)
{
	int num = -1;
	sscanf(str+*siz," Chr ( %c )%n",val,&num);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideInt(int *val, const char *str, int *siz)
{
	int num = -1;
	sscanf(str+*siz," Int ( %d )%n",val,&num);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideNew(long long *val, const char *str, int *siz)
{
	int num = -1;
	sscanf(str+*siz," New ( %lld )%n",val,&num);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideNum(double *val, const char *str, int *siz)
{
	int num = -1;
	sscanf(str+*siz," Num ( %lf )%n",val,&num);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideOld(float *val, const char *str, int *siz)
{
	int num = -1;
	sscanf(str+*siz," Old ( %f )%n",val,&num);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideStr(char **val, const char *str, int *siz)
{
	char *tmp = 0;
	int base = -1;
	int num = -1;
	int limit = -1;
	sscanf(str+*siz," Str ( %n",&base);
	limit = base; while (base != -1 && str[limit] && num == -1) sscanf(str+*siz+(++limit)," )%n",&num);
	if (num == -1) return 0;
	tmp = malloc(limit-base+1);
	if (tmp == 0) return 0;
	strncpy(tmp,str+*siz+base,limit-base); tmp[limit-base] = 0;
	assignStr(val,tmp);
	free(tmp);
	*siz += limit+num;
	return 1;
}

void setupLua(char **mem, const char *str, int idx)
{
	if (mem[idx]) free(mem[idx]);
	mem[idx] = malloc(strlen(str)+1);
	if (mem[idx] == 0) ERROR(exitErr,0)
	strcpy(mem[idx],str);
}
void callLua(lua_State *lua, const char *fnc, const char *str, int num, int idx)
{
	if (lua == 0) ERROR(exitErr,0)
	lua_getglobal(lua,fnc);
	lua_pushstring(lua,str);
	lua_pushnumber(lua,num);
	lua_pushnumber(lua,idx);
	if (lua_pcall(lua, 3, 0, 0) != 0) ERROR(exitErr,0)
}
void noteLua(const char *str, int num, int idx)
{
	callLua(luaerr,exclua[idx],str,num,idx);
}
void readLua(const char *str, int num, int idx)
{
	callLua(luaerr,inplua[idx],str,num,idx);
}
void writeLua(const char *str, int num, int idx)
{
	callLua(luaerr,outlua[idx],str,num,idx);
}
void funcLua(int idx)
{
	lua_State *lua = luaerr;
	const char *fnc = fnclua[idx];
	lua_getglobal(lua,fnc);
	lua_pushnumber(lua,idx);
	if (lua_pcall(lua, 1, 0, 0) != 0) ERROR(exitErr,0)
}
int debugStrLua(lua_State *lua)
{
	luaerr = lua;
	debugStr(lua_tostring(lua,1));
	return 0;
}
int readNoteLua(lua_State *lua)
{
	luaerr = lua;
	setupLua(exclua,lua_tostring(lua,1),(int)lua_tonumber(lua,2));
	readNote(noteLua,(int)lua_tonumber(lua,2));
	return 0;
}
int readJumpLua(lua_State *lua)
{
	luaerr = lua;
	setupLua(inplua,lua_tostring(lua,1),(int)lua_tonumber(lua,2));
	readJump(readLua,(int)lua_tonumber(lua,2));
	return 0;
}
int writeJumpLua(lua_State *lua)
{
	luaerr = lua;
	setupLua(outlua,lua_tostring(lua,1),(int)lua_tonumber(lua,2));
	writeJump(writeLua,(int)lua_tonumber(lua,2));
	return 0;
}
int closeIdentLua(lua_State *lua)
{
	luaerr = lua;
	closeIdent((int)lua_tonumber(lua,1));
	return 0;
}
int moveIdentLua(lua_State *lua)
{
	luaerr = lua;
	moveIdent((int)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
int openPipeLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,openPipe());
	return 1;
}
int openFifoLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,openFifo(lua_tostring(lua,1)));
	return 1;
}
int openFileLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,openFile(lua_tostring(lua,1)));
	return 1;
}
int openInetLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,openInet(lua_tostring(lua,1),lua_tostring(lua,2)));
	return 1;
}
int forkExecLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,forkExec(lua_tostring(lua,1)));
	return 1;
}
int pipeInitLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,pipeInit(lua_tostring(lua,1),lua_tostring(lua,2)));
	return 1;
}
int waitAnyLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,waitAny());
	return 1;
}
int pauseAnyLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,pauseAny(lua_tonumber(lua,1)));
	return 1;
}
int waitAllLua(lua_State *lua)
{
	luaerr = lua;
	waitAll();
	return 0;
}
int callInitLua(lua_State *lua)
{
	luaerr = lua;
	setupLua(fnclua,lua_tostring(lua,1),(int)lua_tonumber(lua,2));
	callInit(funcLua,(int)lua_tonumber(lua,2));
	return 0;
}
int pollPipeLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,pollPipe((int)lua_tonumber(lua,1)));
	return 1;
}
int pollFileLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,pollFile((int)lua_tonumber(lua,1)));
	return 1;
}
int seekFileLua(lua_State *lua)
{
	luaerr = lua;
	seekFile((long long)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
int truncFileLua(lua_State *lua)
{
	luaerr = lua;
	truncFile((int)lua_tonumber(lua,1));
	return 0;
}
int checkFileLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,checkFile((int)lua_tonumber(lua,1)));
	return 1;
}
int inetIdentLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,inetIdent(lua_tostring(lua,1),lua_tostring(lua,2)));
	return 1;
}
int rdlkFileLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,rdlkFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3)));
	return 1;
}
int wrlkFileLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,wrlkFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3)));
	return 1;
}
int unlkFileLua(lua_State *lua)
{
	luaerr = lua;
	unlkFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
int rdlkwFileLua(lua_State *lua)
{
	luaerr = lua;
	rdlkwFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
int wrlkwFileLua(lua_State *lua)
{
	luaerr = lua;
	wrlkwFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
int checkReadLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,checkRead((int)lua_tonumber(lua,1)));
	return 1;
}
int checkWriteLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,checkWrite((int)lua_tonumber(lua,1)));
	return 1;
}
int sleepSecLua(lua_State *lua)
{
	luaerr = lua;
	sleep((int)lua_tonumber(lua,1));
	return 0;
}
void readStrLuaFnc(const char *buf, int trm, int idx, void *arg)
{
	lua_State *lua = arg;
	lua_pushstring(lua,buf);
	lua_pushnumber(lua,trm);
}
int readStrLua(lua_State *lua)
{
	luaerr = lua;
	readStr(readStrLuaFnc,lua,(int)lua_tonumber(lua,1));
	return 2;
}
int readEofLua(lua_State *lua)
{
	luaerr = lua;
	readEof((int)lua_tonumber(lua,1));
	return 0;
}
int readChrLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readChr((int)lua_tonumber(lua,1)));
	return 1;
}
int readIntLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readInt((int)lua_tonumber(lua,1)));
	return 1;
}
int readNumLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readNum((int)lua_tonumber(lua,1)));
	return 1;
}
int readNewLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readNew((int)lua_tonumber(lua,1)));
	return 1;
}
int readOldLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,(double)readOld((int)lua_tonumber(lua,1)));
	return 1;
}
int writeStrLua(lua_State *lua)
{
	luaerr = lua;
	writeStr(lua_tostring(lua,1),(int)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
int writeChrLua(lua_State *lua)
{
	luaerr = lua;
	writeChr((char)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
int writeIntLua(lua_State *lua)
{
	luaerr = lua;
	writeInt((int)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
int writeNumLua(lua_State *lua)
{
	luaerr = lua;
	writeNum((double)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
int writeNewLua(lua_State *lua)
{
	luaerr = lua;
	writeNew((long long)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
int writeOldLua(lua_State *lua)
{
	luaerr = lua;
	writeOld((float)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}

int luaopen_face (lua_State *L)
{
	lua_pushcfunction(L, debugStrLua);
	lua_setglobal(L, "debugStr");
	lua_pushcfunction(L, readNoteLua);
	lua_setglobal(L, "readNote");
	lua_pushcfunction(L, readJumpLua);
	lua_setglobal(L, "readJump");
	lua_pushcfunction(L, writeJumpLua);
	lua_setglobal(L, "writeJump");
	lua_pushcfunction(L, closeIdentLua);
	lua_setglobal(L, "closeIdent");
	lua_pushcfunction(L, moveIdentLua);
	lua_setglobal(L, "moveIdent");
	lua_pushcfunction(L, openPipeLua);
	lua_setglobal(L, "openPipe");
	lua_pushcfunction(L, openFifoLua);
	lua_setglobal(L, "openFifo");
	lua_pushcfunction(L, openFileLua);
	lua_setglobal(L, "openFile");
	lua_pushcfunction(L, forkExecLua);
	lua_setglobal(L, "forkExec");
	lua_pushcfunction(L, pipeInitLua);
	lua_setglobal(L, "pipeInit");
	lua_pushcfunction(L, waitAnyLua);
	lua_setglobal(L, "waitAny");
	lua_pushcfunction(L, waitAllLua);
	lua_setglobal(L, "waitAll");
	lua_pushcfunction(L, callInitLua);
	lua_setglobal(L, "callInit");
	lua_pushcfunction(L, pollPipeLua);
	lua_setglobal(L, "pollPipe");
	lua_pushcfunction(L, pollFileLua);
	lua_setglobal(L, "pollFile");
	lua_pushcfunction(L, seekFileLua);
	lua_setglobal(L, "seekFile");
	lua_pushcfunction(L, truncFileLua);
	lua_setglobal(L, "truncFile");
	lua_pushcfunction(L, checkFileLua);
	lua_setglobal(L, "checkFile");
	lua_pushcfunction(L, rdlkFileLua);
	lua_setglobal(L, "rdlkFile");
	lua_pushcfunction(L, wrlkFileLua);
	lua_setglobal(L, "wrlkFile");
	lua_pushcfunction(L, unlkFileLua);
	lua_setglobal(L, "unlkFile");
	lua_pushcfunction(L, rdlkwFileLua);
	lua_setglobal(L, "rdlkwFile");
	lua_pushcfunction(L, wrlkwFileLua);
	lua_setglobal(L, "wrlkwFile");
	lua_pushcfunction(L, checkReadLua);
	lua_setglobal(L, "checkRead");
	lua_pushcfunction(L, checkWriteLua);
	lua_setglobal(L, "checkWrite");
	lua_pushcfunction(L, sleepSecLua);
	lua_setglobal(L, "sleepSec");
	lua_pushcfunction(L, readStrLua);
	lua_setglobal(L, "readStr");
	lua_pushcfunction(L, readEofLua);
	lua_setglobal(L, "readEof");
	lua_pushcfunction(L, readChrLua);
	lua_setglobal(L, "readChr");
	lua_pushcfunction(L, readIntLua);
	lua_setglobal(L, "readInt");
	lua_pushcfunction(L, readNumLua);
	lua_setglobal(L, "readNum");
	lua_pushcfunction(L, readNewLua);
	lua_setglobal(L, "readNew");
	lua_pushcfunction(L, readOldLua);
	lua_setglobal(L, "readOld");
	lua_pushcfunction(L, writeStrLua);
	lua_setglobal(L, "writeStr");
	lua_pushcfunction(L, writeChrLua);
	lua_setglobal(L, "writeChr");
	lua_pushcfunction(L, writeIntLua);
	lua_setglobal(L, "writeInt");
	lua_pushcfunction(L, writeNumLua);
	lua_setglobal(L, "writeNum");
	lua_pushcfunction(L, writeNewLua);
	lua_setglobal(L, "writeNew");
	lua_pushcfunction(L, writeOldLua);
	lua_setglobal(L, "writeOld");
	return 0;
}
