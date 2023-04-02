#include "face.h"
#include "luax.h"
#include <stdlib.h>
#include <unistd.h>
#include <sys/errno.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <pthread.h>
#include <signal.h>
#include <fcntl.h>
#include <ctype.h>
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
	Bunt, // buffered stream function
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
int bufsize = BUFSIZE;

// double callback buffer
char *prep = 0;

// server address for checking if already connected
struct sockaddr_in6 addr[NUMINET] = {0};
int mad = 0;
int idnt[NUMOPEN] = {0};

// thread to call function when pipe is readable
pthread_t cbpth[NUMOPEN] = {0};
cftype cbfnc[NUMOPEN] = {0};

// read string tokenization
fgtype termfn = 0;
void *usr[NUMOPEN] = {0};

// error handling
chtype intrfn = 0;
cftype notice = 0;
eftype errfnc = 0;
char *luanote = 0;
char *luafunc = 0;
lua_State *luaerr = 0;
#define ERRFNC(IDX) {if (errfnc) errfnc(__FILE__,__LINE__,IDX); else ERROR();}
#define NOTICE(IDX) {if (notice) notice(IDX); else ERRFNC(IDX);}
#define INTRFN() {if (intrfn) intrfn();}

void termFunc(fgtype fnc)
{
	termfn = fnc;
	// >0 valid, 0 not enough, -1 too much, -2 invalid
}
void intrFunc(chtype fnc)
{
	intrfn = fnc;
}
void noteFunc(cftype fnc)
{
	notice = fnc;
}
void errFunc(eftype fnc)
{
	errfnc = fnc;
}
void callIntr()
{
	INTRFN();
}
void callNote(int idx)
{
	NOTICE(idx);
}
void callErr(int idx)
{
	ERRFNC(idx);
}
void closeIdent(int idx)
{
	if (idx < 0 || idx >= lim) ERRFNC(idx);
	if (fdt[idx] != None) {
		close(inp[idx]);
		if (inp[idx] != out[idx]) close(out[idx]);
		fdt[idx] = None;}
	while (lim > 0 && fdt[lim-1] == None) lim--;
}
void moveIdent(int idx0, int idx1)
{
	if (idx1 < 0 || idx1 >= lim) ERRFNC(idx0);
	if (idx0 < 0 || idx0 >= lim) ERRFNC(idx1);
	closeIdent(idx1);
	inp[idx1] = inp[idx0];
	out[idx1] = out[idx0];
	fdt[idx1] = fdt[idx0];
	pid[idx1] = pid[idx0];
	rfn[idx1] = rfn[idx0];
	wfn[idx1] = wfn[idx0];
	closeIdent(idx0);
}
int findIdent(const char *str)
{
	struct stat old;
	struct stat new;
	if (stat(str,&new) != 0) return -1;
	for (int i = 0; i < lim; i++) {
		if (fdt[i] == Wait || fdt[i] == Sock || fdt[i] == Punt || fdt[i] == Bunt || fdt[i] == None) continue;
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
	if (scanInet6(&comp,adr,num) == 0) return -1;
	for (int i = 0; i <= lim; i++)
	if (fdt[i] == Sock && memcmp(&addr[idnt[i]],&comp,sizeof(comp)) == 0)
	return i;
	return -1;
}
void **userIdent(int idx)
{
	if (idx < 0 || idx >= lim) ERRFNC(idx);
	return &usr[idx];
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
int openSide()
{
	int fd[2] = {0};
	if (lim == NUMOPEN) return -1;
	if (pipe(fd) < 0) return -1;
	inp[lim] = fd[0];
	out[lim] = fd[1];
	fdt[lim] = Poll;
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
int openName(const char *str)
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
	fdt[lim] = Wait;
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
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) ERRFNC(lim);
	return val;
}
int openFork(const char *exe, cgtype fnc)
{
	int c2p[2], p2c[2], val;
	if (lim == NUMOPEN) return -1;
	val = pipe(c2p); if (val < 0) return -1;
	val = pipe(p2c); if (val < 0) return -1;
	pid[lim] = fork(); if (pid[lim] < 0) return -1;
	if (pid[lim] == 0) {
		val = close(c2p[0]); if (val < 0) return -1;
		val = close(p2c[1]); if (val < 0) return -1;
		fnc(p2c[0],c2p[1]);
		val = execl(exe,exe,prep,0); if (val < 0) return -1;
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
	sig_t tmp = signal(SIGPIPE,SIG_IGN); if (tmp == SIG_ERR) ERRFNC(lim);
	return val;
}
void openExec(const char *pre)
{
	if (prep) {free(prep); prep = 0;}
	prep = strdup(pre);
}
int pipeInit(const char *av1, const char *av2)
{
	int val, rfd, wfd;
	if (!av1 || !av2) return -1;
	val = sscanf(av1,"%d",&rfd); if (val != 1) return -1;
	val = sscanf(av2,"%d",&wfd); if (val != 1) return -1;
	if (lim == NUMOPEN) return -1;
	inp[lim] = rfd;
	out[lim] = wfd;
	fdt[lim] = Wait;
	pid[lim] = 0;
	rfn[lim] = 0;
	wfn[lim] = 0;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) ERRFNC(lim);
	return lim++;
}
int rdfdInit(int rdfd, int hint)
{
	for (int i = 0; i < lim; i++)
	if (fdt[i] == Wait && inp[i] == 0 && out[i] == hint) {
	inp[i] = rdfd; return i;}
	if (lim == NUMOPEN) return -1;
	inp[lim] = rdfd;
	out[lim] = 0;
	fdt[lim] = Wait;
	pid[lim] = 0;
	rfn[lim] = 0;
	wfn[lim] = 0;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) ERRFNC(lim);
	return lim++;
}
int wrfdInit(int wrfd, int hint)
{
	for (int i = 0; i < lim; i++)
	if (fdt[i] == Wait && inp[i] == hint && out[i] == 0) {
	out[i] = wrfd; return i;}
	if (lim == NUMOPEN) return -1;
	inp[lim] = 0;
	out[lim] = wrfd;
	fdt[lim] = Wait;
	pid[lim] = 0;
	rfn[lim] = 0;
	wfn[lim] = 0;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) ERRFNC(lim);
	return lim++;
}
int puntInit(int rfd, int wfd, pftype rpf, qftype wpf)
{
	int val;
	for (val = 0; val < lim; val++) if (fdt[val] == Punt &&
	(wpf == 0 || wfn[val] == 0 || wfn[val] == wpf) &&
	(rpf == 0 || rfn[val] == 0 || rfn[val] == rpf)) break;
	if (val < lim && rpf == 0) {rpf = rfn[val];}
	if (val < lim && wpf == 0) {wpf = wfn[val];}
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
int buntInit(int rfd, int wfd, pftype rpf, qftype wpf)
{
	int val;
	for (val = 0; val < lim; val++) if (fdt[val] == Bunt &&
	(wpf == 0 || wfn[val] == 0 || wfn[val] == wpf) &&
	(rpf == 0 || rfn[val] == 0 || rfn[val] == rpf)) break;
	if (val < lim && rpf == 0) {rpf = rfn[val];}
	if (val < lim && wpf == 0) {wpf = wfn[val];}
	if (val == lim && lim == NUMOPEN) return -1;
	if (val == lim) lim++;
	fdt[val] = Bunt;
	inp[val] = rfd;
	out[val] = wfd;
	pid[val] = 0;
	rfn[val] = rpf;
	wfn[val] = wpf;
	return val;
}
int waitRead(double dly, int msk)
{
	struct timespec delay = {0};
	struct timespec *ptr = &delay;
	delay.tv_sec = (long long)dly;
	delay.tv_nsec = (dly-(long long)dly)*SEC2NANO;
	if (dly == 0.0) ptr = 0;
	if (dly < 0.0) {delay.tv_sec = 0; delay.tv_nsec = 0;}
	int val = 0;
	int nfd = 0;
	fd_set fds, ers;
	while (1) {
	FD_ZERO(&fds); FD_ZERO(&ers); nfd = 0;
	for (int i = 0; i < lim; i++) if (((msk < 0) && (1<<i) == 0) || (msk & (1<<i))) {
		if (fdt[i] == Wait && nfd <= inp[i]) nfd = inp[i]+1;
		if (fdt[i] == Wait) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}
		if (fdt[i] == Sock && nfd <= inp[i]) nfd = inp[i]+1;
		if (fdt[i] == Sock) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}
		if (fdt[i] == Inet && nfd <= inp[i]) nfd = inp[i]+1;
		if (fdt[i] == Inet) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}}
	if (nfd == 0) return -1;
	val = pselect(nfd,&fds,0,&ers,ptr,0);
	if (val < 0 && errno == EINTR) {INTRFN() continue;}
	if (val < 0 && errno == EBADF) return -1;
	if (val == 0) return -1;
	if (val < 0) ERRFNC(-1);
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
int waitExit()
{
	while (1) {
	int val = 0;
	int ret = wait(&val);
	if (ret < 0 && errno == ECHILD) return 0;
	if (ret < 0) return -1;
	if (!WIFEXITED(val)) return -1;
	if (WEXITSTATUS(val) != 0) return -1;}
	return -1;
}
void *callCall(void *arg)
{
	int idx = (int)(size_t)arg;
	while (1) {
		int sub = waitRead(0.0,idx);
		if (sub == idx) cbfnc[idx](idx);
	}
}
void callInit(cftype fnc, int idx)
{
	if (idx < 0 || idx >= lim || (fdt[idx] != Wait && fdt[idx] != Sock)) ERRFNC(idx);
	cbfnc[idx] = fnc;
	if (pthread_create(&cbpth[idx],0,callCall,(void*)(size_t)idx) != 0) ERRFNC(idx);
}
int pollPipe(int idx)
{
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	if (idx < 0 || idx >= lim || fdt[idx] != Poll) return 0;
	if (nfd <= inp[idx]) nfd = inp[idx]+1;
	FD_SET(inp[idx],&fds); FD_SET(inp[idx],&ers);
	val = -1; errno = EINTR; while (val < 0 && errno == EINTR) val = pselect(nfd,&fds,0,&ers,0,0);
	if (val <= 0) ERRFNC(idx);
	if (FD_ISSET(inp[idx],&fds)) return 1;
	return 0;
}
int pollFile(int idx)
{
	off_t pos, siz;
	if (idx < 0 || idx >= lim || fdt[idx] != Seek) return 0;
	if ((pos = lseek(inp[idx],0,SEEK_CUR)) < 0) ERRFNC(idx);
	if ((siz = lseek(inp[idx],0,SEEK_END)) < 0) ERRFNC(idx);
	if (lseek(inp[idx],pos,SEEK_SET) < 0) ERRFNC(idx);
	return (siz > pos);
}
void seekFile(long long arg, int idx)
{
	off_t pos = arg;
	if (lseek(inp[idx],pos,SEEK_SET) < 0) ERRFNC(idx);
	if (lseek(out[idx],pos,SEEK_SET) < 0) ERRFNC(idx);
}
void truncFile(int idx)
{
	seekFile(0,idx);
	if (ftruncate(inp[idx],0) < 0) ERRFNC(idx);
	if (ftruncate(out[idx],0) < 0) ERRFNC(idx);
}
long long checkFile(int idx)
{
	off_t pos, siz;
	if (idx < 0 || idx >= lim || fdt[idx] != Seek) return 0;
	if ((pos = lseek(inp[idx],0,SEEK_CUR)) < 0) ERRFNC(idx);
	if ((siz = lseek(inp[idx],0,SEEK_END)) < 0) ERRFNC(idx);
	if (lseek(inp[idx],pos,SEEK_SET) < 0) ERRFNC(idx);
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
	if ((val = fcntl(inp[idx],F_SETLK,&lock)) < 0 && errno != EAGAIN) ERRFNC(idx);
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
	if ((val = fcntl(inp[idx],F_SETLK,&lock)) < 0 && errno != EAGAIN) ERRFNC(idx);
	return (val==0);
}
void unlkFile(long long loc, long long siz, int idx)
{
	struct flock lock = {0};
	lock.l_start = loc;
	lock.l_len = siz;
	lock.l_type = F_UNLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLK,&lock) < 0) ERRFNC(idx);
}
void rdlkwFile(long long loc, long long siz, int idx)
{
	struct flock lock = {0};
	lock.l_start = loc;
	lock.l_len = siz;
	lock.l_type = F_RDLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLKW,&lock) < 0) ERRFNC(idx);
}
void wrlkwFile(long long loc, long long siz, int idx)
{
	struct flock lock = {0};
	lock.l_start = loc;
	lock.l_len = siz;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLKW,&lock) < 0) ERRFNC(idx);
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
	if (pselect(0,0,0,0,&delay,0) < 0 && errno != EINTR) ERRFNC(-1);
}
void allocChr(char **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	*ptr = malloc(siz*sizeof(char));
	if (*ptr == 0) ERRFNC(-1);
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocInt(int **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	*ptr = malloc(siz*sizeof(int));
	if (*ptr == 0) ERRFNC(-1);
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void resizeInt(int **ptr, int sav, int siz)
{
	int *tmp = *ptr; allocInt(&tmp,siz);
	for (int i = 0; i < sav && i < siz; i++) tmp[i] = (*ptr)[i];
	allocInt(ptr,0); *ptr = tmp;
}
void appendInt(int **ptr, int val, int *siz)
{
	resizeInt(ptr,(*siz),(*siz)+1);
	(*ptr)[(*siz)++] = val;
}
void allocInt32(int32_t **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	*ptr = malloc(siz*sizeof(int32_t));
	if (*ptr == 0) ERRFNC(-1);
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocNew(long long **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	*ptr = malloc(siz*sizeof(long long));
	if (*ptr == 0) ERRFNC(-1);
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocNum(double **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	*ptr = malloc(siz*sizeof(double));
	if (*ptr == 0) ERRFNC(-1);
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocOld(float **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	*ptr = malloc(siz*sizeof(float));
	if (*ptr == 0) ERRFNC(-1);
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocStr(char* **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	*ptr = malloc(siz*sizeof(char*));
	if (*ptr == 0) ERRFNC(-1);
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void assignStr(char **ptr, const char *str)
{
	if (*ptr && str == 0) {free(*ptr); *ptr = 0;}
	if (str == 0) return;
	*ptr = malloc(strlen(str)+1);
	if (*ptr == 0) ERRFNC(-1);
	strcpy(*ptr,str);
}
void allocDat(void* **ptr, int siz)
{
	if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
	if (siz == 0) return;
	*ptr = malloc(siz*sizeof(void *));
	if (*ptr == 0) ERRFNC(-1);
	for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void assignDat(void **ptr, const void *dat)
{
	if (*ptr && dat == 0) {free(*ptr); *ptr = 0;}
	if (dat == 0) return;
	*ptr = malloc((*(int*)dat)+sizeof(int));
	if (*ptr == 0) ERRFNC(-1);
	memcpy(*ptr,dat,(*(int*)dat)+sizeof(int));
}
void readStr(char **str, int idx)
{
	char *buf = memset(malloc(bufsize),0,bufsize);
	int siz = bufsize; // buf size
	int val = -1; // num read
	int num = 0; // num valid
	int tot = 0; // total read
	if (idx < 0 || idx >= lim || fdt[idx] == None || buf == 0) ERRFNC(idx);
	while (val != 0 && num == 0) {
		while (1) {
		val = ((fdt[idx] == Punt || fdt[idx] == Bunt) ?
		rfn[idx](inp[idx],buf+tot,1) :
		read(inp[idx],buf+tot,1));
		if (val < 0 && errno != EINTR) ERRFNC(idx);
		if (val < 0 && errno == EINTR) INTRFN();
		if (val >= 0) break;}
		tot += val;
		if (termfn) num = termfn(buf,tot,idx);
		else if (strnlen(buf,tot) == tot) num = 0;
		else num = strlen(buf)+1;
		if (num != 0 && num != tot) ERRFNC(idx);
		if (tot == siz) {
		buf = realloc(buf,siz+bufsize);
		memset(buf+siz,0,bufsize);
		siz += bufsize;
		if (buf == 0) ERRFNC(idx);}}
	buf[num] = 0;
	assignStr(str,buf);
	free(buf);
}
void preadStr(char **str, long long loc, int idx)
{
	char *buf = malloc(bufsize);
	int siz = bufsize; // buf size
	int val = -1; // num read
	int num = 0; // num valid
	int tot = 0; // total read
	if (idx < 0 || idx >= lim || fdt[idx] != Seek || buf == 0) ERRFNC(idx);
	while (val != 0 && num == 0) {
		while (1) {
		val = pread(inp[idx],buf+tot,siz-tot,loc+tot);
		if (val < 0 && errno != EINTR) ERRFNC(idx);
		if (val < 0 && errno == EINTR) INTRFN();
		if (val >= 0) break;}
		tot += val;
		if (termfn) num = termfn(buf,tot,idx);
		else if (strnlen(buf,tot) == tot) num = 0;
		else num = strlen(buf)+1;
		if (tot == siz) {
		siz += bufsize;
		buf = realloc(buf,siz);
		if (buf == 0) ERRFNC(idx);}}
	buf[num] = 0;
	assignStr(str,buf);
	free(buf);
}
void readDat(void **dat, int idx)
{
	int size = readInt(idx);
	int val = 0;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	*dat = malloc(size+sizeof(int));
	*(int*)(*dat) = size;
	while (1) {if (fdt[idx] == Punt || fdt[idx] == Bunt) val = rfn[idx](inp[idx],(void*)(((int*)(*dat))+1),size);
	else val = read(inp[idx],(void*)(((int*)(*dat))+1),size);
	if (val < 0 && errno == EINTR) INTRFN() else break;}
	if (val != 0 && val < size) ERRFNC(idx);
	// TODO reopen before calling notice if val == 0 and fdt[idx] == Poll
	if (val == 0)  NOTICE(idx);
}
void readEof(int idx)
{
	char arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = 0;
	while (1) {if (fdt[idx] == Punt || fdt[idx] == Bunt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(char));
	else val = read(inp[idx],(char *)&arg,sizeof(char));
	if (val < 0 && errno == EINTR) INTRFN() else break;}
	// TODO reopen before calling notice if val == 0 and fdt[idx] == Poll
	if (val != 0) NOTICE(idx);
}
char readChr(int idx)
{
	char arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = 0;
	while (1) {if (fdt[idx] == Punt || fdt[idx] == Bunt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(char));
	else val = read(inp[idx],(char *)&arg,sizeof(char));
	if (val < 0 && errno == EINTR) INTRFN() else break;}
	if (val != 0 && val < (int)sizeof(char)) ERRFNC(idx);
	// TODO reopen before calling notice if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(idx);}
	return arg;
}
int readInt(int idx)
{
	int arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = 0;
	while (1) {if (fdt[idx] == Punt || fdt[idx] == Bunt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(int));
	else val = read(inp[idx],(char *)&arg,sizeof(int));
	if (val < 0 && errno == EINTR) INTRFN() else break;}
	if (val != 0 && val < (int)sizeof(int)) ERRFNC(idx);
	// TODO reopen before calling notice if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(idx);}
	return arg;
}
int32_t readInt32(int idx)
{
	int32_t arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = 0;
	while (1) {if (fdt[idx] == Punt || fdt[idx] == Bunt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(int32_t));
	else val = read(inp[idx],(char *)&arg,sizeof(int32_t));
	if (val < 0 && errno == EINTR) INTRFN() else break;}
	if (val != 0 && val < (int)sizeof(int32_t)) ERRFNC(idx);
	// TODO reopen before calling notice if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(idx);}
	return arg;
}
double readNum(int idx)
{
	double arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = 0;
	while (1) {if (fdt[idx] == Punt || fdt[idx] == Bunt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(double));
	else val = read(inp[idx],(char *)&arg,sizeof(double));
	if (val < 0 && errno == EINTR) INTRFN() else break;}
	if (val != 0 && val < (int)sizeof(double)) ERRFNC(idx);
	// TODO reopen before calling notice if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0.0; NOTICE(idx);}
	return arg;
}
long long readNew(int idx)
{
	long long arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	if (inp[idx] < 0) {arg = 0; return arg;}
	int val = 0;
	while (1) {if (fdt[idx] == Punt || fdt[idx] == Bunt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(long long));
	else val = read(inp[idx],(char *)&arg,sizeof(long long));
	if (val < 0 && errno == EINTR) INTRFN() else break;}
	if (val != 0 && val < (int)sizeof(long long)) ERRFNC(idx);
	// TODO reopen before calling notice if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(idx);}
	return arg;
}
float readOld(int idx)
{
	float arg;
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	if (inp[idx] < 0) {arg = 0.0; return arg;}
	int val = 0;
	while (1) {if (fdt[idx] == Punt || fdt[idx] == Bunt) val = rfn[idx](inp[idx],(char *)&arg,sizeof(float));
	else val = read(inp[idx],(char *)&arg,sizeof(float));
	if (val < 0 && errno == EINTR) INTRFN() else break;}
	if (val != 0 && val < (int)sizeof(float)) ERRFNC(idx);
	// TODO reopen before calling notice if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0.0; NOTICE(idx);}
	return arg;
}
int writeBuf(const void *arg, long long siz, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	if (fdt[idx] == Poll || fdt[idx] == Bunt) {
		while (atoms[idx]+siz > atomz[idx]) atom[idx] = realloc(atom[idx],atomz[idx]+=bufsize);
		if (atom[idx] == 0) ERRFNC(idx);
		memcpy(atom[idx]+atoms[idx],arg,siz);
		atoms[idx] += siz;
		return siz;}
	if (fdt[idx] == Punt) return wfn[idx](out[idx],arg,siz);
	return write(out[idx],arg,siz);
}
void flushBuf(int idx)
{
	if (idx < 0 || idx >= lim) ERRFNC(idx);
	if (fdt[idx] != Poll && fdt[idx] != Bunt) ERRFNC(idx);
	if (fdt[idx] == Bunt) {if (wfn[idx](out[idx],atom[idx],atoms[idx]) < 0) ERRFNC(idx);}
	else {if (write(out[idx],atom[idx],atoms[idx]) < 0) ERRFNC(idx);}
	atoms[idx] = 0;
}
void writeStr(const char *arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int siz = (termfn ? termfn(arg,strlen(arg)+1,idx) : strlen(arg)+1);
	if (siz <= 0) ERRFNC(idx);
	int val = writeBuf(/*write(out[idx],*/arg,siz,idx);
	if (val < siz) ERRFNC(idx);
}
void pwriteStr(const char *arg, long long loc, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] != Seek) ERRFNC(idx);
	int siz = (termfn ? termfn(arg,strlen(arg)+1,idx) : strlen(arg)+1);
	if (siz <= 0) ERRFNC(idx);
	int val = pwrite(out[idx],arg,siz,loc);
	if (val < siz) ERRFNC(idx);
}
void writeDat(const void *arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int siz = *(int*)arg;
	int val = writeBuf((void*)(((int*)arg)+1),siz,idx);
	if (val < siz) ERRFNC(idx);
}
void writeChr(char arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(char), idx);
	if (val < (int)sizeof(char)) ERRFNC(idx);
}
void writeInt(int arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(int), idx);
	if (val < (int)sizeof(int)) ERRFNC(idx);
}
void writeInt32(int32_t arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(int32_t), idx);
	if (val < (int)sizeof(int32_t)) ERRFNC(idx);
}
void writeNum(double arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(double), idx);
	if (val < (int)sizeof(double)) ERRFNC(idx);
}
void writeNew(long long arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(long long), idx);
	if (val < (int)sizeof(long long)) ERRFNC(idx);
}
void writeOld(float arg, int idx)
{
	if (idx < 0 || idx >= lim || fdt[idx] == None) ERRFNC(idx);
	int val = writeBuf(/*write(out[idx]*/(char *)&arg,sizeof(float), idx);
	if (val < (int)sizeof(float)) ERRFNC(idx);
}
void showEnum(const char *typ, const char* val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"%s(%s)",typ,val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showFieldV(const char* val, char **str, int *siz, int arg, int *sub)
{
	char *tmp = 0;
	char *temp = 0;
	int num;
	if (asprintf(&tmp,"%s",val) < 0) ERRFNC(-1);
	for (int i = 0; i < arg; i++) {
	if (asprintf(&temp,"%s[%d]",tmp,sub[i]) < 0) ERRFNC(-1);
	free(tmp); tmp = temp;}
	if (asprintf(&temp,"%s:",tmp) < 0) ERRFNC(-1);
	free(tmp); tmp = temp;
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showField(const char* val, char **str, int *siz, int arg, ...)
{
	int *sub = malloc(arg*sizeof(int));
	va_list args = {0};
	va_start(args,arg);
	for (int i = 0; i < arg; i++) sub[i] = va_arg(args,int);
	va_end(args);
	showFieldV(val,str,siz,arg,sub);
	free(sub);
}
void showOpen(const char* val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"%s(",val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showClose(char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,")") < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showChr(char val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Chr(%c)",val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showInt(int val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Int(%d)",val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showInt32(int32_t val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Int32(%ld)",(long)val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showNew(long long val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"New(%lld)",val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showNum(double val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Num(%lf)",val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showOld(float val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Old(%f)",val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showStr(const char* val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Str(%s)",val) < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
void showDat(const void* val, char **str, int *siz)
{
	char *tmp = 0;
	int num;
	if (asprintf(&tmp,"Dat(TODO)") < 0) ERRFNC(-1);
	num = strlen(tmp);
	*str = realloc(*str,*siz+num+1);
	if (*str == 0) ERRFNC(-1);
	memcpy(*str+*siz,tmp,num+1);
	free(tmp);
	*siz += num;
}
int hideIdent(const char *val, const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," %s %%n",val) < 0) ERRFNC(-1);
	sscanf(str+*siz,tmp,&num);
	free(tmp);
	if (num == -1) return 0;
	if (isalnum(str[*siz+num])) return 0;
	*siz += num;
	return 1;
}
int hideEnum(const char* typ, const char *val, const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," %s ( %s ) %%n",typ,val) < 0) ERRFNC(-1);
	sscanf(str+*siz,tmp,&num);
	free(tmp);
	if (num == -1) return 0;
	*siz += num;
	return 1;
}
int hideFieldV(const char *val, const char *str, int *siz, int arg, int *sub)
{
	char *tmp = 0;
	char *temp = 0;
	int num = -1;
	if (asprintf(&tmp," %s",val) < 0) ERRFNC(-1);
	for (int i = 0; i < arg; i++) {
	if (asprintf(&temp,"%s [ %d ]",tmp,sub[i]) < 0) ERRFNC(-1);
	free(tmp); tmp = temp;}
	if (asprintf(&temp,"%s : %%n",tmp) < 0) ERRFNC(-1);
	free(tmp); tmp = temp;
	sscanf(str+*siz,tmp,&num);
	if (num == -1) {printf("hideField\n\t%s\n\t%s\n\t%s\n",str,str+*siz,tmp); free(tmp); return 0;}
	*siz += num;
	free(tmp);
	return 1;
}
int hideField(const char *val, const char *str, int *siz, int arg, ...)
{
	int *sub = malloc(arg*sizeof(int));
	int ret = 0;
	va_list args = {0};
	va_start(args,arg);
	for (int i = 0; i < arg; i++) sub[i] = va_arg(args,int);
	va_end(args);
	ret = hideFieldV(val,str,siz,arg,sub);
	free(sub);
	return ret;
}
int hideOpen(const char *val, const char *str, int *siz)
{
	char *tmp = 0;
	int num = -1;
	if (asprintf(&tmp," %s ( %%n",val) < 0) ERRFNC(-1);
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
	if (asprintf(&tmp," ) %%n") < 0) ERRFNC(-1);
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
int hideInt32(int32_t *val, const char *str, int *siz)
{
	int num = -1;
	long tmp = 0;
	sscanf(str+*siz," Int32 ( %ld )%n",&tmp,&num);
	if (num == -1) return 0;
	*val = tmp; *siz += num;
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
int hideDat(void **val, const char *str, int *siz)
{
	int len = 0;
	char *tmp = 0;
	int base = -1;
	int num = -1;
	int limit = -1;
	sscanf(str+*siz," Dat ( %n",&base);
	limit = base; while (base != -1 && str[limit] && num == -1) sscanf(str+*siz+(++limit)," )%n",&num);
	if (num == -1) return 0;
	tmp = malloc(limit-base+1);
	if (tmp == 0) return 0;
	strncpy(tmp,str+*siz+base,limit-base); tmp[limit-base] = 0;
	assignDat(val,&len); // TODO
	free(tmp);
	*siz += limit+num;
	return 1;
}

void readStrHs(hftype fnc, int idx)
{
	char *str = 0;
	readStr(&str,idx);
	fnc(str);
	free(str);
}
void readDatHs(hktype fnc, int idx)
{
	char *tmp = 0;
	readDat((void**)&tmp,idx);
	fnc(*(int*)tmp,tmp+sizeof(int));
	free(tmp);
}
void writeDatHs(int len, const char *val, int idx)
{
	void *tmp = malloc(len+sizeof(int));
	*(int*)tmp = len;
	memcpy(tmp+sizeof(int),val,len);
	writeDat(tmp,idx);
	free(tmp);
}

int hideEnumHs(const char *typ, const char *val, const char *str, hftype fnc)
{
	int len = 0;
	if (!hideEnum(typ,val,str,&len)) return 0;
	fnc(str+len);
	return 1;
}
int hideOpenHs(const char *typ, const char *str, hftype fnc)
{
	int len = 0;
	if (!hideOpen(typ,str,&len)) return 0;
	fnc(str+len);
	return 1;
}
int hideCloseHs(const char *str, hftype fnc)
{
	int len = 0;
	if (!hideClose(str,&len)) return 0;
	fnc(str+len);
	return 1;
}
int hideFieldHs(const char *typ, int arg, int *ptr, const char *str, hftype fnc)
{
	int len = 0;
	if (!hideFieldV(typ,str,&len,arg,ptr)) return 0;
	fnc(str+len);
	return 1;
}
int hideStrHs(hftype val, const char *str, hftype fnc)
{
	char *tmp = 0;
	int len = 0;
	int ret = hideStr(&tmp, str, &len);
	if (ret) {val(tmp); fnc(str+len);}
	free(tmp);
	return ret;
}
int hideDatHs(hktype val, const char *str, hftype fnc)
{
	char *tmp = 0;
	int len = 0;
	int ret = hideDat((void**)&tmp, str, &len);
	if (ret) {val(*(int*)tmp,tmp+sizeof(int)); fnc(str+len);}
	free(tmp);
	return ret;
}
int hideIntHs(hgtype val, const char *str, hftype fnc)
{
	int tmp = 0;
	int len = 0;
	int ret = hideInt(&tmp, str, &len);
	if (ret) {val(tmp); fnc(str+len);}
	return ret;
}
int hideInt32Hs(hltype val, const char *str, hftype fnc)
{
	int32_t tmp = 0;
	int len = 0;
	int ret = hideInt32(&tmp, str, &len);
	if (ret) {val(tmp); fnc(str+len);}
	return ret;
}
int hideNumHs(hhtype val, const char *str, hftype fnc)
{
	double tmp = 0;
	int len = 0;
	int ret = hideNum(&tmp, str, &len);
	if (ret) {val(tmp); fnc(str+len);}
	return ret;
}
int hideNewHs(hitype val, const char *str, hftype fnc)
{
	long long tmp = 0;
	int len = 0;
	int ret = hideNew(&tmp, str, &len);
	if (ret) {val(tmp); fnc(str+len);}
	return ret;
}
int hideOldHs(hjtype val, const char *str, hftype fnc)
{
	float tmp = 0;
	int len = 0;
	int ret = hideOld(&tmp, str, &len);
	if (ret) {val(tmp); fnc(str+len);}
	return ret;
}

void showEnumHs(const char *typ, const char *val, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showEnum(typ,val,&tmp,&len);
	fnc(tmp);
	free(tmp);
}
void showOpenHs(const char *typ, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showOpen(typ,&tmp,&len);
	fnc(tmp);
	free(tmp);
}
void showCloseHs(const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showClose(&tmp,&len);
	fnc(tmp);
	free(tmp);
}
void showFieldHs(const char *typ, int arg, int *sub, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showFieldV(typ,&tmp,&len,arg,sub);
	fnc(tmp);
	free(tmp);
}
void showStrHs(const char *val, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showStr(val,&tmp,&len);
	fnc(tmp);
	free(tmp);
}
void showDatHs(int siz, const char *val, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	char *cpy = malloc(siz+sizeof(int));
	*(int*)cpy = siz;
	memcpy(cpy+sizeof(int),val,siz);
	showDat(cpy,&tmp,&len);
	fnc(tmp);
	free(tmp);
	free(cpy);
}
void showIntHs(int val, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showInt(val,&tmp,&len);
	fnc(tmp);
	free(tmp);
}
void showInt32Hs(int32_t val, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showInt32(val,&tmp,&len);
	fnc(tmp);
	free(tmp);
}
void showNumHs(double val, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showNum(val,&tmp,&len);
	fnc(tmp);
	free(tmp);
}
void showNewHs(long long val, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showNew(val,&tmp,&len);
	fnc(tmp);
	free(tmp);
}
void showOldHs(float val, const char *str, hftype fnc)
{
	int len = strlen(str);
	char *tmp = strdup(str);
	showOld(val,&tmp,&len);
	fnc(tmp);
	free(tmp);
}

int waitReadLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,waitRead(lua_tonumber(lua,1),(int)lua_tonumber(lua,2)));
	return 1;
}
int waitExitLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,waitExit());
	return 1;
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
int readStrLua(lua_State *lua)
{
	luaerr = lua;
	char *str = 0;
	readStr(&str,(int)lua_tonumber(lua,1));
	lua_pushstring(lua,str);
	free(str);
	return 1;
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
int readInt32Lua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readInt32((int)lua_tonumber(lua,1)));
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
	writeStr(lua_tostring(lua,1),(int)lua_tonumber(lua,2));
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
int writeInt32Lua(lua_State *lua)
{
	luaerr = lua;
	writeInt32((int)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
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
int hideEnumLua(lua_State *lua)
{
	int siz = 0;
	char *str = strdup(lua_tostring(lua,3));
	luaerr = lua;
	if (hideEnum(lua_tostring(lua,1),lua_tostring(lua,2),str,&siz))
	lua_pushnumber(lua,1); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str);
	return 2;
}
int hideFieldLua(lua_State *lua)
{
	int siz = 0;
	char *str = strdup(lua_tostring(lua,2));
	int arg = lua_tonumber(lua,3);
	int *sub = malloc(arg*sizeof(int));
	for (int i = 0; i < arg; i++) sub[i] = lua_tonumber(lua,4+i);
	luaerr = lua;
	if (hideFieldV(lua_tostring(lua,1),str,&siz,arg,sub))
	lua_pushnumber(lua,1); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str); free(sub);
	return 2;
}
int hideOpenLua(lua_State *lua)
{
	int siz = 0;
	char *str = strdup(lua_tostring(lua,2));
	luaerr = lua;
	if (hideOpen(lua_tostring(lua,1),str,&siz))
	lua_pushnumber(lua,1); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str);
	return 2;
}
int hideCloseLua(lua_State *lua)
{
	int siz = 0;
	char *str = strdup(lua_tostring(lua,1));
	luaerr = lua;
	if (hideClose(str,&siz))
	lua_pushnumber(lua,1); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str);
	return 2;
}
int hideStrLua(lua_State *lua)
{
	char *val = 0;
	int siz = 0;
	char *str = strdup(lua_tostring(lua,1));
	luaerr = lua;
	if (hideStr(&val,str,&siz))
	lua_pushstring(lua,val); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str); free(val);
	return 2;
}
int hideIntLua(lua_State *lua)
{
	int val = 0;
	int siz = 0;
	char *str = strdup(lua_tostring(lua,1));
	luaerr = lua;
	if (hideInt(&val,str,&siz))
	lua_pushnumber(lua,val); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str);
	return 2;
}
int hideInt32Lua(lua_State *lua)
{
	int32_t val = 0;
	int siz = 0;
	char *str = strdup(lua_tostring(lua,1));
	luaerr = lua;
	if (hideInt32(&val,str,&siz))
	lua_pushnumber(lua,val); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str);
	return 2;
}
int hideNumLua(lua_State *lua)
{
	double val = 0;
	int siz = 0;
	char *str = strdup(lua_tostring(lua,1));
	luaerr = lua;
	if (hideNum(&val,str,&siz))
	lua_pushnumber(lua,val); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str);
	return 2;
}
int hideNewLua(lua_State *lua)
{
	long long int val = 0;
	int siz = 0;
	char *str = strdup(lua_tostring(lua,1));
	luaerr = lua;
	if (hideNew(&val,str,&siz))
	lua_pushnumber(lua,val); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str);
	return 2;
}
int hideOldLua(lua_State *lua)
{
	float val = 0;
	int siz = 0;
	char *str = strdup(lua_tostring(lua,1));
	luaerr = lua;
	if (hideOld(&val,str,&siz))
	lua_pushnumber(lua,val); else lua_pushnil(lua);
	lua_pushstring(lua,str+siz);
	free(str);
	return 2;
}
int showEnumLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showEnum(lua_tostring(lua,1),lua_tostring(lua,2),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showFieldLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	int arg = lua_tonumber(lua,2);
	int *sub = malloc(arg*sizeof(int));
	for (int i = 0; i < arg; i++) sub[i] = lua_tonumber(lua,3+i);
	luaerr = lua;
	showFieldV(lua_tostring(lua,1),&str,&siz,arg,sub);
	lua_pushstring(lua,str);
	free(sub);
	return 1;
}
int showOpenLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showOpen(lua_tostring(lua,1),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showCloseLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showClose(&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showStrLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showStr(lua_tostring(lua,1),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showChrLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showChr(lua_tonumber(lua,1),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showIntLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showInt(lua_tonumber(lua,1),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showInt32Lua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showInt32(lua_tonumber(lua,1),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showNumLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showNum(lua_tonumber(lua,1),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showNewLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showNew(lua_tonumber(lua,1),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}
int showOldLua(lua_State *lua)
{
	int siz = 0;
	char *str = 0;
	luaerr = lua;
	showOld(lua_tonumber(lua,1),&str,&siz);
	lua_pushstring(lua,str);
	return 1;
}

void noteLua(int idx)
{
	int val = luaxCall(luanote,protoCloseCf(idx));
	if (val < 0) ERROR();
}
void errLua(const char *str, int num, int idx)
{
	int val = luaxCall(luafunc,protoCloseEf(str,num,idx));
	if (val < 0) ERROR();
}
void noteFuncLua(const char *str)
{
	noteFunc(noteLua);
	if (luanote) free(luanote);
	luanote = strdup(str);
}
void errFuncLua(const char *str)
{
	errFunc(errLua);
	if (luaerr) free(luaerr);
	luafunc = strdup(str);
}
void luaxExtend(lua_State *L, const char *str, struct Function fnc);
int luaopen_luax(lua_State *L);
int luaopen_face (lua_State *L)
{
	luaopen_luax(L);
	luaxExtend(L,"noteFunc",protoTypeHf(noteFuncLua));
	luaxExtend(L,"errFunc",protoTypeHf(errFuncLua));
	luaxExtend(L,"closeIdent",protoTypeCf(closeIdent));
	luaxExtend(L,"moveIdent",protoTypeCg(moveIdent));
	luaxExtend(L,"findIdent",protoTypeFf(findIdent));
	luaxExtend(L,"inetIdent",protoTypeGf(inetIdent));
	luaxExtend(L,"openPipe",protoTypeRg(openPipe));
	luaxExtend(L,"openPipe",protoTypeFf(openPipe));
	luaxExtend(L,"openFile",protoTypeFf(openFile));
	luaxExtend(L,"openInet",protoTypeGf(openInet));
	luaxExtend(L,"forkExec",protoTypeFf(forkExec));
	luaxExtend(L,"pipeInit",protoTypeGf(pipeInit));
	lua_pushcfunction(L, waitReadLua); lua_setglobal(L, "waitRead");
	lua_pushcfunction(L, waitExitLua); lua_setglobal(L, "waitExit");
	lua_pushcfunction(L, pollPipeLua); lua_setglobal(L, "pollPipe");
	lua_pushcfunction(L, pollFileLua); lua_setglobal(L, "pollFile");
	lua_pushcfunction(L, seekFileLua); lua_setglobal(L, "seekFile");
	lua_pushcfunction(L, truncFileLua); lua_setglobal(L, "truncFile");
	lua_pushcfunction(L, checkFileLua); lua_setglobal(L, "checkFile");
	lua_pushcfunction(L, rdlkFileLua); lua_setglobal(L, "rdlkFile");
	lua_pushcfunction(L, wrlkFileLua); lua_setglobal(L, "wrlkFile");
	lua_pushcfunction(L, unlkFileLua); lua_setglobal(L, "unlkFile");
	lua_pushcfunction(L, rdlkwFileLua); lua_setglobal(L, "rdlkwFile");
	lua_pushcfunction(L, wrlkwFileLua); lua_setglobal(L, "wrlkwFile");
	lua_pushcfunction(L, checkReadLua); lua_setglobal(L, "checkRead");
	lua_pushcfunction(L, checkWriteLua); lua_setglobal(L, "checkWrite");
	lua_pushcfunction(L, sleepSecLua); lua_setglobal(L, "sleepSec");
	lua_pushcfunction(L, readStrLua); lua_setglobal(L, "readStr");
	lua_pushcfunction(L, readEofLua); lua_setglobal(L, "readEof");
	// lua_pushcfunction(L, readDatLua); lua_setglobal(L, "readDat"); // TODO
	lua_pushcfunction(L, readChrLua); lua_setglobal(L, "readChr");
	lua_pushcfunction(L, readIntLua); lua_setglobal(L, "readInt");
	lua_pushcfunction(L, readInt32Lua); lua_setglobal(L, "readInt32");
	lua_pushcfunction(L, readNumLua); lua_setglobal(L, "readNum");
	lua_pushcfunction(L, readNewLua); lua_setglobal(L, "readNew");
	lua_pushcfunction(L, readOldLua); lua_setglobal(L, "readOld");
	lua_pushcfunction(L, writeStrLua); lua_setglobal(L, "writeStr");
	// lua_pushcfunction(L, writeDatLua); lua_setglobal(L, "writeDat"); // TODO
	lua_pushcfunction(L, writeChrLua); lua_setglobal(L, "writeChr");
	lua_pushcfunction(L, writeIntLua); lua_setglobal(L, "writeInt");
	lua_pushcfunction(L, writeInt32Lua); lua_setglobal(L, "writeInt32");
	lua_pushcfunction(L, writeNumLua); lua_setglobal(L, "writeNum");
	lua_pushcfunction(L, writeNewLua); lua_setglobal(L, "writeNew");
	lua_pushcfunction(L, writeOldLua); lua_setglobal(L, "writeOld");
	lua_pushcfunction(L, hideEnumLua); lua_setglobal(L, "hideEnum");
	lua_pushcfunction(L, hideFieldLua); lua_setglobal(L, "hideField");
	lua_pushcfunction(L, hideOpenLua); lua_setglobal(L, "hideOpen");
	lua_pushcfunction(L, hideCloseLua); lua_setglobal(L, "hideClose");
	lua_pushcfunction(L, hideStrLua); lua_setglobal(L, "hideStr");
	// lua_pushcfunction(L, hideDatLua); lua_setglobal(L, "hideDat"); // TODO
	lua_pushcfunction(L, hideIntLua); lua_setglobal(L, "hideInt");
	lua_pushcfunction(L, hideInt32Lua); lua_setglobal(L, "hideInt32");
	lua_pushcfunction(L, hideNumLua); lua_setglobal(L, "hideNum");
	lua_pushcfunction(L, hideNewLua); lua_setglobal(L, "hideNew");
	lua_pushcfunction(L, hideOldLua); lua_setglobal(L, "hideOld");
	lua_pushcfunction(L, showEnumLua); lua_setglobal(L, "showEnum");
	lua_pushcfunction(L, showFieldLua); lua_setglobal(L, "showField");
	lua_pushcfunction(L, showOpenLua); lua_setglobal(L, "showOpen");
	lua_pushcfunction(L, showCloseLua); lua_setglobal(L, "showClose");
	lua_pushcfunction(L, showStrLua); lua_setglobal(L, "showStr");
	// lua_pushcfunction(L, showDatLua); lua_setglobal(L, "showDat"); // TODO
	lua_pushcfunction(L, showIntLua); lua_setglobal(L, "showInt");
	lua_pushcfunction(L, showInt32Lua); lua_setglobal(L, "showInt32");
	lua_pushcfunction(L, showNumLua); lua_setglobal(L, "showNum");
	lua_pushcfunction(L, showNewLua); lua_setglobal(L, "showNew");
	lua_pushcfunction(L, showOldLua); lua_setglobal(L, "showOld");
	return 0;
}
