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

int inp[NUMOPEN] = {0};
int out[NUMOPEN] = {0};
enum {None,Wait,Poll,Seek,Inet} fdt[NUMOPEN] = {0};
pid_t pid[NUMOPEN] = {0};
int len = 0;
int bufsize = BUFSIZE;
eftype inpexc[NUMOPEN] = {0};
eftype inperr[NUMOPEN] = {0};
eftype outerr[NUMOPEN] = {0};
char *exclua[NUMOPEN] = {0};
char *inplua[NUMOPEN] = {0};
char *outlua[NUMOPEN] = {0};
char *fnclua[NUMOPEN] = {0};
lua_State *luaerr = 0;
struct sockaddr_in6 addr[NUMINET] = {0};
int ads = 0;
int inet[NUMOPEN] = {0};
pthread_t cbpth[NUMOPEN] = {0};
wftype cbfnc[NUMOPEN] = {0};

void exitErr(const char *str, int num, int idx)
{
	fprintf(stderr,"exitErr %s(%d): %d %lld\n",str,num,errno,(long long)getpid());
	exit(-1);
}
void readNote(eftype exc, int idx)
{
	if (idx < 0 || idx >= len) ERROR(exitErr,0)
	inpexc[idx] = exc;
}
void readJump(eftype err, int idx)
{
	if (idx < 0 || idx >= len) ERROR(exitErr,0)
	inperr[idx] = err;
}
void writeJump(eftype err, int idx)
{
	if (idx < 0 || idx >= len) ERROR(exitErr,0)
	outerr[idx] = err;
}
void bothJump(eftype err, int idx)
{
	readJump(err,idx);
	writeJump(err,idx);
}
void closeIdent(int idx)
{
	if (fdt[idx] != None) {
		close(inp[idx]);
		if (inp[idx] != out[idx]) close(out[idx]);
		fdt[idx] = None;
		inpexc[idx] = 0;
		inperr[idx] = 0;
		outerr[idx] = 0;
	}
	while (len > 0 && fdt[len-1] == None) len--;
}
void moveIdent(int idx0, int idx1)
{
	if (idx1 < 0 || idx1 >= len) ERROR(inperr[idx1],idx1);
	if (idx0 < 0 || idx0 >= len) ERROR(inperr[idx1],idx1);
	closeIdent(idx1);
	inp[idx1] = inp[idx0];
	out[idx1] = out[idx0];
	fdt[idx1] = fdt[idx0];
	inpexc[idx1] = inpexc[idx0];
	inperr[idx1] = inperr[idx0];
	outerr[idx1] = outerr[idx0];
}
int openPipe()
{
	int fd[2] = {0};
	if (len >= NUMOPEN) return -1;
	if (pipe(fd) < 0) return -1;
	inp[len] = fd[0];
	out[len] = fd[1];
	fdt[len] = Wait;
	return len++;
}
int openFifo(const char *str)
{
	int fi = 0;
	int fo = 0;
	if (len >= NUMOPEN) return -1;
	if ((mkfifo(str,0666) < 0) && errno != EEXIST) return -1;
	if ((fi = open(str,O_RDONLY | O_NONBLOCK)) < 0) return -1;
	if ((fo = open(str,O_WRONLY | O_NONBLOCK)) < 0) return -1;
	if (fcntl(fi,F_SETFL,0) < 0) return -1;
	if (fcntl(fo,F_SETFL,0) < 0) return -1;
	inp[len] = fi;
	out[len] = fo;
	fdt[len] = Poll;
	return len++;
}
int openFile(const char *str)
{
	int fd = 0;
	if (len >= NUMOPEN) return -1;
	if ((fd = open(str,O_RDWR|O_CREAT,0666)) < 0) return -1;
	inp[len] = fd;
	out[len] = fd;
	fdt[len] = Seek;
	return len++;
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
int openInet(const char *adr, const char *num)
{
	if (len >= NUMOPEN) return -1;
	int fd = 0;
	if (adr == 0) {
	if ((fd = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP)) < 0) return -1;
	int flag = 1;
	if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &flag, sizeof(flag)) < 0) return -1;
	struct sockaddr_in6 adr = {0};
	if (scanInet6(&adr,0,num) == 0) return -1;
	if (bind(fd, (struct sockaddr*)&adr, sizeof(adr)) < 0) return -1;
	if (listen(fd, NUMPEND) < 0) return -1;
	fdt[len] = Inet;} else {
	if (ads >= NUMINET) return -1;
	if ((fd = socket(AF_INET6, SOCK_STREAM, IPPROTO_TCP)) < 0) return -1;
	if (scanInet6(&addr[ads],adr,num) == 0) return -1;
	if (connect(fd, (struct sockaddr*)&addr[ads], sizeof(addr[ads])) < 0) return -1;
	fdt[len] = Wait;
	ads++;}
	inp[len] = fd;
	out[len] = fd;
	return len++;
}
int forkExec(const char *exe)
{
	int c2p[2], p2c[2], val;
	val = pipe(c2p); if (val < 0) return -1;
	val = pipe(p2c); if (val < 0) return -1;
	if (len >= NUMOPEN) return -1;
	pid[len] = fork(); if (pid[len] < 0) return -1;
	if (pid[len] == 0) {
		char ist[33], ost[33], idt[33];
		val = close(c2p[0]); if (val < 0) return -1;
		val = close(p2c[1]); if (val < 0) return -1;
		val = snprintf(ost,32,"%d",c2p[1]); if (val < 0 || val > 32) return -1;
		val = snprintf(ist,32,"%d",p2c[0]); if (val < 0 || val > 32) return -1;
		val = snprintf(idt,32,"%d",len); if (val < 0 || val > 32) return -1;
		val = execl(exe,exe,ist,ost,idt,0); if (val < 0) return -1;
		return -1;}
	val = close(c2p[1]); if (val < 0) return -1;
	val = close(p2c[0]); if (val < 0) return -1;
	inp[len] = c2p[0];
	out[len] = p2c[1];
	fdt[len] = Wait;
	int ret = len;
	len++;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) return -1;
	return ret;
}
int pipeInit(const char *av1, const char *av2)
{
	int val;
	val = sscanf(av1,"%d",&inp[len]); if (val != 1) return -1;
	val = sscanf(av2,"%d",&out[len]); if (val != 1) return -1;
	fdt[len] = Wait;
	int ret = len;
	len++;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) return -1;
	return ret;
}
int pselectAny(struct timespec *dly, int idx)
{
	while (1) {
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	for (int i = 0; i < len; i++) if (idx < 0 || idx == i) {
		if (fdt[i] == Wait && nfd <= inp[i]) nfd = inp[i]+1;
		if (fdt[i] == Wait) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}
		if (fdt[i] == Inet && nfd <= inp[i]) nfd = inp[i]+1;
		if (fdt[i] == Inet) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}}
	if (nfd == 0) return -1;
	val = -1; errno = EINTR;
	while (val < 0 && errno == EINTR) val = pselect(nfd,&fds,0,&ers,dly,0);
	if (val < 0) ERROR(exitErr,0);
	if (val == 0) return -1;
	nfd = 0; for (int i = 0; i < len; i++) if (idx < 0 || idx == i) {
		if (fdt[i] == Wait && FD_ISSET(inp[i],&ers)) {closeIdent(i); nfd++;}
		if (fdt[i] == Inet && FD_ISSET(inp[i],&ers)) {closeIdent(i); nfd++;}}
	if (nfd == 0) for (int i = 0; i < len; i++) if (idx < 0 || idx == i) {
		if (fdt[i] == Wait && FD_ISSET(inp[i],&fds)) return i;
		if (fdt[i] == Inet && FD_ISSET(inp[i],&fds)) {
		struct sockaddr_in6 adr = {0};
		socklen_t len = sizeof(adr);
		if (len >= NUMOPEN) {closeIdent(i); continue;}
		inp[len] = out[len] = accept(inp[i],(struct sockaddr*)&adr,&len);
		if (inp[len] < 0) continue;
		fdt[len] = Wait;
		return len++;}}
	} return -1;
}
int waitAny()
{
	return pselectAny(0,-1);
}
int pauseAny(double dly)
{
	struct timespec delay = {0};
	delay.tv_sec = (long long)dly;
	delay.tv_nsec = (dly-(long long)dly)*SEC2NANO;
	return pselectAny(&delay,-1);
}
void *callCall(void *arg)
{
	int idx = (int)(size_t)arg;
	while (1) {
		int sub = pselectAny(0,idx);
		if (sub == idx) cbfnc[idx](idx);
	}
}
void callInit(wftype fnc, int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] != Wait) ERROR(exitErr,0);
	cbfnc[idx] = fnc;
	if (pthread_create(&cbpth[idx],0,callCall,(void*)(size_t)idx) != 0) ERROR(exitErr,0);
}
int pollPipe(int idx)
{
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	if (idx < 0 || idx >= len || fdt[idx] != Poll) return 0;
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
	if (idx < 0 || idx >= len || fdt[idx] != Seek) return 0;
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
	if (idx < 0 || idx >= len || fdt[idx] != Seek) return 0;
	if ((pos = lseek(inp[idx],0,SEEK_CUR)) < 0) ERROR(inperr[idx],idx);
	if ((siz = lseek(inp[idx],0,SEEK_END)) < 0) ERROR(inperr[idx],idx);
	if (lseek(inp[idx],pos,SEEK_CUR) < 0) ERROR(inperr[idx],idx);
	return siz;
}
int pollInet(const char *adr, const char *num)
{
	return (checkInet(adr,num) < NUMINET);
}
int checkInet(const char *adr, const char *num)
{
	struct sockaddr_in6 comp = {0};
	if (scanInet6(&comp,adr,num) == 0) return -1;
	for (int i = 0; i < ads; i++)
	if (memcmp(&addr[i],&comp,sizeof(comp)) == 0)
	return i;
	return NUMINET;
}
int rdlkFile(long long arg0, long long arg1, int idx)
{
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_RDLCK;
	lock.l_whence = SEEK_SET;
	int val = 0;
	if ((val = fcntl(inp[idx],F_SETLK,&lock)) < 0 && errno != EAGAIN) ERROR(inperr[idx],idx);
	return (val==0);
}
int wrlkFile(long long arg0, long long arg1, int idx)
{
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	int val = 0;
	if ((val = fcntl(inp[idx],F_SETLK,&lock)) < 0 && errno != EAGAIN) ERROR(inperr[idx],idx);
	return (val==0);
}
void unlkFile(long long arg0, long long arg1, int idx)
{
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_UNLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLK,&lock) < 0) ERROR(inperr[idx],idx);
}
void rdlkwFile(long long arg0, long long arg1, int idx)
{
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_RDLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLKW,&lock) < 0) ERROR(inperr[idx],idx);
}
void wrlkwFile(long long arg0, long long arg1, int idx)
{
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLKW,&lock) < 0) ERROR(inperr[idx],idx);
}
int checkRead(int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) return 0;
	return 1;
}
int checkWrite(int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) return 0;
	return 1;
}
void sleepSec(int sec)
{
	sleep(sec);
}
void callStr(const char* str, int trm, void*arg)
{
    char **ptr = arg;
    allocStr(ptr,str);
}
void readStr(cftype fnc, void *arg, int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	char buf[bufsize]; memset(buf,0,bufsize);
	int trm = 0;
	for (int i = 0; i < bufsize-1; i++) {
		int val = read(inp[idx],buf+i,1);
		if (val < 0) ERROR(inperr[idx],idx);
		// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
		if (val == 0 && i == 0) NOTICE(inpexc[idx],idx)
		if (val == 0) {buf[i] = 0; break;}
		if (buf[i] == 0) {trm = 1; break;}}
	buf[bufsize-1] = 0;
	fnc(buf,trm,arg);
}
void readStrHsFnc(const char *buf, int trm, void *arg)
{
	hftype fnc = arg;
	fnc(buf,trm);
}
void readStrHs(hftype fnc, int idx)
{
	readStr(readStrHsFnc,fnc,idx);
}

char readChr(int idx)
{
	char arg;
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int val = read(inp[idx],(char *)&arg,sizeof(char));
	if (val != 0 && val < (int)sizeof(char)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(inpexc[idx],idx)}
	return arg;
}
int readInt(int idx)
{
	int arg;
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int val = read(inp[idx],(char *)&arg,sizeof(int));
	if (val != 0 && val < (int)sizeof(int)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(inpexc[idx],idx)}
	return arg;
}
double readNum(int idx)
{
	double arg;
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int val = read(inp[idx],(char *)&arg,sizeof(double));
	if (val != 0 && val < (int)sizeof(double)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0.0; NOTICE(inpexc[idx],idx)}
	return arg;
}
long long readNew(int idx)
{
	long long arg;
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	if (inp[idx] < 0) {arg = 0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(long long));
	if (val != 0 && val < (int)sizeof(long long)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0; NOTICE(inpexc[idx],idx)}
	return arg;
}
float readOld(int idx)
{
	float arg;
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	if (inp[idx] < 0) {arg = 0.0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(float));
	if (val != 0 && val < (int)sizeof(float)) ERROR(inperr[idx],idx)
	// TODO reopen before calling NOTICE if val == 0 and fdt[idx] == Poll
	if (val == 0) {arg = 0.0; NOTICE(inpexc[idx],idx)}
	return arg;
}
void writeStr(const char *arg, int trm, int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int siz = strlen(arg)+trm;
	int val = write(out[idx],arg,siz);
	if (val < siz) ERROR(outerr[idx],idx)
}
void writeChr(char arg, int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(char));
	if (val < (int)sizeof(char)) ERROR(outerr[idx],idx)
}
void writeInt(int arg, int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(int));
	if (val < (int)sizeof(int)) ERROR(outerr[idx],idx)
}
void writeNum(double arg, int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(double));
	if (val < (int)sizeof(double)) ERROR(outerr[idx],idx)
}
void writeNew(long long arg, int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(long long));
	if (val < (int)sizeof(long long)) ERROR(outerr[idx],idx)
}
void writeOld(float arg, int idx)
{
	if (idx < 0 || idx >= len || fdt[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(float));
	if (val < (int)sizeof(float)) ERROR(outerr[idx],idx)
}

void allocChr(char **ptr, int siz)
{
    if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
    if (siz == 0) return;
    *ptr = realloc(*ptr,siz*sizeof(char));
    for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocInt(int **ptr, int siz)
{
    if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
    if (siz == 0) return;
    *ptr = realloc(*ptr,siz*sizeof(int));
    for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocNew(long long **ptr, int siz)
{
    if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
    if (siz == 0) return;
    *ptr = realloc(*ptr,siz*sizeof(long long));
    for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocNum(double **ptr, int siz)
{
    if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
    if (siz == 0) return;
    *ptr = realloc(*ptr,siz*sizeof(double));
    for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocOld(float **ptr, int siz)
{
    if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
    if (siz == 0) return;
    *ptr = realloc(*ptr,siz*sizeof(float));
    for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void allocStr(char **ptr, const char *str)
{
    if (*ptr && str == 0) {free(*ptr); *ptr = 0;}
    if (str == 0) return;
    *ptr = realloc(*ptr,strlen(str)+1);
    strcpy(*ptr,str);
}
void allocPtr(void ***ptr, int siz)
{
    if (*ptr && siz == 0) {free(*ptr); *ptr = 0;}
    if (siz == 0) return;
    *ptr = realloc(*ptr,siz*sizeof(void*));
    for (int i = 0; i < siz; i++) (*ptr)[i] = 0;
}
void showChr(char val, char **str)
{
	asprintf(str,"Chr(%c)",val);
}
void showInt(int val, char **str)
{
	asprintf(str,"Int(%d)",val);
}
void showNew(long long val, char **str)
{
	asprintf(str,"New(%lld)",val);
}
void showNum(double val, char **str)
{
	asprintf(str,"Num(%lf)",val);
}
void showOld(float val, char **str)
{
	asprintf(str,"Old(%f)",val);
}
void showStr(const char* val, char **str)
{
	asprintf(str,"Str(%s)",val);
}
int hideChr(char *val, const char *str)
{
    int len,num;
    num = sscanf(str," Chr ( %c )%n",val,&len);
    return (num ? len : 0);
}
int hideInt(int *val, const char *str)
{
    int len,num;
    num = sscanf(str," Int ( %d )%n",val,&len);
    return (num ? len : 0);
}
int hideNew(long long *val, const char *str)
{
    int len,num;
    num = sscanf(str," New ( %lld )%n",val,&len);
    return (num ? len : 0);
}
int hideNum(double *val, const char *str)
{
    int len,num;
    num = sscanf(str," Num ( %lf )%n",val,&len);
    return (num ? len : 0);
}
int hideOld(float *val, const char *str)
{
    int len,num;
    num = sscanf(str," Old ( %f )%n",val,&len);
    return (num ? len : 0);
}
int hideStr(char* *val, const char *str)
{
	int base = -1;
	int lim = -1;
	int num,limit;
	num = sscanf(str," Str ( %n",&base);
	limit = base; while (num == 0 && str[limit] && lim == -1) num = sscanf(str+(++limit)," )%n",&lim);
	*val = malloc(limit-base+1);
	strncpy(*val,str+base,limit-base); (*val)[limit-base] = 0;
	return (num == 0 ? limit+lim : 0);
}

void setupLua(char **mem, const char *str, int idx)
{
	if (mem[idx]) free(mem[idx]);
	mem[idx] = malloc(strlen(str)+1);
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
int bothJumpLua(lua_State *lua)
{
	luaerr = lua;
	readJumpLua(lua);
	writeJumpLua(lua);
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
int pollInetLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,pollInet(lua_tostring(lua,1),lua_tostring(lua,2)));
	return 1;
}
int checkInetLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,checkInet(lua_tostring(lua,1),lua_tostring(lua,2)));
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
void readStrLuaFnc(const char *buf, int trm, void *arg)
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
	lua_pushcfunction(L, readNoteLua);
	lua_setglobal(L, "readNote");
	lua_pushcfunction(L, readJumpLua);
	lua_setglobal(L, "readJump");
	lua_pushcfunction(L, writeJumpLua);
	lua_setglobal(L, "writeJump");
	lua_pushcfunction(L, bothJumpLua);
	lua_setglobal(L, "bothJump");
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
