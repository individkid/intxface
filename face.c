/*
*    face.c
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <lua.h>
#include "face.h"

#define BUFSIZE 1024
int inp[NUMOPEN] = {0};
int out[NUMOPEN] = {0};
enum {Wait,Poll,Seek,None} vld[NUMOPEN] = {0};
pid_t pid[NUMOPEN] = {0};
int len = 0;
char buf[BUFSIZE+1] = {0};
eftype inperr[NUMOPEN] = {0};
eftype outerr[NUMOPEN] = {0};
char *inplua[NUMOPEN] = {0};
char *outlua[NUMOPEN] = {0};
lua_State *luaerr = 0;

void exitErr(int idx)
{
	exit(-1);
}
void setupLua(char **mem, const char *str, int idx)
{
	if (mem[idx]) free(mem[idx]);
	mem[idx] = malloc(strlen(str)+1);
	strcpy(mem[idx],str);
}
void callLua(lua_State *lua, const char *str, int idx)
{
	if (lua == 0) ERROR(exitErr,0)
	lua_getglobal(lua,str);
	lua_pushnumber(lua,idx);
	if (lua_pcall(lua, 2, 0, 0) != 0) ERROR(exitErr,0)
}
void readLua(int idx)
{
	callLua(luaerr,inplua[idx],idx);
}
void writeLua(int idx)
{
	callLua(luaerr,outlua[idx],idx);
}
void readJump(eftype err, int idx)
{
	if (idx < 0 || idx >= len) ERROR(exitErr,0)
	inperr[idx] = err;
}
int readJumpLua(lua_State *lua)
{
	setupLua(inplua,lua_tostring(lua,1),(int)lua_tonumber(lua,2));
	readJump(readLua,(int)lua_tonumber(lua,2));
	return 0;
}
void writeJump(eftype err, int idx)
{
	if (idx < 0 || idx >= len) ERROR(exitErr,0)
	outerr[idx] = err;
}
int writeJumpLua(lua_State *lua)
{
	setupLua(outlua,lua_tostring(lua,1),(int)lua_tonumber(lua,2));
	writeJump(writeLua,(int)lua_tonumber(lua,2));
	return 0;
}
void bothJump(eftype err, int idx)
{
	readJump(err,idx);
	writeJump(err,idx);
}
int bothJumpLua(lua_State *lua)
{
	readJumpLua(lua);
	writeJumpLua(lua);
	return 0;	
}
void closeIdent(int idx)
{
	if (vld[idx] != None) {
		close(inp[idx]);
		close(out[idx]);
		vld[idx] = None;
	}
	while (len > 0 && vld[len-1] == None) len--;
}
int closeIdentLua(lua_State *lua)
{
	luaerr = lua;
	closeIdent((int)lua_tonumber(lua,1));
	return 0;
}
void moveIdent(int idx0, int idx1)
{
	if (idx1 < 0 || idx1 >= len) ERROR(exitErr,0)
	if (idx0 < 0 || idx0 >= len) ERROR(exitErr,0)
	closeIdent(idx1);
	if (idx0 == len) len++;
	inp[idx1] = inp[idx0];
	out[idx1] = out[idx0];
	vld[idx1] = vld[idx0];
}
int moveIdentLua(lua_State *lua)
{
	luaerr = lua;
	moveIdent((int)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
int openIdent()
{
	if (len >= NUMOPEN) return -1;
	vld[len] = None;
	return len++;
}
int openIdentLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,openIdent());
	return 1;
}
int openPipe()
{
	int fd[2] = {0};
	if (len >= NUMOPEN) return -1;
	if (pipe(fd) < 0) return -1;
	inp[len] = fd[0];
	out[len] = fd[1];
	vld[len] = Wait;
	return len++;
}
int openPipeLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,openPipe());
	return 1;
}
int openFifo(const char *str)
{
	int fi = 0;
	int fo = 0;
	if (len >= NUMOPEN) return -1;
	if ((mkfifo(str,0666) < 0) && errno != EEXIST) return -1;
	if ((fi = open(str,O_RDONLY)) < 0) return -1;
	if ((fo = open(str,O_WRONLY)) < 0) return -1;
	inp[len] = fi;
	out[len] = fo;
	vld[len] = Poll;
	return len++;
}
int openFifoLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,openFifo(lua_tostring(lua,1)));
	return 1;
}
int openFile(const char *str)
{
	int fd = 0;
	if (len >= NUMOPEN) return -1;
	if ((fd = open(str,O_RDWR|O_CREAT,0666)) < 0) return -1;
	inp[len] = fd;
	out[len] = fd;
	vld[len] = Seek;
	return len++;
}
int openFileLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,openFile(lua_tostring(lua,1)));
	return 1;
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
	vld[len] = Wait;
	int ret = len;
	len++;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) return -1;
	return ret;
}
int forkExecLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,forkExec(lua_tostring(lua,1)));
	return 1;
}
int pipeInit(const char *av1, const char *av2)
{
	int val;
	val = sscanf(av1,"%d",&inp[len]); if (val != 1) return -1;
	val = sscanf(av2,"%d",&out[len]); if (val != 1) return -1;
	vld[len] = Wait;
	int ret = len;
	len++;
	sig_t fnc = signal(SIGPIPE,SIG_IGN); if (fnc == SIG_ERR) return -1;
	return ret;
}
int pipeInitLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,pipeInit(lua_tostring(lua,1),lua_tostring(lua,2)));
	return 1;
}
int waitAny()
{
	while (1) {
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	for (int i = 0; i < len; i++) {
		if (vld[i] == Wait && nfd <= inp[i]) nfd = inp[i]+1;
		if (vld[i] == Wait) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}}
	if (nfd == 0) return -1;
	val = -1; errno = EINTR;
	while (val < 0 && errno == EINTR) val = pselect(nfd,&fds,0,&ers,0,0);
	if (val <= 0) return -1;
	nfd = 0; for (int i = 0; i < len; i++) {
		if (vld[i] == Wait && FD_ISSET(inp[i],&ers)) {closeIdent(i); nfd++;}}
	if (nfd == 0) for (int i = 0; i < len; i++) {
		if (vld[i] == Wait && FD_ISSET(inp[i],&fds)) return i;}
	} return -1;
}
int waitAnyLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,waitAny());
	return 1;
}
int pollPipe(int idx)
{
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	if (idx < 0 || idx >= len || vld[idx] != Poll) return 0;
	if (nfd <= inp[idx]) nfd = inp[idx]+1;
	FD_SET(inp[idx],&fds); FD_SET(inp[idx],&ers);
	val = -1; while (val < 0 && errno == EINTR) val = pselect(nfd,&fds,0,&ers,0,0);
	if (val <= 0) ERROR(inperr[idx],idx)
	if (FD_ISSET(inp[idx],&fds)) return 1;
	return 0;
}
int pollPipeLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,pollPipe((int)lua_tonumber(lua,1)));
	return 1;
}
int pollFile(int idx)
{
	off_t pos, len;
	if (idx < 0 || idx >= len || vld[idx] != Wait) return 0;
	if ((pos = lseek(inp[idx],0,SEEK_CUR)) < 0) ERROR(inperr[idx],idx);
	if ((len = lseek(inp[idx],0,SEEK_END)) < 0) ERROR(inperr[idx],idx);
	if (lseek(inp[idx],pos,SEEK_CUR) < 0) ERROR(inperr[idx],idx);
	return (len > pos);
}
int pollFileLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,pollFile((int)lua_tonumber(lua,1)));
	return 1;
}
void seekFile(long long arg, int idx)
{
	off_t pos = arg;
	if (lseek(inp[idx],pos,SEEK_SET) < 0) ERROR(inperr[idx],idx);
	if (lseek(out[idx],pos,SEEK_SET) < 0) ERROR(outerr[idx],idx);
}
int seekFileLua(lua_State *lua)
{
	luaerr = lua;
	seekFile((long long)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
void truncFile(int idx)
{
	if (ftruncate(inp[idx],0) < 0) ERROR(inperr[idx],idx);
	if (ftruncate(out[idx],0) < 0) ERROR(outerr[idx],idx);
}
int truncFileLua(lua_State *lua)
{
	luaerr = lua;
	truncFile((int)lua_tonumber(lua,1));
	return 0;
}
long long sizeFile(int idx)
{
	off_t pos = 0;
	if ((pos = lseek(inp[idx],0,SEEK_SET)) < 0) ERROR(inperr[idx],idx);
	return (long long)pos;
}
int sizeFileLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,sizeFile((int)lua_tonumber(lua,1)));
	return 1;
}
void rdlkFile(long long arg0, long long arg1, int idx)
{
	off_t pos = arg0;
	off_t len = arg1;
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_RDLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLK,&lock) < 0) ERROR(inperr[idx],idx);
}
int rdlkFileLua(lua_State *lua)
{
	luaerr = lua;
	rdlkFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
void wrlkFile(long long arg0, long long arg1, int idx)
{
	off_t pos = arg0;
	off_t len = arg1;
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLK,&lock) < 0) ERROR(inperr[idx],idx);
}
int wrlkFileLua(lua_State *lua)
{
	luaerr = lua;
	wrlkFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
void unlkFile(long long arg0, long long arg1, int idx)
{
	off_t pos = arg0;
	off_t len = arg1;
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_UNLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLK,&lock) < 0) ERROR(inperr[idx],idx);
}
int unlkFileLua(lua_State *lua)
{
	luaerr = lua;
	unlkFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
void rdlkwFile(long long arg0, long long arg1, int idx)
{
	off_t pos = arg0;
	off_t len = arg1;
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_RDLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLKW,&lock) < 0) ERROR(inperr[idx],idx);
}
int rdlkwFileLua(lua_State *lua)
{
	luaerr = lua;
	rdlkwFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
void wrlkwFile(long long arg0, long long arg1, int idx)
{
	off_t pos = arg0;
	off_t len = arg1;
	struct flock lock = {0};
	lock.l_start = arg0;
	lock.l_len = arg1;
	lock.l_type = F_WRLCK;
	lock.l_whence = SEEK_SET;
	if (fcntl(inp[idx],F_SETLKW,&lock) < 0) ERROR(inperr[idx],idx);
}
int wrlkwFileLua(lua_State *lua)
{
	luaerr = lua;
	wrlkwFile((long long)lua_tonumber(lua,1),(long long)lua_tonumber(lua,2),(int)lua_tonumber(lua,3));
	return 0;
}
int checkRead(int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return 0;
	return 1;
}
int checkReadLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,checkRead((int)lua_tonumber(lua,1)));
	return 1;
}
int checkWrite(int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return 0;
	return 1;
}
int checkWriteLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,checkWrite((int)lua_tonumber(lua,1)));
	return 1;
}
void sleepSec(int sec)
{
	sleep(sec);
}
int sleepSecLua(lua_State *lua)
{
	luaerr = lua;
	sleep((int)lua_tonumber(lua,1));
	return 0;
}
char *checkStr(int pos)
{
	if (pos >= BUFSIZE) ERROR(exitErr,0)
	return buf+pos;
}
int checkStrLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushstring(lua,checkStr((int)lua_tonumber(lua,1)));
	return 1;
}
int readStr(int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	for (int i = 0; i < BUFSIZE; i++) {
		int val = read(inp[idx],&buf[i],1);
		if (val < 0) ERROR(inperr[idx],idx)
		if (val == 0) {buf[i] = 0; return i;}
		if (buf[i] == 0) return i+1;
	}
	buf[BUFSIZE] = 0; return BUFSIZE;
}
int readStrLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readStr((int)lua_tonumber(lua,1)));
	return 1;
}
int readInt(int idx)
{
	int arg;
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	int val = read(inp[idx],(char *)&arg,sizeof(int));
	if (val != 0 && val < (int)sizeof(int)) ERROR(inperr[idx],idx)
	if (val == 0) {arg = 0; closeIdent(idx);}
	return arg;
}
int readIntLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readInt((int)lua_tonumber(lua,1)));
	return 1;
}
double readNum(int idx)
{
	double arg;
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	int val = read(inp[idx],(char *)&arg,sizeof(double));
	if (val != 0 && val < (int)sizeof(double)) ERROR(inperr[idx],idx)
	if (val == 0) {arg = 0.0; closeIdent(idx);}
	return arg;
}
int readNumLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readNum((int)lua_tonumber(lua,1)));
	return 1;
}
long long readNew(int idx)
{
	long long arg;
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	if (inp[idx] < 0) {arg = 0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(long long));
	if (val != 0 && val < (int)sizeof(long long)) ERROR(inperr[idx],idx)
	if (val == 0) {arg = 0; closeIdent(idx);}
	return arg;
}
int readNewLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readNew((int)lua_tonumber(lua,1)));
	return 1;
}
float readOld(int idx)
{
	float arg;
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	if (inp[idx] < 0) {arg = 0.0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(float));
	if (val != 0 && val < (int)sizeof(float)) ERROR(inperr[idx],idx)
	if (val == 0) {arg = 0.0; closeIdent(idx);}
	return arg;
}
int readOldLua(lua_State *lua)
{
	luaerr = lua;
	lua_pushnumber(lua,readOld((int)lua_tonumber(lua,1)));
	return 1;
}
void writeStr(const char *arg, int siz, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],arg,siz);
	if (val < siz) ERROR(outerr[idx],idx)
}
int writeStrLua(lua_State *lua)
{
	luaerr = lua;
	writeStr(lua_tostring(lua,1),strlen(lua_tostring(lua,1))+1,(int)lua_tonumber(lua,2));
	return 0;
}
void writeInt(int arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(int));
	if (val < (int)sizeof(int)) ERROR(outerr[idx],idx)
}
int writeIntLua(lua_State *lua)
{
	luaerr = lua;
	writeInt((int)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
void writeNum(double arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(double));
	if (val < (int)sizeof(double)) ERROR(outerr[idx],idx)
}
int writeNumLua(lua_State *lua)
{
	luaerr = lua;
	writeNum((double)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
void writeNew(long long arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(long long));
	if (val < (int)sizeof(long long)) ERROR(outerr[idx],idx)
}
int writeNewLua(lua_State *lua)
{
	luaerr = lua;
	writeNew((long long)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
void writeOld(float arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) ERROR(exitErr,0)
	int val = write(out[idx],(char *)&arg,sizeof(float));
	if (val < (int)sizeof(float)) ERROR(outerr[idx],idx)
}
int writeOldLua(lua_State *lua)
{
	luaerr = lua;
	writeOld((float)lua_tonumber(lua,1),(int)lua_tonumber(lua,2));
	return 0;
}
int luaopen_face (lua_State *L)
{
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
	lua_pushcfunction(L, openIdentLua);
	lua_setglobal(L, "openIdent");
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
	lua_pushcfunction(L, pollPipeLua);
	lua_setglobal(L, "pollPipe");
	lua_pushcfunction(L, pollFileLua);
	lua_setglobal(L, "pollFile");
	lua_pushcfunction(L, seekFileLua);
	lua_setglobal(L, "seekFile");
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
	lua_pushcfunction(L, checkStrLua);
	lua_setglobal(L, "checkStr");
	lua_pushcfunction(L, readStrLua);
	lua_setglobal(L, "readStr");
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
