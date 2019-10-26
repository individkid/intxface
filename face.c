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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h>
#include <sys/errno.h>
#include <signal.h>
#include <setjmp.h>
#include <lua.h>
#include "face.h"

#define BUFSIZE 1024
int inp[NUMOPEN] = {0};
int out[NUMOPEN] = {0};
enum {Wait,Poll,Seek,None} vld[NUMOPEN] = {0};
pid_t pid[NUMOPEN] = {0};
int len = 0;
char buf[BUFSIZE] = {0};
jmp_buf *inpjmp[NUMOPEN] = {0};
jmp_buf *outjmp[NUMOPEN] = {0};

void readJump(jmp_buf *jmp, int idx)
{
	inpjmp[idx] = jmp;
}
void writeJump(jmp_buf *jmp, int idx)
{
	outjmp[idx] = jmp;
}
void fileJump(jmp_buf *jmp, int idx)
{
	inpjmp[idx] = outjmp[idx] = jmp;
}
void closeIdent(int idx)
{
	if (close(inp[idx]) < 0) ERROR(*inpjmp[idx])
	if (close(out[idx]) < 0) ERROR(*outjmp[idx])
}
int addPipe(int fd0, int fd1)
{
	if (len >= NUMOPEN) return -1;
	inp[len] = fd0;
	out[len] = fd1;
	vld[len] = Poll;
	int ret = len;
	len++;
	return ret;
}
int addPipeLua(lua_State *lua)
{
	lua_pushnumber(lua,addPipe((int)lua_tonumber(lua,1),lua_tointeger(lua,2)));
	return 1;
}
int addFile(int fd)
{
	if (len >= NUMOPEN) return -1;
	inp[len] = fd;
	out[len] = fd;
	vld[len] = Seek;
	int ret = len;
	len++;
	return ret;
}
int addFileLua(lua_State *lua)
{
	lua_pushnumber(lua,addFile((int)lua_tonumber(lua,1)));
	return 1;
}
void setFile(int fd, int idx)
{
	if (idx < 0 || idx >= len) ERROR(*inpjmp[idx])
	inp[idx] = fd;
	out[idx] = fd;
}
int setFileLua(lua_State *lua)
{
	setFile((int)lua_tonumber(lua,1),lua_tointeger(lua,2));
	return 0;
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
	lua_pushnumber(lua,pipeInit(lua_tostring(lua,1),lua_tostring(lua,2)));
	return 1;
}
int waitAny()
{
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
	for (int i = 0; i < len; i++) {
		if (inp[i] >= 0 && FD_ISSET(inp[i],&ers)) vld[i] = None;}
	for (int i = 0; i < len; i++) {
		if (inp[i] >= 0 && FD_ISSET(inp[i],&fds)) return i;}
	return -1;
}
int waitAnyLua(lua_State *lua)
{
	lua_pushnumber(lua,waitAny());
	return 1;
}
int pollPipe(int idx)
{
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	if (idx < 0 || idx >= len || vld[idx] == None || vld[idx] == Wait) return 0;
	if (nfd <= inp[idx]) nfd = inp[idx]+1;
	FD_SET(inp[idx],&fds); FD_SET(inp[idx],&ers);
	val = -1; while (val < 0 && errno == EINTR) val = pselect(nfd,&fds,0,&ers,0,0);
	if (val <= 0) ERROR(*inpjmp[idx])
	if (FD_ISSET(inp[idx],&fds)) return 1;
	return 0;
}
int pollPipeLua(lua_State *lua)
{
	lua_pushnumber(lua,pollPipe(lua_tointeger(lua,1)));
	return 1;
}
int todoFile(int idx)
{
	off_t pos, len;
	if (idx < 0 || idx >= len || vld[idx] != Wait) return 0;
	if ((pos = lseek(inp[idx],0,SEEK_CUR)) < 0) ERROR(*inpjmp[idx]);
	if ((len = lseek(inp[idx],0,SEEK_END)) < 0) ERROR(*inpjmp[idx]);
	if (lseek(inp[idx],pos,SEEK_CUR) < 0) ERROR(*inpjmp[idx]);
	return (len > pos);
}
int todoFileLua(lua_State *lua)
{
	lua_pushnumber(lua,todoFile(lua_tointeger(lua,1)));
	return 1;
}
int checkRead(int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return 0;
	return (inp[idx] >= 0);
}
int checkReadLua(lua_State *lua)
{
	lua_pushnumber(lua,checkRead(lua_tointeger(lua,1)));
	return 1;
}
int checkWrite(int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return 0;
	return (out[idx] >= 0);
}
int checkWriteLua(lua_State *lua)
{
	lua_pushnumber(lua,checkWrite(lua_tointeger(lua,1)));
	return 1;
}
void sleepSec(int sec)
{
	sleep(sec);
}
int sleepSecLua(lua_State *lua)
{
	sleep(lua_tointeger(lua,1));
	return 0;
}

const char *readStr(int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) {buf[0] = 0; return buf;}
	if (inp[idx] < 0) {buf[0] = 0; return buf;}
	for (int i = 0; i < BUFSIZE-1; i++) {
	int val = read(inp[idx],&buf[i],1); if (val < 0) ERROR(*inpjmp[idx])
	if (val == 0) {buf[i] = 0; vld[idx] = None;}
	if (buf[i] == 0) return buf;
	}
	buf[BUFSIZE-1] = 0;
	return buf;
}
int readStrLua(lua_State *lua)
{
	lua_pushstring(lua,readStr(lua_tointeger(lua,1)));
	return 1;
}
int readInt(int idx)
{
	int arg;
	if (idx < 0 || idx >= len || vld[idx] == None) {arg = 0; return arg;}
	if (inp[idx] < 0) {arg = 0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(int));
	if (val != 0 && val < (int)sizeof(int)) ERROR(*inpjmp[idx])
	if (val == 0) {arg = 0; vld[idx] = None;}
	return arg;
}
int readIntLua(lua_State *lua)
{
	lua_pushnumber(lua,readInt(lua_tointeger(lua,1)));
	return 1;
}
double readNum(int idx)
{
	double arg;
	if (idx < 0 || idx >= len || vld[idx] == None) {arg = 0.0; return arg;}
	if (inp[idx] < 0) {arg = 0.0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(double));
	if (val != 0 && val < (int)sizeof(double)) ERROR(*inpjmp[idx])
	if (val == 0) {arg = 0.0; vld[idx] = None;}
	return arg;
}
int readNumLua(lua_State *lua)
{
	lua_pushnumber(lua,readNum(lua_tointeger(lua,1)));
	return 1;
}
long long readNew(int idx)
{
	long long arg;
	if (idx < 0 || idx >= len || vld[idx] == None) {arg = 0; return arg;}
	if (inp[idx] < 0) {arg = 0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(long long));
	if (val != 0 && val < (int)sizeof(long long)) ERROR(*inpjmp[idx])
	if (val == 0) {arg = 0; vld[idx] = None;}
	return arg;
}
int readNewLua(lua_State *lua)
{
	lua_pushnumber(lua,readNew(lua_tointeger(lua,1)));
	return 1;
}
float readOld(int idx)
{
	float arg;
	if (idx < 0 || idx >= len || vld[idx] == None) {arg = 0.0; return arg;}
	if (inp[idx] < 0) {arg = 0.0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(float));
	if (val != 0 && val < (int)sizeof(float)) ERROR(*inpjmp[idx])
	if (val == 0) {arg = 0.0; vld[idx] = None;}
	return arg;
}
int readOldLua(lua_State *lua)
{
	lua_pushnumber(lua,readOld(lua_tointeger(lua,1)));
	return 1;
}
void writeStr(const char *arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return;
	if (out[idx] < 0) return;
	int siz = 0; while (arg[siz]) siz++;
	int val = write(out[idx],arg,siz+1);
	if (val < 0 && errno == EPIPE) vld[idx] = None;
	else if (val < siz+1) ERROR(*outjmp[idx])
}
int writeStrLua(lua_State *lua)
{
	writeStr(lua_tostring(lua,1),lua_tointeger(lua,2));
	return 0;
}
void writeInt(int arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return;
	if (out[idx] < 0) return;
	int val = write(out[idx],(char *)&arg,sizeof(int));
	if (val < 0 && errno == EPIPE) vld[idx] = None;
	else if (val < (int)sizeof(int)) ERROR(*outjmp[idx])
}
int writeIntLua(lua_State *lua)
{
	writeInt((int)lua_tonumber(lua,1),lua_tointeger(lua,2));
	return 0;
}
void writeNum(double arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return;
	if (out[idx] < 0) return;
	int val = write(out[idx],(char *)&arg,sizeof(double));
	if (val < 0 && errno == EPIPE) vld[idx] = None;
	else if (val < (int)sizeof(double)) ERROR(*outjmp[idx])
}
int writeNumLua(lua_State *lua)
{
	writeNum((double)lua_tonumber(lua,1),lua_tointeger(lua,2));
	return 0;
}
void writeNew(long long arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return;
	if (out[idx] < 0) return;
	int val = write(out[idx],(char *)&arg,sizeof(long long));
	if (val < 0 && errno == EPIPE) vld[idx] = None;
	else if (val < (int)sizeof(long long)) ERROR(*outjmp[idx])
}
int writeNewLua(lua_State *lua)
{
	writeNew((long long)lua_tonumber(lua,1),lua_tointeger(lua,2));
	return 0;
}
void writeOld(float arg, int idx)
{
	if (idx < 0 || idx >= len || vld[idx] == None) return;
	if (out[idx] < 0) return;
	int val = write(out[idx],(char *)&arg,sizeof(float));
	if (val < 0 && errno == EPIPE) vld[idx] = None;
	else if (val < (int)sizeof(float)) ERROR(*outjmp[idx])
}
int writeOldLua(lua_State *lua)
{
	writeOld((float)lua_tonumber(lua,1),lua_tointeger(lua,2));
	return 0;
}
int luaopen_face (lua_State *L)
{
	lua_pushcfunction(L, addPipeLua);
	lua_setglobal(L, "addPipe");
	lua_pushcfunction(L, addFileLua);
	lua_setglobal(L, "addFile");
	lua_pushcfunction(L, setFileLua);
	lua_setglobal(L, "setFile");
	lua_pushcfunction(L, forkExecLua);
	lua_setglobal(L, "forkExec");
	lua_pushcfunction(L, pipeInitLua);
	lua_setglobal(L, "pipeInit");
	lua_pushcfunction(L, waitAnyLua);
	lua_setglobal(L, "waitAny");
	lua_pushcfunction(L, pollPipeLua);
	lua_setglobal(L, "pollPipe");
	lua_pushcfunction(L, todoFileLua);
	lua_setglobal(L, "todoFile");
	lua_pushcfunction(L, checkReadLua);
	lua_setglobal(L, "checkRead");
	lua_pushcfunction(L, checkWriteLua);
	lua_setglobal(L, "checkWrite");
	lua_pushcfunction(L, sleepSecLua);
	lua_setglobal(L, "sleepSec");
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
