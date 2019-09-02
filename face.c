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
#include "face.h"

#define ERROR {fprintf(stderr,"%s(%d): %d\n",__FILE__,__LINE__,errno);exit(-1);}
#define BUFSIZE 1024

int inp[BUFSIZE];
int out[BUFSIZE];
pid_t pid[BUFSIZE];
int len = 0;
char buf[BUFSIZE];

void forkExec(const char *exe)
{
	int c2p[2], p2c[2], val;
	val = pipe(c2p); if (val < 0) ERROR
	val = pipe(p2c); if (val < 0) ERROR
	pid[len] = fork(); if (pid[len] < 0) ERROR
	if (pid[len] == 0) {
		char ist[33], ost[33], idt[33];
		val = close(c2p[0]); if (val < 0) ERROR
		val = close(p2c[1]); if (val < 0) ERROR
		val = snprintf(ost,32,"%d",c2p[1]); if (val < 0 || val > 32) ERROR
		val = snprintf(ist,32,"%d",p2c[0]); if (val < 0 || val > 32) ERROR
		val = snprintf(idt,32,"%d",len); if (val < 0 || val > 32) ERROR
		val = execl(exe,exe,ist,ost,idt,0); if (val < 0) ERROR
		ERROR}
	val = close(c2p[1]); if (val < 0) ERROR
	val = close(p2c[0]); if (val < 0) ERROR
	inp[len] = c2p[0];
	out[len] = p2c[1];
	len++;
}
int forkExecLua(lua_State *lua)
{
	forkExec(lua_tostring(lua,1));
	return 0;
}
void pipeInit(const char *av1, const char *av2)
{
	int val;
	val = sscanf(av1,"%d",&inp[len]); if (val != 1) ERROR
	val = sscanf(av2,"%d",&out[len]); if (val != 1) ERROR
	len++;
}
int pipeInitLua(lua_State *lua)
{
	pipeInit(lua_tostring(lua,1),lua_tostring(lua,2));
	return 0;
}
int waitAny()
{
	int val;
	int nfd = 0;
	fd_set fds, ers; FD_ZERO(&fds); FD_ZERO(&ers);
	for (int i = 0; i < len; i++) {
		if (nfd <= inp[i]) nfd = inp[i]+1;
		if (inp[i] >= 0) {FD_SET(inp[i],&fds); FD_SET(inp[i],&ers);}}
	if (nfd == 0) return len;
	val = -1; while (val < 0) val = pselect(nfd,&fds,0,&ers,0,0);
	if (val == 0 || (val < 0 && errno != EINTR)) ERROR
	for (int i = 0; i < len; i++) {
		if (inp[i] >= 0 && FD_ISSET(inp[i],&fds)) return i;}
	for (int i = 0; i < len; i++) {
		if (inp[i] >= 0 && FD_ISSET(inp[i],&ers)) inp[i] = -1;}
	return len;
}
int waitAnyLua(lua_State *lua)
{
	lua_pushinteger(lua,waitAny());
	return 1;
}
int checkRead(int idx)
{
	return (inp[idx] >= 0);
}
int checkReadLua(lua_State *lua)
{
	lua_pushinteger(lua,checkRead(lua_tointeger(lua,1)));
	return 1;
}
int checkWrite(int idx)
{
	return (out[idx] >= 0);
}
int checkWriteLua(lua_State *lua)
{
	lua_pushinteger(lua,checkWrite(lua_tointeger(lua,1)));
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

const char *readString(int idx)
{
	if (inp[idx] < 0) {buf[0] = 0; return buf;}
	for (int i = 0; i < BUFSIZE-1; i++) {
	int val = read(inp[idx],&buf[i],1); if (val < 0) ERROR
	if (val == 0) {buf[i] = 0; inp[idx] = -1;}
	if (buf[i] == 0) return buf;
	}
	buf[BUFSIZE-1] = 0;
	return buf;
}
int readStringLua(lua_State *lua)
{
	lua_pushstring(lua,readString(lua_tointeger(lua,1)));
	return 1;
}
int readInt(int idx)
{
	int arg;
	if (inp[idx] < 0) {arg = 0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(int));
	if (val != 0 && val < (int)sizeof(int)) ERROR
	if (val == 0) {arg = 0; inp[idx] = -1;}
	return arg;
}
int readIntLua(lua_State *lua)
{
	lua_pushinteger(lua,readInt(lua_tointeger(lua,1)));
	return 1;
}
double readNum(int idx)
{
	double arg;
	if (inp[idx] < 0) {arg = 0.0; return arg;}
	int val = read(inp[idx],(char *)&arg,sizeof(double));
	if (val != 0 && val < (int)sizeof(double)) ERROR
	if (val == 0) {arg = 0.0; inp[idx] = -1;}
	return arg;
}
int readNumLua(lua_State *lua)
{
	lua_pushnumber(lua,readNum(lua_tointeger(lua,1)));
	return 1;
}
void writeString(const char *arg, int idx)
{
	if (out[idx] < 0) return;
	int siz = 0; while (arg[siz]) siz++;
	int val = write(out[idx],arg,siz+1);
	if (val < 0 && errno == EPIPE) out[idx] = -1;
	else if (val < siz+1) ERROR
}
int writeStringLua(lua_State *lua)
{
	writeString(lua_tostring(lua,2),lua_tointeger(lua,1));
	return 0;
}
void writeInt(int arg, int idx)
{
	if (out[idx] < 0) return;
	int val = write(out[idx],(char *)&arg,sizeof(int));
	if (val < 0 && errno == EPIPE) out[idx] = -1;
	else if (val < (int)sizeof(int)) ERROR
}
int writeIntLua(lua_State *lua)
{
	writeInt(lua_tointeger(lua,2),lua_tointeger(lua,1));
	return 0;
}
void writeNum(double arg, int idx)
{
	if (out[idx] < 0) return;
	int val = write(out[idx],(char *)&arg,sizeof(double));
	if (val < 0 && errno == EPIPE) out[idx] = -1;
	else if (val < (int)sizeof(double)) ERROR
}
int writeNumLua(lua_State *lua)
{
	writeNum(lua_tonumber(lua,2),lua_tointeger(lua,1));
	return 0;
}
