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
int num = 0;

void forkExec(const char *exe)
{
	int ifd[2], ofd[2], val;
	pid_t pid;
	val = pipe(ifd); if (val < 0) ERROR
	val = pipe(ofd); if (val < 0) ERROR
	pid = fork(); if (pid < 0) ERROR
	if (pid == 0) {
		char ist[33], ost[33];
		val = close(ifd[0]); if (val < 0) ERROR
		val = close(ofd[1]); if (val < 0) ERROR
		val = snprintf(ist,32,"%d",ifd[1]); if (val < 0 || val > 32) ERROR
		val = snprintf(ost,32,"%d",ofd[0]); if (val < 0 || val > 32) ERROR
		val = execl(exe,exe,ist,ost,0); if (val < 0) ERROR
		ERROR}
	val = close(ifd[1]); if (val < 0) ERROR
	val = close(ofd[0]); if (val < 0) ERROR
	inp[num] = ifd[0];
	out[num] = ofd[1];
	num++;
}
int forkExecLua(lua_State *lua)
{
	forkExec(lua_tostring(lua,1));
	return 1;
}
void pipeInit(const char *av1, const char *av2)
{
	int val;
	val = sscanf(av1,"%d",&out[num]); if (val != 1) ERROR
	val = sscanf(av2,"%d",&inp[num]); if (val != 1) ERROR
	num++;
}
int pipeInitLua(lua_State *lua)
{
	pipeInit(lua_tostring(lua,1),lua_tostring(lua,2));
	return 1;
}

int pollAny()
{
	int val;
	int nfd = 0;
	fd_set fds; FD_ZERO(&fds);
	for (int i = 0; i < num; i++) {
		if (nfd <= inp[i]) nfd = inp[i]+1;
		FD_SET(inp[i],&fds);}
	struct timespec tsp; tsp.tv_sec = 0; tsp.tv_nsec = 0;
	val = pselect(nfd,&fds,0,&fds,&tsp,0); if (val < 0) ERROR
	if (val == 0) return -1;
	for (int i = 0; i < num; i++) {
		if (FD_ISSET(inp[i],&fds)) return i;}
	ERROR return -1;
}
int pollAnyLua(lua_State *lua)
{
	lua_pushinteger(lua,pollAny());
	return 1;
}
int waitAny()
{
	int val;
	int nfd = 0;
	fd_set fds; FD_ZERO(&fds);
	for (int i = 0; i < num; i++) {
		if (nfd <= inp[i]) nfd = inp[i]+1;
		FD_SET(inp[i],&fds);}
	val = pselect(nfd,&fds,0,&fds,0,0); if (val < 0) ERROR
	for (int i = 0; i < num; i++) {
		if (FD_ISSET(inp[i],&fds)) return i;}
	ERROR return -1;
}
int waitAnyLua(lua_State *lua)
{
	lua_pushinteger(lua,waitAny());
	return 1;
}

void readString(char *arg, int idx)
{
	while (1) {
	int val = read(inp[idx],arg,1); if (val < 1) ERROR
	if (*arg == 0) break;
	arg++;}
}
int readStringLua(lua_State *lua)
{
	char val[BUFSIZE]; readString(val,lua_tointeger(lua,1));
	lua_pushstring(lua,val);
	return 1;
}
void readInt(int *arg, int idx)
{
	int val = read(inp[idx],(char *)arg,sizeof(int)); if (val < sizeof(int)) ERROR
}
int readIntLua(lua_State *lua)
{
	int val; readInt(&val,lua_tointeger(lua,1));
	lua_pushinteger(lua,val);
	return 1;
}
void readNum(double *arg, int idx)
{
	int val = read(inp[idx],(char *)arg,sizeof(double)); if (val < sizeof(double)) ERROR
}
int readNumLua(lua_State *lua)
{
	double val; readNum(&val,lua_tointeger(lua,1));
	lua_pushnumber(lua,val);
	return 1;
}
void writeString(const char *arg, int idx)
{
	int siz = 0; while (arg[siz]) siz++;
	int val = write(out[idx],arg,siz+1); if (val < 1) ERROR
}
int writeStringLua(lua_State *lua)
{
	writeString(lua_tostring(lua,2),lua_tointeger(lua,1));
	return 1;
}
void writeInt(int arg, int idx)
{
	int val = write(out[idx],(char *)&arg,sizeof(int)); if (val < sizeof(int)) ERROR
}
int writeIntLua(lua_State *lua)
{
	writeInt(lua_tointeger(lua,2),lua_tointeger(lua,1));
	return 1;
}
void writeNum(double arg, int idx)
{
	int val = write(out[idx],(char *)&arg,sizeof(double)); if (val < sizeof(double)) ERROR
}
int writeNumLua(lua_State *lua)
{
	writeNum(lua_tonumber(lua,2),lua_tointeger(lua,1));
	return 1;
}
