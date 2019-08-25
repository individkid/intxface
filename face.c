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
#include <lua.h>

#define ERROR {fprintf(stderr,"%s(%d): %d\n",__FILE__,__LINE__,errno);exit(-1);}

int inp = -1;
int out = -1;

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
		ERROR
	}
	val = close(ifd[1]); if (val < 0) ERROR
	val = close(ofd[0]); if (val < 0) ERROR
	inp = ifd[0];
	out = ofd[1];
}

void pipeInit(int argc, char **argv)
{
	int val;
	if (argc != 3) ERROR
	val = sscanf(argv[1],"%d",&out); if (val != 1) ERROR
	val = sscanf(argv[2],"%d",&inp); if (val != 1) ERROR
}

#define BUFSIZE 1024

char buf[BUFSIZE];
int len = 0;

void buff(int siz)
{
	if (siz > BUFSIZE) ERROR
	if (len < siz) {
		int val;
		int nfd = inp+1;
		fd_set fds; FD_ZERO(&fds); FD_SET(inp,&fds);
		struct timespec tsp; tsp.tv_sec = 0; tsp.tv_nsec = 0;
		val = pselect(nfd,&fds,0,&fds,&tsp,0); if (val < 0) ERROR
		if (val == 0) return;
		val = read(inp,buf+len,siz-len); if (val < 0) ERROR
		len += val;
	}
}

void copy(int siz, char *chs)
{
	for (int i = 0; i < len; i++)
	if (i < siz) chs[i] = buf[i];
	else buf[i-siz] = buf[i];
	len -= siz;
}

int poll(int siz, char *chs)
{
	buff(siz);
	if (len < siz) return 0;
	copy(siz,chs);
	return 1;
}

int pollString(int *siz, char *val)
{
	for (int i = 0; i < *siz; i++) {
		if (i >= len) buff(len+1);
		if (buf[i] == 0) {
			copy(i+1,val);
			*siz = i+1;
			return 1;
		}
	}
	return 0;
}

int pollInt(int *val) {return poll(sizeof(int),(char *)val);}
int pollNum(double *val) {return poll(sizeof(double),(char *)val);}
int pollInts(int siz, int *val) {return poll(siz*sizeof(int),(char *)val);}
int pollNums(int siz, double *val) {return poll(siz*sizeof(double),(char *)val);}

#define POLLLUA(NAME,TYPE,FUNC) \
int poll##NAME##Lua(lua_State *lua) \
{ \
	TYPE val; \
	if (poll##NAME(&val) == 0) return 0; \
	lua_push##FUNC(lua,val); \
	return 1; \
}

#define POLLSLUA(NAME,TYPE,FUNC) \
int poll##NAME##sLua(lua_State *lua) \
{ \
	if (!lua_isnumber(lua,1)) ERROR \
	int siz = lua_tonumber(lua,1); \
	TYPE val[siz]; \
	if (poll##NAME##s(siz,val) == 0) return 0; \
	for (int i = 0; i < siz; i++) lua_push##FUNC(lua,val[i]); \
	return siz; \
}

int pollStringLua(lua_State *lua)
{
	char val[BUFSIZE];
	int siz = BUFSIZE;
	if (pollString(&siz,val) == 0) return 0;
	lua_pushstring(lua,val);
	return 1;
}

POLLLUA(Int,int,integer)
POLLLUA(Num,double,number)
POLLSLUA(Int,int,integer)
POLLSLUA(Num,double,number)

void fill(int siz)
{
	if (siz > BUFSIZE) ERROR
	while (len < siz) {
		int val;
		val = read(inp,buf+len,siz-len); if (val < 0) ERROR
		len += val;
	}
}

void recv(int siz, char *chs)
{
	fill(siz);
	copy(siz,chs);
}

void readString(int siz, char *val)
{
	for (int i = 0; i < siz; i++) {
		if (i >= len) fill(len+1);
		if (buf[i] == 0) {
			copy(i+1,val);
			break;
		}
	}
}

void readInt(int *val) {recv(sizeof(int),(char *)val);}
void readNum(double *val) {recv(sizeof(double),(char *)val);}
void readInts(int siz, int *val) {recv(siz*sizeof(int),(char *)val);}
void readNums(int siz, double *val) {recv(siz*sizeof(double),(char *)val);}

#define READLUA(NAME,TYPE,FUNC) \
int read##NAME##Lua(lua_State *lua) \
{ \
	TYPE val; \
	read##NAME(&val); \
	lua_push##FUNC(lua,val); \
	return 1; \
}

#define READSLUA(NAME,TYPE,FUNC) \
int read##NAME##sLua(lua_State *lua) \
{ \
	if (!lua_isnumber(lua,1)) ERROR \
	int siz = lua_tonumber(lua,1); \
	TYPE val[siz]; \
	read##NAME##s(siz,val); \
	for (int i = 0; i < siz; i++) lua_push##FUNC(lua,val[i]); \
	return siz; \
}

int readStringLua(lua_State *lua)
{
	char val[BUFSIZE];
	int siz = BUFSIZE;
	readString(siz,val);
	lua_pushstring(lua,val);
	return 1;
}

READLUA(Int,int,integer)
READLUA(Num,double,number)
READSLUA(Int,int,integer)
READSLUA(Num,double,number)

void send(int siz, const char *chs)
{
	int val;
	val = write(out,chs,siz); if (val < 0) ERROR
}

void writeString(const char *val)
{
	int siz = 0;
	while (val[siz]) siz++;
	send(siz+1,val);
}

void writeInt(int val) {send(sizeof(int),(char *)&val);}
void writeNum(double val) {send(sizeof(double),(char *)&val);}
void writeInts(int siz, int *val) {send(siz*sizeof(int),(char *)val);}
void writeNums(int siz, double *val) {send(siz*sizeof(double),(char *)val);}

#define WRITELUA(NAME,TYPE,FUNC) \
int write##NAME##Lua(lua_State *lua) \
{ \
	if (!lua_is##FUNC(lua,1)) ERROR \
	TYPE val = lua_to##FUNC(lua,1); \
	write##NAME(val); \
	return 0; \
}

#define WRITESLUA(NAME,TYPE,FUNC) \
int write##NAME##sLua(lua_State *lua) \
{ \
	if (!lua_isnumber(lua,1)) ERROR \
	int siz = lua_tonumber(lua,1); \
	TYPE val[siz]; \
	for (int i = 0; i < siz; i++) { \
		if (!lua_is##FUNC(lua,i+2)) ERROR \
		val[i] = lua_to##FUNC(lua,i+2); \
	} \
	write##NAME##s(siz,val); \
	return 0; \
}

int writeStringLua(lua_State *lua)
{
	if (!lua_isstring(lua,1)) ERROR
	const char *val = lua_tostring(lua,1);
	writeString(val);
	return 0;
}

WRITELUA(Int,int,integer)
WRITELUA(Num,double,number)
WRITESLUA(Int,int,integer)
WRITESLUA(Num,double,number)
