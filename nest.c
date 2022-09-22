#include "nest.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <string.h>
#include <regex.h>
#include <lua.h>

struct Enter {
	int idx; // index into string array
	int pos; // expression in string
	int wrp; // whether resume completed
	char *exp; // copy of exp in string
	char *str; // copy of last result
	lua_State *lua; // subcontext for expression
};
struct Enter *ent = 0; // expressions of line
int dim = 0; // number of expressions in line
const char **line = 0; // strings of line
char **rslt = 0; // with expressions replaced
int lsiz = 0; // number of strings in line
lua_State *luastate = 0;

void *nestLua(void *ud, void *ptr, size_t osize, size_t nsize)
{
	if (nsize == 0) {free(ptr); return 0;}
	return realloc(ptr, nsize);
}
const char *nestReader(lua_State *L, void *data, size_t *size)
{
	const char *exp = (const char *)data;
	*size = strlen(exp);
	return exp;
}
int nestSkip(const char **str)
 {
	char *bas = strchr(*str,'(');
	char *lim = strchr(*str,')');
	if (!lim) ERROR(exitErr,0);
	if (bas && bas < lim) {*str = bas+1; return 1;}
	if (!bas || lim < bas) {*str = lim+1; return -1;}
	return 0;
}
int nestClosure(lua_State *L)
{
	enum Prototype pro = lua_tointeger(L, lua_upvalueindex(1));
	union Proto typ = {0};
	typ.vp = lua_touserdata(L, lua_upvalueindex(2));
	switch (pro) {
		case (Lftype): {
			int val = 0;
			int ret = 0;
			int len = lua_type(L,3)==LUA_TNONE?0:lua_tointeger(L,3);
			ret = typ.lf(&val,lua_tostring(L,1),lua_tostring(L,2),&len);
			lua_pushinteger(L,val);
			lua_pushinteger(L,len);
			lua_pushinteger(L,ret);
			return 3;}
		default: return 0;}
}
int nestScript(int *val, const char *exp, int arg)
{
	int ret = 0;
	if (!luastate) luastate = lua_newstate(nestLua,0);
	lua_load(luastate,nestReader,(void*)exp,"","bt");
	lua_pushinteger(luastate,arg);
	ret = lua_pcall(luastate,1,1,0);
	*val = lua_tonumber(luastate,-1);
	lua_pop(luastate,1);
	return (ret == LUA_OK);
}
void nestFunc(const char *str, enum Prototype pro, union Proto typ)
{
	if (!luastate) luastate = lua_newstate(nestLua,0);
	lua_pushinteger(luastate, pro);
	lua_pushlightuserdata(luastate, typ.vp);
	lua_pushcclosure(luastate, nestClosure, 2);
	lua_setglobal(luastate, str);
}
void nestInit(int siz)
{
	for (int i = 0; i < lsiz; i++) if (rslt[i]) {free(rslt[i]); rslt[i] = 0;}
	rslt = realloc(rslt,siz); line = realloc(line,siz); lsiz = siz;
}
void nestFree()
{
	if (!luastate) luastate = lua_newstate(nestLua,0);
	for (int i = 0; i < dim; i++) if (ent[i].str) free(ent[i].str);
	for (int i = 0; i < dim; i++) if (ent[i].exp) free(ent[i].exp);
	if (dim != 0) {free(ent); ent = 0; dim = 0;}
	lua_settop(luastate,0);	
}
void nestElem(int i, const char *str)
{
	if (i < 0 || i >= lsiz) ERROR(exitErr,0);
	if (dim) nestFree();
	line[i] = str;
}
int nestScan()
{
	if (!luastate) luastate = lua_newstate(nestLua,0);
	if (dim) nestFree();	
	for (int i = 0; i < lsiz; i++)
	for (const char *str = line[i]; *(str = strstr(str,"%(")) && *(str += 2); dim++)
	for (int nst = 1; nst; nst += nestSkip(&str));
	ent = realloc(ent,dim); dim = 0;
	for (int i = 0; i < lsiz; i++)
	for (const char *str = line[i]; *(str = strstr(str,"%(")) && *(str += 2); dim++) {
		int pos, len;
		ent[dim].pos = pos = str-line[i];
		for (int nst = 1; nst; nst += nestSkip(&str));
		len = str-line[i]-pos-1;
		ent[dim].exp = realloc(ent[dim].exp,len+1);
		strncpy(ent[dim].exp,line[i]+pos,len);
		ent[dim].exp[len] = 0;}
	for (int i = 0; i < dim; i++) {
		ent[i].wrp = 0;
		ent[i].str = 0;
		ent[i].lua = lua_newthread(luastate);}
	return dim;
}
int nestEval(int i)
{
	int num = 0;
	int len = 0;
	if (i < 0 || i >= dim) ERROR(exitErr,0);
	if (!luastate) luastate = lua_newstate(nestLua,0);
	if (!ent[i].wrp) lua_load(ent[i].lua,nestReader,(void*)(ent[i].exp),"","bt");
	ent[i].wrp = (lua_resume(luastate,ent[i].lua,0,&num) == LUA_YIELD);
	for (int j = 0; j < num; j++) len += strlen(lua_tostring(ent[i].lua,j-num));
	ent[i].str = realloc(ent[i].str,len+1); ent[i].str[0] = 0;
	for (int j = 0; j < num; j++) strcat(ent[i].str,lua_tostring(ent[i].lua,j-num));
	lua_pop(ent[i].lua,num);
	return ent[i].wrp;
}
int nestPass()
{
	int retval = 0;
	for (int i = 0; i < dim; i++) retval |= nestEval(i);
	return 0;
}
const char *nestRepl(int i)
{
	int length = 0;
	int pos = 0;
	if (i < 0 || i >= lsiz) ERROR(exitErr,0);
	length = strlen(line[i]);
	for (int j = 0; j < dim; j++) if (ent[j].idx == i) {
	length -= strlen(ent[j].exp)+3; length += strlen(ent[j].str);}
	rslt[i] = realloc(rslt[i],length+1); length = 0;
	for (int j = 0; j < dim; j++) if (ent[j].idx == i) {
		int len = ent[j].pos-pos-2;
		strncpy(rslt[i]+length,line[i]+pos,len); length += len; pos += len+strlen(ent[j].exp)+3;
		strcat(rslt[i]+length,ent[j].str); length += strlen(ent[j].str);}
	return rslt[i];
}

int nestInitLua(lua_State *L)
{
	nestInit(lua_tonumber(L,1));
	return 0;
}
int nestElemLua(lua_State *L)
{
	nestElem((int)lua_tonumber(L,1),lua_tostring(L,2));
	return 0;
}
int nestScanLua(lua_State *L)
{
	lua_pushnumber(L,nestScan());
	return 1;
}
int nestEvalLua(lua_State *L)
{
	lua_pushnumber(L,nestEval((int)lua_tonumber(L,1)));
	return 1;
}
int nestPassLua(lua_State *L)
{
	lua_pushnumber(L,nestPass());
	return 1;
}
int nestReplLua(lua_State *L)
{
	lua_pushstring(L,nestRepl((int)lua_tonumber(L,1)));
	return 1;
}

int luaopen_nest(lua_State *L)
{
	lua_pushcfunction(L, nestInitLua);
	lua_setglobal(L, "nestInit");
	lua_pushcfunction(L, nestElemLua);
	lua_setglobal(L, "nestElem");
	lua_pushcfunction(L, nestScanLua);
	lua_setglobal(L, "nestScan");
	lua_pushcfunction(L, nestEvalLua);
	lua_setglobal(L, "nestEval");
	lua_pushcfunction(L, nestPassLua);
	lua_setglobal(L, "nestPass");
	lua_pushcfunction(L, nestReplLua);
	lua_setglobal(L, "nestRepl");
	return 0;
}