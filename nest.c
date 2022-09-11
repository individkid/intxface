#include "nest.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <string.h>
#include <regex.h>
#include <lua.h>

#define ERROR(FNC,ARG) {if (FNC) FNC(__FILE__,__LINE__,ARG); else {fprintf(stderr,"%s(%d): %d %lld\n",__FILE__,__LINE__,errno,(long long)getpid()); exit(-1);}}

// lua function state
struct Enter {
	int idx; // index into string array
	int pos; // expression in string
	int wrp; // whether resume completed
	char *exp; // copy of exp in string
	char *str; // result of last evaluation
	lua_State *lua; // subcontext for expression
};
struct Enter *ent = 0; // expressions of line
int dim = 0; // number of expressions in line
const char **line = 0; // strings of line
char **rslt = 0; // with expressions replaced
int mline = 0; // number of strings in line
int msize = 0; // size of string array
lua_State *luastate = 0;
lua_State *luatemp = 0;

void *nestLua(void *ud, void *ptr, size_t osize, size_t nsize)
{
	if (nsize == 0) {free(ptr); return 0;}
	return realloc(ptr, nsize);
}
const char *nestReader(lua_State *L, void *data, size_t *size)
{
	struct Enter *ent = (struct Enter *)data;
	if (ent->wrp == 0) {*size = 0; return 0;}
	*size = strlen(ent->exp);
	ent->wrp = 0;
	return ent->exp;
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
void nestFunc(const char *str, enum Prototype pro, union Proto typ)
{
	if (!luastate) luastate = lua_newstate(nestLua,0);
	lua_pushinteger(luastate, pro);
	lua_pushlightuserdata(luastate, typ.vp);
	lua_pushcclosure(luastate, nestClosure, 2);
	lua_setglobal(luastate, str);
}
void nestDone(int siz)
{
	if (!luastate) luastate = lua_newstate(nestLua,0);
	for (int i = 0; i < mline; i++) if (rslt[i]) free(rslt[i]); mline = 0;
	if (msize != siz && msize == 0) {line = malloc(siz); rslt = malloc(siz); msize = siz;}
	else if (msize != siz && siz == 0) {free(line); free(rslt); line = 0; msize = 0;}
	else if (msize != siz) {line = realloc(line,siz); rslt = realloc(rslt,siz); msize = siz;}
	for (int i = 0; i < msize; i++) rslt[i] = 0;
	for (int i = 0; i < dim; i++) if (ent[i].str) free(ent[i].str);
	if (dim != 0) {free(ent); ent = 0; dim = 0;}
	lua_settop(luastate,0);
}
void nestElem(const char *str)
{
	if (mline == msize) ERROR(exitErr,0);
	line[mline++] = str;
}
void nestLine()
{
	const char *exp = 0;
	regex_t exprex = {0};
	regmatch_t match[5];
	if (!luastate) luastate = lua_newstate(nestLua,0);
	if (dim != 0) ERROR(exitErr,0);
	if (regcomp(&exprex,"(.*)%(",REG_MINIMAL) != 0) ERROR(exitErr,0);
	for (int i = 0; i < mline; i++) for (dim = 0, exp = line[i]; regexec(&exprex,exp,2,match,0) == 0; dim += 1) {
	exp += match[1].rm_eo + 2; for (int nst = 1; nst; nst += nestSkip(&exp));}
	if (dim) ent = malloc((dim)*sizeof(struct Enter));
	for (int i = 0; i < mline; i++) for (dim = 0, exp = line[i]; regexec(&exprex,exp,2,match,0) == 0; dim += 1) {
		const char *bas = 0;
		const char *lim = 0;
		bas = exp += match[1].rm_eo + 2; for (int nst = 1; nst; nst += nestSkip(&exp));
		lim = exp - 1;
		ent[dim].idx = i;
		ent[dim].pos = bas-line[i];
		ent[dim].wrp = 1;
		ent[dim].exp = malloc(lim-bas+1);
		strncpy(ent[dim].exp,bas,lim-bas);
		ent[dim].exp[lim-bas] = 0;
		ent[dim].str = 0;
		ent[dim].lua = lua_newthread(luastate);}
}
int nestPass()
{
	int sub = 0;
	if (!luastate) luastate = lua_newstate(nestLua,0);
	for (sub = dim-1; sub > 0 && ent[sub].wrp; sub--);
	if (sub == 0 && ent[sub].wrp && ent[sub].str != 0) return 0;
	while (sub < dim) {
		int nrslt = 0;
		const char *str = 0;
		int ret = 0;
		if (ent[sub].wrp) lua_load(ent[sub].lua,nestReader,(void*)(ent+sub),"","bt");
		ret = lua_resume(ent[sub].lua,luastate,0,&nrslt);
		if (ret == LUA_OK) ent[sub].wrp = 1;
		else if (ret == LUA_YIELD) ent[sub].wrp = 0;
		else ERROR(exitErr,0);
		if (nrslt != 1) ERROR(exitErr,0);
		str = lua_tostring(ent[sub].lua,1);
		ent[sub].str = realloc(ent[sub].str,strlen(str)+1);
		strcpy(ent[sub].str,str);
		lua_pop(ent[sub].lua,1); // pop str
		if (ent[sub].wrp) lua_pop(ent[sub].lua,1); // pop exp
		sub++;}
	return 1;
}
const char *nestNext(int i)
{
	if (i < 0 || i >= mline) ERROR(exitErr,0);
	return rslt[i];
}
int nestScript(int *val, const char *str, int arg)
{
	if (!luastate) luastate = lua_newstate(nestLua,0);
	lua_getglobal(luastate,"load");
	lua_pushstring(luastate,str);
	if (lua_pcall(luastate, 1, 1, 0) != 0) ERROR(exitErr,0)
	lua_pushnumber(luastate,arg);
	if (lua_pcall(luastate, 1, 1, 0) != 0) ERROR(exitErr,0)
	*val = lua_tonumber(luastate,1);
	lua_pop(luastate,1);
	return 1;
}

int nestScriptLua(lua_State *L)
{
	int val = 0;
	lua_pushnumber(L,nestScript(&val, lua_tostring(L,1), lua_tonumber(L,2)));
	lua_pushnumber(L,val);
	return 2;
}

int luaopen_nest(lua_State *L)
{
	lua_pushcfunction(L, nestScriptLua);
	lua_setglobal(L, "nestScript");
	return 0;
}