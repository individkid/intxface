#include "luax.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <string.h>
#include <regex.h>
#include <lua.h>
#include <lualib.h>

struct Fiber {
	int idx; // index into string array
	int pos; // expression in string
	char *exp; // copy of exp in string
	char *str; // copy of last result
	lua_State *lua; // subcontext for expression
	int top; // location of subcontext
};
struct Reader {
   int vld;
   char *str;
};
struct Fiber *fiber = 0; // expressions of line
int dim = 0; // number of expressions in line
char **line = 0; // strings of line
char **rslt = 0; // with expressions replaced
int lsiz = 0; // number of strings in line
lua_State *luastate = 0;

void *luaxLua(void *ud, void *ptr, size_t osize, size_t nsize)
{
	if (nsize == 0) {free(ptr); return 0;}
	return realloc(ptr, nsize);
}
const char *luaxReader(lua_State *L, void *data, size_t *size)
{
	struct Reader *reader = (struct Reader *)data;
	if (!reader->vld) {*size = 0; return 0;}
	*size = strlen(reader->str);
	reader->vld = 0;
	return reader->str;
}
void luaxLoad(lua_State *luastate, const char *exp) // TODO return error code
{
	int retval = 0;
	struct Reader reader = {0};
	asprintf(&reader.str,"%s",exp);
	reader.vld = 1;
	retval = lua_load(luastate,luaxReader,(void*)&reader,"","t");
	free(reader.str);
	if (retval != LUA_OK) {
		printf("lua %s\n",lua_tostring(luastate,-1));
		ERROR(exitErr,0);}
}
int luaxSide(const char *exp)
{
	int ret = 0;
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	luaxLoad(luastate,exp);
	ret = lua_pcall(luastate,0,0,0);
	return (ret == LUA_OK);	
}
int luaxDict(char **val, const char *exp, const char *arg)
{
	int ret = 0;
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	luaxLoad(luastate,exp);
	lua_pushstring(luastate,arg);
	ret = lua_pcall(luastate,1,1,0);
	asprintf(val,"%s",lua_tostring(luastate,-1));
	lua_pop(luastate,1);
	return (ret == LUA_OK);
}
int luaxPerm(int *val, const char *exp, int arg)
{
	int ret = 0;
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	luaxLoad(luastate,exp);
	lua_pushinteger(luastate,arg);
	ret = lua_pcall(luastate,1,1,0);
	*val = lua_tonumber(luastate,-1);
	lua_pop(luastate,1);
	return (ret == LUA_OK);
}
int luaxClosure(lua_State *L)
{
	struct Prototype fnc = {0};
	fnc.ft = lua_tointeger(L, lua_upvalueindex(1));
	fnc.vp = lua_touserdata(L, lua_upvalueindex(2));
	switch (fnc.ft) {
		case (Lftype): {
			int val = 0;
			int ret = 0;
			int len = lua_type(L,3)==LUA_TNONE?0:lua_tointeger(L,3);
			ret = fnc.lf(&val,lua_tostring(L,1),lua_tostring(L,2),&len);
			lua_pushinteger(L,val);
			lua_pushinteger(L,len);
			lua_pushinteger(L,ret);
			return 3;}
		case (Fftype): {
			lua_pushinteger(L,fnc.ff(lua_tostring(L,1)));
			return 1;}
		case (Gftype): {
			lua_pushinteger(L,fnc.gf(lua_tostring(L,1),lua_tostring(L,2)));
			return 1;}
// typedef int (*oftype)(void *arg);
// typedef void (*nftype)(void **use, const char *str);
// typedef void (*mftype)(void **run, void *use);
		default: return 0;}
}
void luaxFunc(const char *str, struct Prototype fnc)
{
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	lua_pushinteger(luastate, fnc.ft);
	lua_pushlightuserdata(luastate, fnc.vp);
	lua_pushcclosure(luastate, luaxClosure, 2);
	lua_setglobal(luastate, str);
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
void nestFree()
{
	for (int i = 0; i < dim; i++) if (fiber[i].str) free(fiber[i].str);
	for (int i = 0; i < dim; i++) if (fiber[i].exp) free(fiber[i].exp);
	if (dim) free(fiber); fiber = 0; dim = 0;
	lua_settop(luastate,0);
}
void nestInit(int siz)
{
	if (dim) nestFree();
	for (int i = 0; i < lsiz; i++) if (rslt[i]) {free(rslt[i]); rslt[i] = 0;}
	for (int i = 0; i < lsiz; i++) if (line[i]) {free(line[i]); line[i] = 0;}
	if (siz) {rslt = realloc(rslt,siz*sizeof(char *)); line = realloc(line,siz*sizeof(char *));}
	else {free(rslt); rslt = 0; free(line); line = 0;}
	for (int i = lsiz; i < siz; i++) {rslt[i] = 0; line[i] = 0;}
	lsiz = siz;
}
void nestElem(int i, const char *str)
{
	if (i < 0 || i >= lsiz) ERROR(exitErr,0);
	line[i] = realloc(line[i],strlen(str)+1);
	strcpy(line[i],str);
}
void nestScan()
{
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	if (dim) nestFree();
	for (int i = 0; i < lsiz; i++) {
	for (const char *str = line[i]; str && strstr(str,"%(") && *(str = strstr(str,"%(")) && *(str += 2); dim++) {
	for (int nst = 1; nst; nst += nestSkip(&str)) {}}}
	fiber = realloc(fiber,dim*sizeof(struct Fiber)); dim = 0;
	for (int i = 0; i < lsiz; i++) {
	for (const char *str = line[i]; str && strstr(str,"%(") && *(str = strstr(str,"%(")) && *(str += 2); dim++) {
		int pos, len;
		fiber[dim].pos = pos = str-line[i];
		for (int nst = 1; nst; nst += nestSkip(&str)) {}
		len = str-line[i]-pos-1;
		fiber[dim].exp = malloc(len+1);
		strncpy(fiber[dim].exp,line[i]+pos,len);
		fiber[dim].exp[len] = 0;
		fiber[dim].str = 0;
		fiber[dim].lua = 0;
		fiber[dim].top = 0;
		fiber[dim].idx = i;}}
}
int nestEval(int i)
{
	int ret = 0;
	int num = 0;
	int len = 0;
	if (i < 0 || i >= dim) ERROR(exitErr,0);
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	if (!fiber[i].lua) {fiber[i].lua = lua_newthread(luastate); fiber[i].top = lua_gettop(luastate); luaxLoad(fiber[i].lua,fiber[i].exp);}
	ret = lua_resume(fiber[i].lua,0,0,&num);
	if (ret != LUA_OK && ret != LUA_YIELD) {
		printf("lua %s\n",lua_tostring(fiber[i].lua,-1));
		ERROR(exitErr,0);}
	for (int j = 0; j < num; j++) if (!lua_tostring(fiber[i].lua,j-num)) {
		printf("lua returned null: %s\n",fiber[i].exp);
		ERROR(exitErr,0);}
	for (int j = 0; j < num; j++) len += strlen(lua_tostring(fiber[i].lua,j-num));
	fiber[i].str = realloc(fiber[i].str,len+1); fiber[i].str[0] = 0;
	for (int j = 0; j < num; j++) strcat(fiber[i].str,lua_tostring(fiber[i].lua,j-num));
	lua_pop(fiber[i].lua,num);
	if (ret == LUA_OK) {fiber[i].lua = 0; lua_remove(luastate,fiber[i].top); for (int j = 0; j < dim; j++) {if (fiber[j].top > fiber[i].top) fiber[j].top--;} fiber[i].top = 0;}
	return (ret == LUA_YIELD);
}
int nestPass()
{
	int retval = 0;
	for (int i = 0; i < dim; i++) retval |= nestEval(i);
	return retval;
}
const char *nestRepl(int i)
{
	int length = 0;
	int pos = 0;
	if (i < 0 || i >= lsiz) ERROR(exitErr,0);
	length = strlen(line[i]);
	for (int j = 0; j < dim; j++) if (fiber[j].idx == i) {
	length -= strlen(fiber[j].exp)+3; length += strlen(fiber[j].str);}
	rslt[i] = realloc(rslt[i],length+1); length = 0;
	for (int j = 0; j < dim; j++) if (fiber[j].idx == i) {
		int len = fiber[j].pos-pos-2;
		strncpy(rslt[i]+length,line[i]+pos,len); length += len; pos += len+strlen(fiber[j].exp)+3;
		strcpy(rslt[i]+length,fiber[j].str); length += strlen(fiber[j].str);}
	strcpy(rslt[i]+length,line[i]+pos);
	// if (strcmp(line[i],rslt[i]) != 0) printf("nestRepl %s -> %s\n",line[i],rslt[i]);
	return rslt[i];
}

int luaxSideLua(lua_State *L)
{
	luaxSide(lua_tostring(L,1));
	return 0;
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
	nestScan();
	return 0;
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

int luaopen_luax(lua_State *L)
{
	lua_pushcfunction(L, luaxSideLua);
	lua_setglobal(L, "luaxSide");
	lua_pushcfunction(L, nestInitLua);
	lua_setglobal(L, "nestInit");
	lua_pushcfunction(L, nestElemLua);
	lua_setglobal(L, "nestElem");
	lua_pushcfunction(L, nestScanLua);
	lua_setglobal(L, "nestScan");
	lua_pushcfunction(L, nestPassLua);
	lua_setglobal(L, "nestPass");
	lua_pushcfunction(L, nestReplLua);
	lua_setglobal(L, "nestRepl");
	return 0;
}
