#include "luax.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <string.h>
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
char **temp = 0; // to exp or str
int lsiz = 0; // number of strings in line
int irslt = 0;
void *prslt = 0;
char *srslt = 0;
lua_State *luastate = 0;

void luaxErr()
{
	protoErr("%s\n",lua_tostring(luastate,-1));
	lua_pop(luastate,1);
}
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
int luaxLoad(lua_State *luastate, const char *exp)
{
	int retval = 0;
	struct Reader reader = {0};
	asprintf(&reader.str,"%s",exp);
	reader.vld = 1;
	retval = lua_load(luastate,luaxReader,(void*)&reader,"","t");
	free(reader.str);
	if (retval != LUA_OK) {luaxErr(); return -1;}
	return 0;
}
int luaxPath(const char *exp, const char *fnc)
{
	int i = protoPath(exp);
	char *temp = 0;
	int val = 0;
	if (i < 0) return -1;
	asprintf(&temp,"%s(\"%s%s\")",fnc,protoGet(i),exp);
	val = luaxSide(temp);
	free(temp);
	return val;
}
int luaxFile(const char *exp)
{
	return luaxPath(exp,"dofile");
}
int luaxLib(const char *exp)
{
	return luaxPath(exp,"require");
}
int luaxSide(const char *exp)
{
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	if (luaxLoad(luastate,exp) != 0) return -2;
	if (lua_pcall(luastate,0,0,0) != LUA_OK) {luaxErr(); return -1;}
	return 0;	
}
int luaxCall(const char *str, const struct Closure *fnc)
{
	int val = 0;
	const char *ptr = 0;
	size_t len = 0;
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	for (int i = 0; i < fnc->na; i++) {
		struct Argument *arg = fnc->aa+i;
		switch (arg->at) {
		case (Iatype): lua_pushinteger(luastate,arg->ia); break;
		case (Satype): lua_pushstring(luastate,arg->sa); break;
		case (Latype): lua_pushlstring(luastate,arg->sa,arg->la); break;
		case (Patype): lua_pushlightuserdata(luastate,arg->pa); break;
		default: break;}}
	val = luaxSide(str);
	if (val < 0) return -1;
	for (int i = 0; i < fnc->nb; i++) {
		struct Argument *arg = fnc->ab+i;
		switch (arg->at) {
		case (Iatype): protoMakeIf(arg,lua_tonumber(luastate,i+1)); break;
		case (Satype): ptr = lua_tostring(luastate,i+1); protoMakeSf(arg,ptr); break;
		case (Latype): ptr = lua_tolstring(luastate,i+1,&len); protoMakeLf(arg,ptr,len); break;
		case (Patype): protoMakePf(arg,lua_touserdata(luastate,i+1)); break;
		default: break;}}
	lua_pop(luastate,fnc->nb);
	return val;
}
int luaxClosure(lua_State *L)
{
	struct Function fnc = {0};
	void *mem = 0;
	fnc.ft = lua_tointeger(L, lua_upvalueindex(1));
	fnc.vp = lua_touserdata(L, lua_upvalueindex(2));
	switch (fnc.ft) {
		// typedef int (*fftype)(const char *str);
		case (Fftype): lua_pushinteger(L,fnc.ff(lua_tostring(L,1))); return 1;
		// typedef int (*gftype)(const char *one, const char *oth);
		case (Gftype): lua_pushinteger(L,fnc.gf(lua_tostring(L,1),lua_tostring(L,2))); return 1;
		// typedef int (*oftype)(void *arg);
		case (Oftype): lua_pushinteger(L,fnc.of(lua_touserdata(L,1))); return 1;
		// typedef const char *(*aftype)(void *mem);
		case (Aftype): lua_pushstring(L,fnc.af(lua_touserdata(L,1))); return 1;
		// typedef void (*nftype)(void **use, const char *str);
		case (Nftype): mem = lua_touserdata(L,1); fnc.nf(&mem,lua_tostring(L,2)); lua_pushlightuserdata(L,mem); return 1;
		// typedef void (*mftype)(void **run, void *use);
		case (Mftype): mem = lua_touserdata(L,1); fnc.mf(&mem,lua_touserdata(L,2)); lua_pushlightuserdata(L,mem); return 1;
		// typedef void (*dftype)(void **mem);
		case (Dftype): mem = lua_touserdata(L,1); fnc.df(&mem); lua_pushlightuserdata(L,mem); return 1;
		// typedef void (*iftype)(void **mem, int key);
		case (Iftype): mem = lua_touserdata(L,1); fnc.it(&mem,lua_tointeger(L,2)); lua_pushlightuserdata(L,mem); return 1;
		// typedef void *(*jftype)(void *giv, void *key);
		case (Jftype): lua_pushlightuserdata(L,fnc.jf(lua_touserdata(L,1),lua_touserdata(L,2))); return 1;
		// typedef void *(*kftype)(void *giv, int key);
		case (Kftype): lua_pushlightuserdata(L,fnc.kf(lua_touserdata(L,1),lua_tointeger(L,2))); return 1;
		// typedef void *(*tftype)(int idx);
		case (Tftype): lua_pushlightuserdata(L,fnc.tf(lua_tointeger(L,1))); return 1;
		// typedef int (*lftype)(void **mem);
		case (Lftype): mem = lua_touserdata(L,1); lua_pushinteger(L,fnc.lf(&mem)); lua_pushlightuserdata(L,mem); return 2;
		default: break;}
	return 0;
}
void luaxAdd(const char *str, struct Function fnc)
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
	if (siz) {rslt = realloc(rslt,siz*sizeof(char *)); temp = realloc(temp,siz*sizeof(char *)); line = realloc(line,siz*sizeof(char *));}
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
	if (!fiber[i].lua) {fiber[i].lua = lua_newthread(luastate); fiber[i].top = lua_gettop(luastate); if (luaxLoad(fiber[i].lua,fiber[i].exp) != 0) return 0;}
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
	for (int j = 0; j < dim; j++) if (fiber[j].idx == i) if (fiber[j].str) {
		length -= strlen(fiber[j].exp)+3; length += strlen(fiber[j].str);}
	rslt[i] = realloc(rslt[i],length+1); length = 0;
	for (int j = 0; j < dim; j++) if (fiber[j].idx == i) {
		int len = fiber[j].pos-pos-2;
		int exp = strlen(fiber[j].exp)+3;
		int siz = (fiber[j].str ? strlen(fiber[j].str) : 0);
		strncpy(rslt[i]+length,line[i]+pos,len); length += len; pos += len;
		if (fiber[j].str) {strcpy(rslt[i]+length,fiber[j].str); length += siz;}
		else {strncpy(rslt[i]+length,line[i]+pos,exp); length += exp;}
		pos += exp;}
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
