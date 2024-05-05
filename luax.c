#include "luax.h"
#include "face.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <string.h>
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
{ // pushes chunk on the stack
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
void wrapCallback(const struct Close *arg);
int luaxUnwrap(lua_State *L)
{
	const struct Close *arg = lua_touserdata(L, lua_upvalueindex(1));
	for (int i = 0; i < arg->n; i++) switch (arg->a[i].t) {
		case (Itype): arg->a[i].i = lua_tointeger(L,i+1); break;
		case (Jtype): arg->a[i].j = lua_tointeger(L,i+1); break;
		case (Ktype): arg->a[i].k = lua_tointeger(L,i+1); break;
		case (Mtype): arg->a[i].m = lua_tonumber(L,i+1); break;
		case (Ntype): arg->a[i].n = lua_tonumber(L,i+1); break;
		case (Utype): arg->a[i].u = lua_tostring(L,i+1); break;
		default: ERROR();}
	wrapCallback(arg);
	lua_pop(L,arg->n);
	int vld = (arg->i < 0 || arg->i >= arg->m || arg->b[arg->i].i == 0 ? 1 : 0);
	if (!vld) lua_pushnil(L);
	for (int i = !vld; i < arg->m; i++) switch (arg->b[i].t) {
		case (Itype): lua_pushinteger(L,arg->b[i].i); break;
		case (Jtype): lua_pushinteger(L,arg->b[i].j); break;
		case (Ktype): lua_pushinteger(L,arg->b[i].k); break;
		case (Mtype): lua_pushnumber(L,arg->b[i].m); break;
		case (Ntype): lua_pushnumber(L,arg->b[i].n); break;
		case (Utype): lua_pushstring(L,arg->b[i].u); break;
		case (Vtype): lua_pushstring(L,arg->b[i].v); break;
		default: ERROR();}
	return arg->m;
}
void luaxWrap(lua_State *L, const char *str, const struct Close *arg)
{
	lua_pushlightuserdata(L, (void*)arg);
	lua_pushcclosure(L, luaxUnwrap, 1);
	lua_setglobal(L, str);
}
lua_State *luaxInit()
{
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	return luastate;
}
int luaxSide(const char *exp)
{ // evaluates expression without arguments
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	if (luaxLoad(luastate,exp) != 0) return -2;
	if (lua_pcall(luastate,0,0,0) != LUA_OK) {luaxErr(); return -1;}
	return 0;
}
int nestSkip(const char **str)
{
	char *bas = strchr(*str,'(');
	char *lim = strchr(*str,')');
	if (!lim) ERROR();
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
{ // number strings in the repeatable chunk, zero to deallocate
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	if (dim) nestFree();
	for (int i = 0; i < lsiz; i++) if (rslt[i]) {free(rslt[i]); rslt[i] = 0;}
	for (int i = 0; i < lsiz; i++) if (line[i]) {free(line[i]); line[i] = 0;}
	if (siz) {rslt = realloc(rslt,siz*sizeof(char *)); temp = realloc(temp,siz*sizeof(char *)); line = realloc(line,siz*sizeof(char *));}
	else {free(rslt); rslt = 0; free(line); line = 0;}
	for (int i = lsiz; i < siz; i++) {rslt[i] = 0; line[i] = 0;}
	lsiz = siz;
}
void nestElem(int i, const char *str)
{ // set given string in chunk
	if (i < 0 || i >= lsiz) ERROR();
	line[i] = realloc(line[i],strlen(str)+1);
	strcpy(line[i],str);
}
void nestScan()
{ // get dim %() expressions from the lsiz strings
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
{ // evaluate given expression, returning if it yielded
	int ret = 0;
	int num = 0;
	int len = 0;
	if (i < 0 || i >= dim) ERROR();
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	if (!fiber[i].lua) {fiber[i].lua = lua_newthread(luastate); fiber[i].top = lua_gettop(luastate); if (luaxLoad(fiber[i].lua,fiber[i].exp) != 0) return 0;}
	ret = lua_resume(fiber[i].lua,0,0,&num);
	if (ret != LUA_OK && ret != LUA_YIELD) {
		printf("lua %s\n",lua_tostring(fiber[i].lua,-1));
		ERROR();}
	for (int j = 0; j < num; j++) if (!lua_tostring(fiber[i].lua,j-num)) {
		printf("lua returned null: %s\n",fiber[i].exp);
		ERROR();}
	for (int j = 0; j < num; j++) len += strlen(lua_tostring(fiber[i].lua,j-num));
	fiber[i].str = realloc(fiber[i].str,len+1); fiber[i].str[0] = 0;
	for (int j = 0; j < num; j++) strcat(fiber[i].str,lua_tostring(fiber[i].lua,j-num));
	lua_pop(fiber[i].lua,num);
	if (ret == LUA_OK) {fiber[i].lua = 0; lua_remove(luastate,fiber[i].top); for (int j = 0; j < dim; j++) {if (fiber[j].top > fiber[i].top) fiber[j].top--;} fiber[i].top = 0;}
	return (ret == LUA_YIELD);
}
int nestPass()
{ // evaluate all expressions in chunk, returining if chunk should be repeated because any of its expressions yielded
	int retval = 0;
	for (int i = 0; i < dim; i++) retval |= nestEval(i);
	return retval;
}
const char *nestRepl(int i)
{ // get given string with expressions replaced
	int length = 0;
	int pos = 0;
	if (i < 0 || i >= lsiz) ERROR();
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
	return rslt[i];
}
void wrapFace(lua_State *L);
void wrapLuax(lua_State *L);
int luaopen_luax(lua_State *L)
{
	wrapFace(L);
	wrapLuax(L);
	return 0;
}

