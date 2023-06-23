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
int luaxClose(const struct Closure *fnc)
{ // evaluates the chunk on the stack
	const char *ptr = 0;
	size_t len = 0;
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	for (int i = 0; i < fnc->na; i++) {
		struct Parameter *arg = fnc->aa+i;
		switch (arg->at) {
		case (Iatype): lua_pushinteger(luastate,arg->ia); break;
		case (Satype): lua_pushstring(luastate,arg->sa); break;
		case (Latype): lua_pushlstring(luastate,arg->sa,arg->la); break;
		case (Patype): lua_pushlightuserdata(luastate,arg->pa); break;
		default: ERROR();}}
	if (lua_pcall(luastate,fnc->na,fnc->nb,0) != LUA_OK) {luaxErr(); return -1;}
	for (int i = 0; i < fnc->nb; i++) {
		struct Parameter *arg = fnc->ab+i;
		switch (arg->at) {
		case (Iatype): protoMakeIf(arg,lua_tonumber(luastate,i+1)); break;
		case (Satype): ptr = lua_tostring(luastate,i+1); protoMakeSf(arg,ptr); break;
		case (Latype): ptr = lua_tolstring(luastate,i+1,&len); protoMakeLf(arg,ptr,len); break;
		case (Patype): protoMakePf(arg,lua_touserdata(luastate,i+1)); break;
		default: ERROR();}}
	lua_pop(luastate,fnc->nb);
	return 0;	
}
int luaxSide(const char *exp)
{ // evaluates expression without arguments
	return luaxExpr(exp,protoClose(0,0));
}
int luaxExpr(const char *exp, const struct Closure *fnc)
{ // evaluates expression with arguments
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	if (luaxLoad(luastate,exp) != 0) return -2;
	return luaxClose(fnc);	
}
int luaxCall(const char *str, const struct Closure *fnc)
{ // calls function with arguments
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	lua_getglobal(luastate,str);
	return luaxClose(fnc);
}
int luaxClosure(lua_State *L)
{ // calls function given by c closure
	struct Function fnc = {0}; int len = 0; int ret = 0;
	long long val = 0; double num = 0.0; char *str = 0; void *ptr = 0;
	int32_t v32 = 0; int vnt = 0; float old = 0.0; char chr = 0;
	fnc.ft = lua_tointeger(L, lua_upvalueindex(1));
	fnc.vp = lua_touserdata(L, lua_upvalueindex(2));
	switch (fnc.ft) {
		// typedef void (*cftype)(int idx); // thread callback
		case (Cftype): fnc.cf(lua_tointeger(L,1)); break;
		// typedef void (*cgtype)(int idx0, int idx1);
		case (Cgtype): fnc.cg(lua_tointeger(L,1),lua_tointeger(L,2)); break;
		// typedef void (*chtype)();
		case (Chtype): fnc.ch(); break;
		// typedef void (*dftype)(void *dat);
		case (Dftype): fnc.df(lua_touserdata(L,1)); break;
		// typedef void (*dgtype)(void *dat, int mem, int sub);
		case (Dgtype): fnc.dg(lua_touserdata(L,1),lua_tointeger(L,2)); break;
		// typedef void (*dhtype)(void **dat, int mem, int sub);
		case (Dhtype): fnc.dh(&ptr,lua_tointeger(L,1)); lua_pushlightuserdata(L,ptr); break;
		// typedef void (*eftype)(const char *str, int num, int idx); // error throws
		case (Eftype): fnc.ef(lua_tostring(L,1),lua_tointeger(L,2),lua_tointeger(L,3)); break;
		// typedef void (*egtype)(int idx, const char *str); // add string
		case (Egtype): fnc.eg(lua_tointeger(L,1),lua_tostring(L,2)); break;
		// typedef void (*ehtype)(const char *str); // lua wrapper
		case (Ehtype): fnc.eh(lua_tostring(L,1)); break;
		// typedef void (*hftype)(const char *val); // haskell wrapper
		// typedef void (*hgtype)(int val); // haskell wrapper
		// typedef void (*hhtype)(double val); // haskell wrapper
		// typedef void (*hitype)(long long val); // haskell wrapper
		// typedef void (*hjtype)(float val); // haskell wrapper
		// typedef void (*hktype)(int len, const char *val); // haskell wrapper
		// typedef void (*hltype)(int32_t val); // haskell wrapper
		// typedef void (*hmtype)(char val); // haskell wrapper
		// typedef int (*pftype)(int fildes, void *buf, int nbyte); // stream to punt to
		// typedef int (*qftype)(int fildes, const void *buf, int nbyte); // stream to punt to
		// typedef int (*fftype)(const char *str);
		case (Fftype): lua_pushinteger(L,fnc.ff(lua_tostring(L,1))); ret = 1; break;
		// typedef int (*fgtype)(const char *str, int len, int idx);
		case (Fgtype): lua_pushinteger(L,fnc.fg(lua_tostring(L,1),lua_tointeger(L,2),lua_tointeger(L,3))); ret = 1; break;
		// typedef int (*fhtype)(int idx, const char *str);
		case (Fhtype): lua_pushinteger(L,fnc.fh(lua_tointeger(L,1),lua_tostring(L,2))); ret = 1; break;
		// typedef int (*gftype)(const char *one, const char *oth);
		case (Gftype): lua_pushinteger(L,fnc.gf(lua_tostring(L,1),lua_tostring(L,2))); ret = 1; break;
		// typedef int (*ggtype)(int rfd, int wfd);
		case (Ggtype): lua_pushinteger(L,fnc.gg(lua_tointeger(L,1),lua_tointeger(L,2))); ret = 1; break;
		// typedef int (*oftype)(void *arg);
		case (Oftype): lua_pushinteger(L,fnc.of(lua_touserdata(L,1))); ret = 1; break;

		// typedef int (*tftype)(double dly, int msk);
		case (Tftype): lua_pushinteger(L,fnc.tf(lua_tonumber(L,1),lua_tointeger(L,2))); ret = 1; break;
		// typedef void (*tgtype)(long long arg, int idx);
		case (Tgtype): fnc.tg(lua_tointeger(L,1),lua_tointeger(L,2)); break;
		// typedef int (*thtype)(long long loc, long long siz, int idx);
		case (Thtype): lua_pushinteger(L,fnc.th(lua_tointeger(L,1),lua_tointeger(L,2),lua_tointeger(L,3))); ret = 1; break;
		// typedef long long (*titype)(int idx);
		case (Titype): lua_pushinteger(L,fnc.ti(lua_tointeger(L,1))); ret = 1; break;
		// typedef void (*tjtype)(long long loc, long long siz, int idx);
		case (Tjtype): fnc.tj(lua_tointeger(L,1),lua_tointeger(L,2),lua_tointeger(L,3)); break;
		// typedef void (*tktype)(double sec);
		case (Tktype): fnc.tk(lua_tonumber(L,1)); break;
		// typedef int (*tltype)(int arg);
		case (Tltype): lua_pushinteger(L,fnc.tl(lua_tointeger(L,1))); ret = 1; break;
		// typedef int (*tmtype)();
		case (Tmtype): lua_pushinteger(L,fnc.tm()); ret = 1; break;

		// typedef void (*sftype)(char **str, int idx);
		case (Sftype): fnc.sf(&str,lua_tointeger(L,1)); lua_pushstring(L,str); ret = 1; break;
		// typedef void (*sgtype)(char **str, long long loc, int idx);
		case (Sgtype): fnc.sg(&str,lua_tointeger(L,1),lua_tointeger(L,2)); lua_pushstring(L,str); ret = 1; break;
		// typedef void (*shtype)(void **dat, int idx);
		case (Shtype): fnc.sh(&ptr,lua_tointeger(L,1)); lua_pushlightuserdata(L,ptr); ret = 1; break;
		// typedef char (*sitype)(int idx);
		case (Sitype): lua_pushinteger(L,fnc.si(lua_tointeger(L,1))); ret = 1; break;
		// typedef int (*sjtype)(int idx);
		case (Sjtype): lua_pushinteger(L,fnc.sj(lua_tointeger(L,1))); ret = 1; break;
		// typedef int32_t (*sktype)(int idx);
		case (Sktype): lua_pushinteger(L,fnc.sk(lua_tointeger(L,1))); ret = 1; break;
		// typedef double (*sltype)(int idx);
		case (Sltype): lua_pushnumber(L,fnc.sl(lua_tointeger(L,1))); ret = 1; break;
		// typedef long long (*smtype)(int idx);
		case (Smtype): lua_pushinteger(L,fnc.sm(lua_tointeger(L,1))); ret = 1; break;
		// typedef float (*sntype)(int idx);
		case (Sntype): lua_pushnumber(L,fnc.sn(lua_tointeger(L,1))); ret = 1; break;

		// typedef void (*lftype)(const char *arg, int idx);
		case (Lftype): fnc.lf(lua_tostring(L,1),lua_tointeger(L,2)); break;
		// typedef void (*lgtype)(const char *arg, long long loc, int idx);
		case (Lgtype): fnc.lg(lua_tostring(L,1),lua_tointeger(L,2),lua_tointeger(L,3)); break;
		// typedef void (*lhtype)(const void *arg, int idx);
		case (Lhtype): fnc.lh(lua_touserdata(L,1),lua_tointeger(L,2)); break;
		// typedef void (*litype)(char arg, int idx);
		case (Litype): fnc.li(lua_tointeger(L,1),lua_tointeger(L,2)); break;
		// typedef void (*ljtype)(int arg, int idx);
		case (Ljtype): fnc.lj(lua_tointeger(L,1),lua_tointeger(L,2)); break;
		// typedef void (*lktype)(int32_t arg, int idx);
		case (Lktype): fnc.lk(lua_tointeger(L,1),lua_tointeger(L,2)); break;
		// typedef void (*lltype)(double arg, int idx);
		case (Lltype): fnc.ll(lua_tonumber(L,1),lua_tointeger(L,2)); break;
		// typedef void (*lmtype)(long long arg, int idx);
		case (Lmtype): fnc.lm(lua_tointeger(L,1),lua_tointeger(L,2)); break;
		// typedef void (*lntype)(float arg, int idx);
		case (Lntype): fnc.ln(lua_tonumber(L,1),lua_tointeger(L,2)); break;

		// typedef void (*mftype)(const char* val, char **str);
		case (Mftype): str = strdup(lua_tostring(L,2)); fnc.mf(lua_tostring(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mhtype)(const void* val, char **str);
		case (Mhtype): str = strdup(lua_tostring(L,2)); fnc.mh(lua_touserdata(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mitype)(char val, char **str);
		case (Mitype): str = strdup(lua_tostring(L,2)); fnc.mi(lua_tointeger(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mjtype)(int val, char **str);
		case (Mjtype): str = strdup(lua_tostring(L,2)); fnc.mj(lua_tointeger(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mktype)(int32_t val, char **str);
		case (Mktype): str = strdup(lua_tostring(L,2)); fnc.mk(lua_tointeger(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mltype)(double val, char **str);
		case (Mltype): str = strdup(lua_tostring(L,2)); fnc.ml(lua_tonumber(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mmtype)(long long val, char **str);
		case (Mmtype): str = strdup(lua_tostring(L,2)); fnc.mm(lua_tointeger(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mntype)(float val, char **str);
		case (Mntype): str = strdup(lua_tostring(L,2)); fnc.mn(lua_tonumber(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*motype)(const char *typ, const char* val, char **str);
		case (Motype): str = strdup(lua_tostring(L,3)); fnc.mo(lua_tostring(L,1),lua_tostring(L,2),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mptype)(const char* val, char **str);
		case (Mptype): str = strdup(lua_tostring(L,2)); fnc.mp(lua_tostring(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*mqtype)(char **str);
		case (Mqtype): str = strdup(lua_tostring(L,1)); fnc.mq(&str); lua_pushstring(L,str); free(str); ret = 1; break;

		// typedef int (*nftype)(char **val, const char *str, int *siz);
		case (Nftype): len = lua_tointeger(L,3); if (fnc.nf(&str,lua_tostring(L,1),&len)) lua_pushstring(L,str); else lua_pushnil(L); lua_pushnumber(L,len); free(str); ret = 2; break;
		// typedef int (*nhtype)(void **val, const char *str, int *siz);
		case (Nhtype): len = lua_tointeger(L,2); if (fnc.nh(&ptr,lua_tostring(L,1),&len)) lua_pushlightuserdata(L,ptr); else lua_pushnil(L); lua_pushnumber(L,len); free(ptr); ret = 2; break;
		// typedef int (*nitype)(char *val, const char *str, int *siz);
		case (Nitype): len = lua_tointeger(L,2); if (fnc.ni(&chr,lua_tostring(L,1),&len)) lua_pushinteger(L,chr); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;
		// typedef int (*njtype)(int *val, const char *str, int *siz);
		case (Njtype): len = lua_tointeger(L,2); if (fnc.nj(&vnt,lua_tostring(L,1),&len)) lua_pushinteger(L,vnt); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;
		// typedef int (*nktype)(int32_t *val, const char *str, int *siz);
		case (Nktype): len = lua_tointeger(L,2); if (fnc.nk(&v32,lua_tostring(L,1),&len)) lua_pushinteger(L,v32); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;
		// typedef int (*nltype)(double *val, const char *str, int *siz);
		case (Nltype): len = lua_tointeger(L,2); if (fnc.nl(&num,lua_tostring(L,1),&len)) lua_pushnumber(L,num); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;
		// typedef int (*nmtype)(long long *val, const char *str, int *siz);
		case (Nmtype): len = lua_tointeger(L,2); if (fnc.nm(&val,lua_tostring(L,1),&len)) lua_pushinteger(L,val); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;
		// typedef int (*nntype)(float *val, const char *str, int *siz);
		case (Nntype): len = lua_tointeger(L,2); if (fnc.nn(&old,lua_tostring(L,1),&len)) lua_pushnumber(L,old); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;
		// typedef int (*notype)(const char* typ, const char *val, const char *str, int *siz);
		case (Notype): len = lua_tointeger(L,4); if (fnc.no(lua_tostring(L,1),lua_tostring(L,2),lua_tostring(L,3),&len)) lua_pushnumber(L,1); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;
		// typedef int (*nptype)(const char *val, const char *str, int *siz);
		case (Nptype): len = lua_tointeger(L,3); if (fnc.np(lua_tostring(L,1),lua_tostring(L,2),&len)) lua_pushnumber(L,1); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;
		// typedef int (*nqtype)(const char *str, int *siz);
		case (Nqtype): len = lua_tointeger(L,2); if (fnc.nq(lua_tostring(L,1),&len)) lua_pushnumber(L,1); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2; break;

		// typedef void (*rftype)(int siz);
		case (Rftype): fnc.rf(lua_tointeger(L,1)); break;
		// typedef void (*rgtype)(int i, const char *str);
		case (Rgtype): fnc.rg(lua_tointeger(L,1),lua_tostring(L,2)); break;
		// typedef void (*rhtype)();
		case (Rhtype): fnc.rh(); break;
		// typedef int (*ritype)();
		case (Ritype): lua_pushinteger(L,fnc.ri()); ret = 1; break;
		// typedef const char *(*rjtype)(int i);
		case (Rjtype): lua_pushstring(L,fnc.rj(lua_tointeger(L,1))); ret = 1; break;
		// typedef int (*rktype)(char **val, const char *key);
		case (Rktype): lua_pushinteger(L,fnc.rk(&str,lua_tostring(L,1))); lua_pushstring(L,str); free(str); ret = 2; break;
		// typedef void (*rltype)(const char *key, const char *val, int typ);
		case (Rltype): fnc.rl(lua_tostring(L,1),lua_tostring(L,2),lua_tointeger(L,3)); break;
		// typedef void (*rktype)(char **val, const char *key);
		case (Rmtype): fnc.rm(&str,lua_tostring(L,1)); lua_pushstring(L,str); free(str); ret = 1; break;
		// typedef void (*rltype)(const char *key, const char *val);
		case (Rntype): fnc.rn(lua_tostring(L,1),lua_tostring(L,2)); break;

		default: ERROR();}
	return ret;
}
void luaxExtend(lua_State *L, const char *str, struct Function fnc)
{ // extends interpreter with given function
	lua_pushinteger(L, fnc.ft);
	lua_pushlightuserdata(L, fnc.vp);
	lua_pushcclosure(L, luaxClosure, 2);
	lua_setglobal(L, str);
}
void luaxAdd(const char *str, struct Function fnc)
{
	if (!luastate) {luastate = lua_newstate(luaxLua,0); luaL_openlibs(luastate);}
	luaxExtend(luastate,str,fnc);
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

int luaopen_luax(lua_State *L)
{
	luastate = L;
	luaxExtend(L,"luaxSide",protoTypeFf(luaxSide));
	luaxExtend(L,"nestInit",protoTypeRf(nestInit));
	luaxExtend(L,"nestElem",protoTypeRg(nestElem));
	luaxExtend(L,"nestScan",protoTypeRh(nestScan));
	luaxExtend(L,"nestPass",protoTypeRi(nestPass));
	luaxExtend(L,"nestRepl",protoTypeRj(nestRepl));
	return 0;
}
