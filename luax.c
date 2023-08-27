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
#define LUAXARGS lua_tostring
#define LUAXARGI lua_tointeger
#define LUAXARGN lua_tonumber
#define LUAXARGU lua_touserdata
#define LUAXRETS lua_pushstring
#define LUAXRETI lua_pushinteger
#define LUAXRETN lua_pushnumber
#define LUAXRETU lua_pushlightuserdata
#define LUAXDUPS(U,V) U = strdup(V)
#define LUAXDUPI(U,V) U = V
#define LUAXDUPN(U,V) U = V
#define LUAXDUPU(U,V) ERROR()
#define LUAXTMPS(U) free(U)
#define LUAXTMPI(U)
#define LUAXTMPN(U)
#define LUAXTMPU(U)
#define LUAXCASE10(UC,LC,T0) case (UC##type): LUAXRET##T0(L,fnc.LC()); ret = 1; break;
#define LUAXCASE11(UC,LC,T0,T1) case (UC##type): LUAXRET##T0(L,fnc.LC(LUAXARG##T1(L,1))); ret = 1; break;
#define LUAXCASE12(UC,LC,T0,T1,T2) case (UC##type): LUAXRET##T0(L,fnc.LC(LUAXARG##T1(L,1),LUAXARG##T2(L,2))); ret = 1; break;
#define LUAXCASE13(UC,LC,T0,T1,T2,T3) case (UC##type): LUAXRET##T0(L,fnc.LC(LUAXARG##T1(L,1),LUAXARG##T2(L,2),LUAXARG##T3(L,3))); ret = 1; break;
#define LUAXCASE00(UC,LC) case (UC##type): fnc.LC(); break;
#define LUAXCASE01(UC,LC,T1) case (UC##type): fnc.LC(LUAXARG##T1(L,1)); break;
#define LUAXCASE02(UC,LC,T1,T2) case (UC##type): fnc.LC(LUAXARG##T1(L,1),LUAXARG##T2(L,2)); break;
#define LUAXCASE03(UC,LC,T1,T2,T3) case (UC##type): fnc.LC(LUAXARG##T1(L,1),LUAXARG##T2(L,2),LUAXARG##T3(L,3)); break;
#define LUAXCASEX0(UC,LC,X,TX) case (UC##type): fnc.LC(&X); LUAXRET##TX(L,X); LUAXTMP##TX(X); ret = 1; break;
#define LUAXCASEX1(UC,LC,X,TX,T1) case (UC##type): fnc.LC(&X,LUAXARG##T1(L,1)); LUAXRET##TX(L,X); LUAXTMP##TX(X); ret = 1; break;
#define LUAXCASEX2(UC,LC,X,TX,T1,T2) case (UC##type): fnc.LC(&X,LUAXARG##T1(L,1),LUAXARG##T2(L,2)); LUAXRET##TX(L,X); LUAXTMP##TX(X); ret = 1; break;
// case (Sftype): fnc.sf(&str,lua_tointeger(L,1),lua_tointeger(L,2),lua_tointeger(L,3)); lua_pushstring(L,str); ret = 1; break;
#define LUAXCASEX3(UC,LC,X,TX,T1,T2,T3) case (UC##type): fnc.LC(&X,LUAXARG##T1(L,1),LUAXARG##T2(L,2),LUAXARG##T3(L,3)); LUAXRET##TX(L,X); LUAXTMP##TX(X); ret = 1; break;
#define LUAXCASEY0(UC,LC,Y,TY) case (UC##type): LUAXDUP##TY(Y,LUAXARG##TY(L,1)); fnc.LC(&Y); LUAXRET##TY(L,Y); LUAXTMP##TY(Y); ret = 1; break;
#define LUAXCASEY1(UC,LC,Y,TY,T1) case (UC##type): LUAXDUP##TY(Y,LUAXARG##TY(L,2)); fnc.LC(LUAXARG##T1(L,1),&Y); LUAXRET##TY(L,Y); LUAXTMP##TY(Y); ret = 1; break;
// case (Mftype): str = strdup(lua_tostring(L,2)); fnc.mf(lua_tostring(L,1),&str); lua_pushstring(L,str); free(str); ret = 1; break;
#define LUAXCASEY2(UC,LC,Y,TY,T1,T2) case (UC##type): LUAXDUP##TY(Y,LUAXARG##TY(L,3)); fnc.LC(LUAXARG##T1(L,1),LUAXARG##T2(L,2),&Y); LUAXRET##TY(L,Y); LUAXTMP##TY(Y); ret = 1; break;
#define LUAXCASEZ0(UC,LC,Z,TZ,W,TW) case (UC##type): LUAXDUP##TW(W,LUAXARG##TW(L,1)); if (fnc.LC(&Z,&W)) LUAXRET##TZ(L,Z); else lua_pushnil(L); LUAXRET##TW(L,W); LUAXTMP##TZ(Z); LUAXTMP##TW(W); ret = 2; break;
#define LUAXCASEZ1(UC,LC,Z,TZ,W,TW,T1) case (UC##type): LUAXDUP##TW(W,LUAXARG##TW(L,2)); if (fnc.LC(&Z,LUAXARG##T1(L,1),&W)) LUAXRET##TZ(L,Z); else lua_pushnil(L); LUAXRET##TW(L,W); LUAXTMP##TZ(Z); LUAXTMP##TW(W); ret = 2; break;
// case (Nftype): len = lua_tointeger(L,2); if (fnc.nf(&str,lua_tostring(L,1),&len)) lua_pushstring(L,str); else lua_pushnil(L); lua_pushnumber(L,len); free(str); ret = 2; break;
#define LUAXCASEZ2(UC,LC,Z,TZ,W,TW,T1,T2) case (UC##type): LUAXDUP##TW(W,LUAXARG##TW(L,3)); if (fnc.LC(&Z,LUAXARG##T1(L,1),LUAXARG##T2(L,2),&W)) LUAXRET##TZ(L,Z); else lua_pushnil(L); LUAXRET##TW(L,W); LUAXTMP##TZ(Z); LUAXTMP##TW(W); ret = 2; break;
int luaxClosure(lua_State *L)
{ // calls function given by c closure
	struct Function fnc = {0}; int len = 0; int ret = 0;
	long long val = 0; double num = 0.0; char *str = 0; void *ptr = 0;
	int32_t v32 = 0; int vnt = 0; float old = 0.0; char chr = 0;
	fnc.ft = lua_tointeger(L, lua_upvalueindex(1));
	fnc.vp = lua_touserdata(L, lua_upvalueindex(2));
	switch (fnc.ft) {

		LUAXCASE13(Pf,pf,I,I,U,I) // typedef int (*pftype)(int fildes, void *buf, int nbyte);
		LUAXCASE13(Qf,qf,I,I,U,I) // typedef int (*qftype)(int fildes, const void *buf, int nbyte);
		LUAXCASE03(Ef,ef,S,I,I) // typedef void (*eftype)(const char *str, int num, int idx);
		LUAXCASE13(Fg,fg,I,S,I,I) // typedef int (*fgtype)(const char *str, int len, int idx);
		LUAXCASE02(Cg,cg,I,I) // typedef void (*cgtype)(int idx0, int idx1);
		LUAXCASE02(Hk,hk,I,S) // typedef void (*hktype)(int len, const char *val);
		LUAXCASE12(Gf,gf,I,S,S) // typedef int (*gftype)(const char *one, const char *oth);

		LUAXCASE00(Ch,ch) // typedef void (*chtype)();
		LUAXCASE01(Hm,hm,I) // typedef void (*hmtype)(char val);
		LUAXCASE01(Hg,hg,I) // typedef void (*hgtype)(int val);
		LUAXCASE01(Hl,hl,I) // typedef void (*hltype)(int32_t val);
		LUAXCASE01(Hi,hi,I) // typedef void (*hitype)(long long val);
		LUAXCASE01(Hj,hj,N) // typedef void (*hjtype)(float val);
		LUAXCASE01(Hh,hh,N) // typedef void (*hhtype)(double val);
		LUAXCASE01(Hf,hf,S) // typedef void (*hftype)(const char *val);

		LUAXCASE10(Tm,tm,I) // typedef int (*tmtype)();
		LUAXCASE11(Tl,tl,I,I) // typedef int (*tltype)(int arg);
		LUAXCASE11(Ff,ff,I,S) // typedef int (*fftype)(const char *str);

		LUAXCASE12(Gg,gg,I,I,I) // typedef int (*ggtype)(int rfd, int wfd);
		LUAXCASE12(Tf,tf,I,N,I) // typedef int (*tftype)(double dly, int msk);
		LUAXCASE13(Th,th,I,I,I,I) // typedef int (*thtype)(long long loc, long long siz, int idx);
		LUAXCASE03(Tj,tj,I,I,I) // typedef void (*tjtype)(long long loc, long long siz, int idx);

		LUAXCASEX1(Se,se,str,S,I) // typedef void (*sftype)(char **str, int idx);
		LUAXCASEX3(Sf,sf,str,S,I,I,I) // typedef void (*sftype)(char **str, int len, int idx, int loc);
		LUAXCASEX2(Sg,sg,str,S,I,I) // typedef void (*sgtype)(char **str, long long loc, int idx);
		LUAXCASEX1(Sh,sh,ptr,U,I) // typedef void (*shtype)(void **dat, int idx);

		LUAXCASE11(Si,si,I,I) // typedef char (*sitype)(int idx);
		LUAXCASE11(Sj,sj,I,I) // typedef int (*sjtype)(int idx);
		LUAXCASE11(Sk,sk,I,I) // typedef int32_t (*sktype)(int idx);
		LUAXCASE11(Sl,sl,N,I) // typedef double (*sltype)(int idx);
		LUAXCASE11(Sm,sm,I,I) // typedef long long (*smtype)(int idx);
		LUAXCASE11(Sn,sn,N,I) // typedef float (*sntype)(int idx);

		LUAXCASE02(Lf,lf,S,I) // typedef void (*lftype)(const char *arg, int idx);
		LUAXCASE03(Lg,lg,S,I,I) // typedef void (*lgtype)(const char *arg, long long loc, int idx);
		LUAXCASE02(Lh,lh,S,I) // typedef void (*lhtype)(const void *arg, int idx);
		LUAXCASE02(Li,li,I,I) // typedef void (*litype)(char arg, int idx);
		LUAXCASE02(Lj,lj,I,I) // typedef void (*ljtype)(int arg, int idx);
		LUAXCASE02(Lk,lk,I,I) // typedef void (*lktype)(int32_t arg, int idx);
		LUAXCASE02(Ll,ll,N,I) // typedef void (*lltype)(double arg, int idx);
		LUAXCASE02(Lm,lm,I,I) // typedef void (*lmtype)(long long arg, int idx);
		LUAXCASE02(Ln,ln,N,I) // typedef void (*lntype)(float arg, int idx);

		LUAXCASEY1(Mf,mf,str,S,S) // typedef void (*mftype)(const char* val, char **str);
		LUAXCASEY1(Mh,mh,str,S,U) // typedef void (*mhtype)(const void* val, char **str);
		LUAXCASEY1(Mi,mi,str,S,I) // typedef void (*mitype)(char val, char **str);
		LUAXCASEY1(Mj,mj,str,S,I) // typedef void (*mjtype)(int val, char **str);
		LUAXCASEY1(Mk,mk,str,S,I) // typedef void (*mktype)(int32_t val, char **str);
		LUAXCASEY1(Ml,ml,str,S,N) // typedef void (*mltype)(double val, char **str);
		LUAXCASEY1(Mm,mm,str,S,I) // typedef void (*mmtype)(long long val, char **str);
		LUAXCASEY1(Mn,mn,str,S,N) // typedef void (*mntype)(float val, char **str);
		LUAXCASEY2(Mo,mo,str,S,S,S) // typedef void (*motype)(const char *typ, const char* val, char **str);
		LUAXCASEY1(Mp,mp,str,S,S) // typedef void (*mptype)(const char* val, char **str);
		LUAXCASEY0(Mq,mq,str,S) // typedef void (*mqtype)(char **str);

		LUAXCASEZ1(Nf,nf,str,S,len,I,S) // typedef int (*nftype)(char **val, const char *str, int *len);
		LUAXCASEZ1(Nh,nh,ptr,U,len,I,S) // typedef int (*nhtype)(void **val, const char *str, int *siz);
		LUAXCASEZ1(Ni,ni,chr,I,len,I,S) // typedef int (*nitype)(char *val, const char *str, int *siz);
		LUAXCASEZ1(Nj,nj,vnt,I,len,I,S) // typedef int (*njtype)(int *val, const char *str, int *siz);
		LUAXCASEZ1(Nk,nk,v32,I,len,I,S) // typedef int (*nktype)(int32_t *val, const char *str, int *siz);
		LUAXCASEZ1(Nl,nl,num,N,len,I,S) // typedef int (*nltype)(double *val, const char *str, int *siz);
		LUAXCASEZ1(Nm,nm,val,I,len,I,S) // typedef int (*nmtype)(long long *val, const char *str, int *siz);
		LUAXCASEZ1(Nn,nn,old,N,len,I,S) // typedef int (*nntype)(float *val, const char *str, int *siz);
		case (Notype): len = lua_tointeger(L,4); if (fnc.no(lua_tostring(L,1),lua_tostring(L,2),lua_tostring(L,3),&len))
		lua_pushnumber(L,1); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2;
		break; // typedef int (*notype)(const char* typ, const char *val, const char *str, int *siz);
		case (Nptype): len = lua_tointeger(L,3); if (fnc.np(lua_tostring(L,1),lua_tostring(L,2),&len))
		lua_pushnumber(L,1); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2;
		break;// typedef int (*nptype)(const char *val, const char *str, int *siz);
		case (Nqtype): len = lua_tointeger(L,2); if (fnc.nq(lua_tostring(L,1),&len))
		lua_pushnumber(L,1); else lua_pushnil(L); lua_pushnumber(L,len); ret = 2;
		break; // typedef int (*nqtype)(const char *str, int *siz);

		case (Rjtype): lua_pushstring(L,fnc.rj(lua_tointeger(L,1))); ret = 1; break; // typedef const char *(*rjtype)(int i);
		LUAXCASE01(Rk,rk,U) // typedef void (*hntype)(void *dat);

		default: printf("fnc.ft:%d\n",fnc.ft); ERROR();}
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
	luaxExtend(L,"nestInit",protoTypeHg(nestInit));
	luaxExtend(L,"nestElem",protoTypeHk(nestElem));
	luaxExtend(L,"nestScan",protoTypeCh(nestScan));
	luaxExtend(L,"nestPass",protoTypeTm(nestPass));
	luaxExtend(L,"nestRepl",protoTypeRj(nestRepl));
	return 0;
}
