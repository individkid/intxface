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
	int bas; // 
	int len; // length of exp in string
	int wrp; // whether evaluation wrapped
	int pos; // position of yield
	int nst; // nesting level of yield
	char *str; // result of last evaluation
};
struct Enter *ent = 0; // expressions of line
int dim = 0; // number of expressions in line
int max = 0; // size of expression array
const char **line = 0; // strings of line
int mline = 0; // number of strings in line
int msize = 0; // size of string array
lua_State *luastate = 0;
lua_State *luatemp = 0;

void nestErr(const char *str, int num, int idx)
{
	fprintf(stderr,"nestErr %s(%d): %d %lld\n",str,num,errno,(long long)getpid());
	exit(-1);
}
void *hideLua(void *ud, void *ptr, size_t osize, size_t nsize)
{
	if (nsize == 0) {free(ptr); return 0;}
	return realloc(ptr, nsize);
}
int hideNest(char **str)
 {
	char *bas = strchr(*str,'(');
	char *lim = strchr(*str,')');
	if (!lim) ERROR(nestErr,0);
	if (bas && bas < lim) {*str = bas+1; return 1;}
	if (!bas || lim < bas) {*str = lim+1; return -1;}
	return 0;
}
// TODO first nestDone allocates the line string array
// TODO then nestElem records a line string
// TODO then nestLine finds expressions
// TODO then nestPass evaluates the wrapped or top expressions to update the result strings
// TODO then nestNext gets indicated string with result string replacements
int hidePercent(char **ret, const char *str, lftype fnc)
{
	regex_t castex = {0};
	regex_t exprex = {0};
	regmatch_t match[5];
	if (!luastate) luastate = lua_newstate(hideLua,0);
	if (asprintf(ret,"%s",str) < 0) ERROR(nestErr,0);
	if (dim < 0) {dim = 0; return 0;}
	if (ent && ent[dim - 1].wrp) {free(ent); ent = 0; dim = 0; return 0;}
	if (regcomp(&castex,"(.*)Int%([A-F][A-Fa-f0-9]*)\\(([A-F][A-Fa-f0-9]*)\\)(.*)",REG_MINIMAL) != 0) ERROR(nestErr,0);
	if (regcomp(&exprex,"(.*)%(",REG_MINIMAL) != 0) ERROR(nestErr,0);
	while (regexec(&castex,*ret,5,match,0) == 0) {
		int val = 0;
		int nln = 0;
		char *tmp[4] = {0};
		int ofs[4] = {0};
		int len[4] = {0};
		for (int i = 0; i < 4; i++) ofs[i] = match[i+1].rm_so;
		for (int i = 0; i < 4; i++) len[i] = match[i+1].rm_eo-ofs[i];
		for (int i = 0; i < 4; i++) tmp[i] = malloc(len[i]+1);
		for (int i = 0; i < 4; i++) strncpy(tmp[i],*ret+ofs[i],len[i]);
		for (int i = 0; i < 4; i++) tmp[i][len[i]] = 0;
		nln = strlen(tmp[2]);
		if (!fnc(&val,tmp[1],tmp[2],&nln)) ERROR(nestErr,0);
		free(*ret); *ret = 0;
		if (asprintf(ret,"%sInt(%d)%s",tmp[0],val,tmp[3]) < 0) ERROR(nestErr,0);
		for (int i = 0; i < 4; i++) free(tmp[i]);}
	if (!ent) {
		char *exp = 0;
		for (dim = 0, exp = *ret; regexec(&exprex,exp,2,match,0) == 0; dim += 1) {
			exp += match[1].rm_eo + 2; for (int nst = 1; nst; nst += hideNest(&exp));}
		if (dim) ent = malloc((dim)*sizeof(struct Enter));
		for (dim = 0, exp = *ret; regexec(&exprex,exp,2,match,0) == 0; dim += 1) {
			char *str = 0;
			char *bas = 0;
			char *lim = 0;
			bas = exp += match[1].rm_eo + 2; for (int nst = 1; nst; nst += hideNest(&exp));
			lim = exp - 1;
			ent[dim].wrp = 0;
			ent[dim].pos = 0;
			ent[dim].nst = 0;
			ent[dim].str = str = malloc(lim-bas+1);
			strncpy(str,bas,lim-bas);
			str[lim-bas] = 0;}}
	for (dim = 0; regexec(&exprex,*ret,2,match,0) == 0; dim += 1) {
		int num = 0;
		char *tmp[2] = {0};
		int ofs[2] = {0};
		int len[2] = {0};
		char *exp = 0;
		ofs[0] = 0; len[0] = match[1].rm_eo;
		exp = *ret + match[1].rm_eo + 2; for (int nst = 1; nst; nst += hideNest(&exp));
		ofs[1] = exp-*ret; len[1] = strlen(*ret) - ofs[1];
		for (int i = 0; i < 2; i++) tmp[i] = malloc(len[i]+1);
		for (int i = 0; i < 2; i++) strncpy(tmp[i],*ret+ofs[i],len[i]);
		for (int i = 0; i < 2; i++) tmp[i][len[i]] = 0;
		num = 0; // TODO planeEnter(ent,dim); increment pos if dim is 0 or ent of dim-1 is wrp; set wrp if pos wraps
		free(*ret); *ret = 0;
		if (asprintf(ret,"%s%d%s",tmp[0],num,tmp[1]) < 0) ERROR(nestErr,0);
		for (int i = 0; i < 2; i++) free(tmp[i]);}
	if (!dim) dim = -1;
	return 1;
}
int hideScript(int *val, const char *str, int arg)
{
	if (!luastate) luastate = lua_newstate(hideLua,0);
	lua_getglobal(luastate,"load");
	lua_pushstring(luastate,str);
	if (lua_pcall(luastate, 1, 1, 0) != 0) ERROR(nestErr,0)
	lua_pushnumber(luastate,arg);
	if (lua_pcall(luastate, 1, 1, 0) != 0) ERROR(nestErr,0)
	*val = lua_tonumber(luastate,1);
	lua_pop(luastate,1);
	return 1;
}

int funcPercent(int *val, const char *typ, const char *str, int *siz)
{
	int ret = 0;
	lua_pushinteger(luatemp,0);
	lua_copy(luatemp,2,1);
	lua_pushstring(luatemp,typ);
	lua_pushstring(luatemp,str);
	lua_call(luatemp,2,3);
	ret = lua_tonumber(luatemp,1);
	*val = lua_tonumber(luatemp,2);
	*siz = lua_tonumber(luatemp,3);
	lua_pop(luatemp,3);
	return ret;
}
int hidePercentLua(lua_State *lua)
{
	const char *str = lua_tostring(lua,1);
	char *ret = 0;
	lua_pop(lua,1);
	luatemp = lua;
	lua_pushnumber(lua,hidePercent(&ret,str,funcPercent));
	lua_pushstring(lua,ret);
	return 2;
}
int hideScriptLua(lua_State *lua)
{
	int val = 0;
	lua_pushnumber(lua,hideScript(&val, lua_tostring(lua,1), lua_tonumber(lua,2)));
	lua_pushnumber(lua,val);
	return 2;
}

int luaopen_nest(lua_State *L)
{
	lua_pushcfunction(L, hidePercentLua);
	lua_setglobal(L, "hidePercent");
	lua_pushcfunction(L, hideScriptLua);
	lua_setglobal(L, "hideScript");
	return 0;
}