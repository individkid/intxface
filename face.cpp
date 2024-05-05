extern "C" {
#include <lua.h>
#include "face.h"
#include <stdlib.h>
#include <string.h>
void luaxWrap(lua_State *L, const char *str, const struct Close *arg);
void wrapFace(lua_State *L);
lua_State *facelua = 0;
char *luanote = 0;
char *luafunc = 0;
void noteLua(int idx)
{
	lua_getglobal(facelua, luanote);
	lua_pushinteger(facelua, idx);
	lua_pcall(facelua, 1, 0, 0);
}
void errLua(const char *str, int num, int idx)
{
	lua_getglobal(facelua, luafunc);
	lua_pushstring(facelua, str);
	lua_pushinteger(facelua, num);
	lua_pushinteger(facelua, idx);
	lua_pcall(facelua, 3, 0, 0);
}
void noteFuncLua(const char *str)
{
	noteFunc(noteLua);
	if (luanote) free(luanote);
	luanote = strdup(str);
}
void errFuncLua(const char *str)
{
	errFunc(errLua);
	if (luafunc) free(luafunc);
	luafunc = strdup(str);
}
int hideFieldLua(lua_State *lua)
{
	int siz = lua_tonumber(lua,3);
	int arg = lua_tonumber(lua,4);
	int *sub = (int*)malloc(arg*sizeof(int));
	for (int i = 0; i < arg; i++) sub[i] = lua_tonumber(lua,5+i);
	if (hideFieldV(lua_tostring(lua,1),lua_tostring(lua,2),&siz,arg,sub))
	lua_pushnumber(lua,1); else lua_pushnil(lua);
	lua_pushnumber(lua,siz);
	free(sub);
	return 2;
}
int showFieldLua(lua_State *lua)
{
	char *str = strdup(lua_tostring(lua,2));
	int arg = lua_tonumber(lua,3);
	int *sub = (int*)malloc(arg*sizeof(int));
	for (int i = 0; i < arg; i++) sub[i] = lua_tonumber(lua,4+i);
	showFieldV(lua_tostring(lua,1),&str,arg,sub);
	lua_pushstring(lua,str); free(str);
	free(sub);
	return 1;
}
}
#include "wrap.h"

void wrapFace(lua_State *L)
{
	facelua = L;
	lua_pushcfunction(L, showFieldLua); lua_setglobal(L, "showField");
	lua_pushcfunction(L, hideFieldLua); lua_setglobal(L, "hideField");
	luaxWrap(L,"noteFunc",(new WrapClose([](const struct WrapClose *arg) -> void {noteFuncLua(arg->u(0));},1,0))->ua(0));
	luaxWrap(L,"errFunc",(new WrapClose([](const struct WrapClose *arg) -> void {errFuncLua(arg->u(0));},1,0))->ua(0));
	luaxWrap(L,"closeIdent",(new WrapClose([](const struct WrapClose *arg) -> void {closeIdent(arg->i(0));},1,0))->ia(0));
	luaxWrap(L,"moveIdent",(new WrapClose([](const struct WrapClose *arg) -> void {moveIdent(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1));
	luaxWrap(L,"findIdent",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=findIdent(arg->u(0));},1,1))->ua(0)->ib(0));
	luaxWrap(L,"inetIdent",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=inetIdent(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0));
	luaxWrap(L,"openPipe",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=openPipe();},0,1))->ib(0));
	luaxWrap(L,"openFile",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=openFile(arg->u(0));},1,1))->ua(0)->ib(0));
	luaxWrap(L,"openInet",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=openInet(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0));
	luaxWrap(L,"forkExec",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=forkExec(arg->u(0));},1,1))->ua(0)->ib(0));
	luaxWrap(L,"pipeInit",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=pipeInit(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0));
	luaxWrap(L,"openFork",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=openFork();},0,1))->ib(0));
	luaxWrap(L,"openCheck",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=openCheck(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"openRdfd",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=openRdfd(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"openWrfd",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=openWrfd(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"openExec",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=openExec(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0));
	luaxWrap(L,"rdwrInit",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=rdwrInit(arg->i(0),arg->i(1));},2,1))->ia(0)->ia(1)->ib(0));
	luaxWrap(L,"waitRead",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=waitRead(arg->k(0),arg->i(1));},2,1))->ka(0)->ia(1)->ib(0));
	luaxWrap(L,"waitExit",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=waitExit();},0,1))->ib(0));
	luaxWrap(L,"pollPipe",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=pollPipe(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"pollFile",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=pollFile(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"seekFile",(new WrapClose([](const struct WrapClose *arg) -> void {seekFile(arg->k(0),arg->i(1));},2,0))->ia(0)->ka(1));
	luaxWrap(L,"truncFile",(new WrapClose([](const struct WrapClose *arg) -> void {truncFile(arg->i(0));},1,0))->ia(0));
	luaxWrap(L,"checkFile",(new WrapClose([](const struct WrapClose *arg) -> void {arg->k_(0)=checkFile(arg->i(0));},1,1))->ia(0)->kb(0));
	luaxWrap(L,"rdlkFile",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=rdlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,1))->ka(0)->ka(1)->ia(2)->ib(0));
	luaxWrap(L,"wrlkFile",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=wrlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,1))->ka(0)->ka(1)->ia(2)->ib(0));
	luaxWrap(L,"unlkFile",(new WrapClose([](const struct WrapClose *arg) -> void {unlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2));
	luaxWrap(L,"rdlkwFile",(new WrapClose([](const struct WrapClose *arg) -> void {rdlkwFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2));
	luaxWrap(L,"wrlkwFile",(new WrapClose([](const struct WrapClose *arg) -> void {wrlkwFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2));
	luaxWrap(L,"checkRead",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=checkRead(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"checkWrite",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=checkWrite(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"sleepSec",(new WrapClose([](const struct WrapClose *arg) -> void {sleepSec(arg->m(0));},1,0))->ma(0));
	luaxWrap(L,"readEof",(new WrapClose([](const struct WrapClose *arg) -> void {readEof(arg->i(0));},1,0))->ia(0));
	luaxWrap(L,"readStr",(new WrapClose([](const struct WrapClose *arg) -> void {readStr(&arg->v_(0),arg->i(0));},1,1))->ia(0)->vb(0));
	luaxWrap(L,"preadStr",(new WrapClose([](const struct WrapClose *arg) -> void {preadStr(&arg->v_(0),arg->k(0),arg->i(1));},2,1))->ka(0)->ia(1)->vb(0));
	luaxWrap(L,"readDat",(new WrapClose([](const struct WrapClose *arg) -> void {readDat(&arg->q_(0),arg->i(0));},1,1))->ia(0)->pb(0));
	luaxWrap(L,"readChr",(new WrapClose([](const struct WrapClose *arg) -> void {arg->w_(0)=readChr(arg->i(0));},1,1))->ia(0)->wb(0));
	luaxWrap(L,"readInt",(new WrapClose([](const struct WrapClose *arg) -> void {arg->i_(0)=readInt(arg->i(0));},1,1))->ia(0)->ib(0));
	luaxWrap(L,"readInt32",(new WrapClose([](const struct WrapClose *arg) -> void {arg->j_(0)=readInt32(arg->i(0));},1,1))->ia(0)->jb(0));
	luaxWrap(L,"readNew",(new WrapClose([](const struct WrapClose *arg) -> void {arg->k_(0)=readNew(arg->i(0));},1,1))->ia(0)->kb(0));
	luaxWrap(L,"readNum",(new WrapClose([](const struct WrapClose *arg) -> void {arg->m_(0)=readNum(arg->i(0));},1,1))->ia(0)->mb(0));
	luaxWrap(L,"readOld",(new WrapClose([](const struct WrapClose *arg) -> void {arg->n_(0)=readOld(arg->i(0));},1,1))->ia(0)->nb(0));
	luaxWrap(L,"writeStr",(new WrapClose([](const struct WrapClose *arg) -> void {writeStr(arg->u(0),arg->i(1));},2,0))->ua(0)->ia(1));
	luaxWrap(L,"pwriteStr",(new WrapClose([](const struct WrapClose *arg) -> void {pwriteStr(arg->u(0),arg->k(1),arg->i(2));},3,0))->ua(0)->ka(1)->ia(2));
	luaxWrap(L,"writeDat",(new WrapClose([](const struct WrapClose *arg) -> void {writeDat(arg->p(0),arg->i(1));},2,0))->pa(0)->ia(1));
	luaxWrap(L,"writeChr",(new WrapClose([](const struct WrapClose *arg) -> void {writeChr(arg->w(0),arg->i(1));},2,0))->wa(0)->ia(1));
	luaxWrap(L,"writeInt",(new WrapClose([](const struct WrapClose *arg) -> void {writeInt(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1));
	luaxWrap(L,"writeInt32",(new WrapClose([](const struct WrapClose *arg) -> void {writeInt32(arg->j(0),arg->i(1));},2,0))->ja(0)->ia(1));
	luaxWrap(L,"writeNum",(new WrapClose([](const struct WrapClose *arg) -> void {writeNum(arg->m(0),arg->i(1));},2,0))->ma(0)->ia(1));
	luaxWrap(L,"writeNew",(new WrapClose([](const struct WrapClose *arg) -> void {writeNew(arg->k(0),arg->i(1));},2,0))->ka(0)->ia(1));
	luaxWrap(L,"writeOld",(new WrapClose([](const struct WrapClose *arg) -> void {writeOld(arg->n(0),arg->i(1));},2,0))->na(0)->ia(1));
	luaxWrap(L,"showStr",(new WrapClose([](const struct WrapClose *arg) -> void {showStr(arg->u(0),&arg->s_(1));arg->s(0);},2,1))->ua(0)->ua(1)->vb(0));
	luaxWrap(L,"showDat",(new WrapClose([](const struct WrapClose *arg) -> void {showDat(arg->p(0),&arg->s_(1));arg->s(0);},2,1))->pa(0)->ua(1)->vb(0));
	luaxWrap(L,"showChr",(new WrapClose([](const struct WrapClose *arg) -> void {showChr(arg->w(0),&arg->s_(1));arg->s(0);},2,1))->wa(0)->ua(1)->vb(0));
	luaxWrap(L,"showInt",(new WrapClose([](const struct WrapClose *arg) -> void {showInt(arg->i(0),&arg->s_(1));arg->s(0);},2,1))->ia(0)->ua(1)->vb(0));
	luaxWrap(L,"showInt32",(new WrapClose([](const struct WrapClose *arg) -> void {showInt32(arg->j(0),&arg->s_(1));arg->s(0);},2,1))->ja(0)->ua(1)->vb(0));
	luaxWrap(L,"showNew",(new WrapClose([](const struct WrapClose *arg) -> void {showNew(arg->k(0),&arg->s_(1));arg->s(0);},2,1))->ka(0)->ua(1)->vb(0));
	luaxWrap(L,"showNum",(new WrapClose([](const struct WrapClose *arg) -> void {showNum(arg->m(0),&arg->s_(1));arg->s(0);},2,1))->ma(0)->ua(1)->vb(0));
	luaxWrap(L,"showOld",(new WrapClose([](const struct WrapClose *arg) -> void {showOld(arg->n(0),&arg->s_(1));arg->s(0);},2,1))->na(0)->ua(1)->vb(0));
	luaxWrap(L,"showEnum",(new WrapClose([](const struct WrapClose *arg) -> void {showEnum(arg->u(0),arg->u(1),&arg->s_(2));arg->s(0);},3,1))->ua(0)->ua(1)->ua(2)->vb(0));
	luaxWrap(L,"showOpen",(new WrapClose([](const struct WrapClose *arg) -> void {showOpen(arg->u(0),&arg->s_(1));arg->s(0);},2,1))->ua(0)->ua(1)->vb(0));
	luaxWrap(L,"showClose",(new WrapClose([](const struct WrapClose *arg) -> void {showClose(&arg->s_(0));arg->s(0);},1,1))->ua(0)->vb(0));
	luaxWrap(L,"hideStr",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideStr(&arg->v_(0),arg->u(0),&arg->t_(1,1));},2,2))->ua(0)->ia(1)->vb(0)->ib(1));
	luaxWrap(L,"hideDat",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideDat(&arg->q_(0),arg->u(0),&arg->t_(1,1));},2,2))->ua(0)->ia(1)->qb(0)->ib(1));
	luaxWrap(L,"hideChr",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideChr(&arg->w_(0),arg->u(0),&arg->t_(1,1));},2,2))->ua(0)->ia(1)->wb(0)->ib(1));
	luaxWrap(L,"hideInt",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideInt(&arg->i_(0),arg->u(0),&arg->t_(1,1));},2,2))->ua(0)->ia(1)->ib(0)->ib(1));
	luaxWrap(L,"hideInt32",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideInt32(&arg->j_(0),arg->u(0),&arg->t_(1,1));},2,2))->ua(0)->ia(1)->jb(0)->ib(1));
	luaxWrap(L,"hideNew",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideNew(&arg->k_(0),arg->u(0),&arg->t_(1,1));},2,2))->ua(0)->ia(1)->kb(0)->ib(1));
	luaxWrap(L,"hideNum",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideNum(&arg->m_(0),arg->u(0),&arg->t_(1,1));},2,2))->ua(0)->ia(1)->mb(0)->ib(1));
	luaxWrap(L,"hideOld",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideOld(&arg->n_(0),arg->u(0),&arg->t_(1,1));},2,2))->ua(0)->ia(1)->nb(0)->ib(1));
	luaxWrap(L,"hideEnum",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideEnum(arg->u(0),arg->u(1),arg->u(2),&arg->t_(3,1));arg->i_(0)=1;},4,2))->ua(0)->ua(1)->ua(2)->ia(3)->ib(0)->ib(1));
	luaxWrap(L,"hideOpen",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideOpen(arg->u(0),arg->u(1),&arg->t_(2,1));arg->i_(0)=1;},3,2))->ua(0)->ua(1)->ia(2)->ib(0)->ib(1));
	luaxWrap(L,"hideClose",(new WrapClose([](const struct WrapClose *arg) -> void {arg->r(0)=!hideClose(arg->u(0),&arg->t_(1,1));arg->i_(0)=1;},2,2))->ua(0)->ia(1)->ib(0)->ib(1));
}
