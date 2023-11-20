extern "C" {
#include <lua.h>
#include "face.h"
void wrapFace(lua_State *L);
}
#include "wrap.h"

void wrapFace(lua_State *L)
{
	(new WrapClose(L,"noteFunc",[](const struct WrapClose *arg) -> void {noteFuncLua(arg->u(0));},1,0))->ua(0);
	(new WrapClose(L,"errFunc",[](const struct WrapClose *arg) -> void {errFuncLua(arg->u(0));},1,0))->ua(0);
	(new WrapClose(L,"closeIdent",[](const struct WrapClose *arg) -> void {closeIdent(arg->i(0));},1,0))->ia(0);
	(new WrapClose(L,"moveIdent",[](const struct WrapClose *arg) -> void {moveIdent(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1);
	(new WrapClose(L,"findIdent",[](const struct WrapClose *arg) -> void {arg->i_(0)=findIdent(arg->u(0));},1,1))->ua(0)->ib(0);
	(new WrapClose(L,"inetIdent",[](const struct WrapClose *arg) -> void {arg->i_(0)=inetIdent(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0);
	(new WrapClose(L,"openPipe",[](const struct WrapClose *arg) -> void {arg->i_(0)=openPipe();},0,1))->ib(0);
	(new WrapClose(L,"openFile",[](const struct WrapClose *arg) -> void {arg->i_(0)=openFile(arg->u(0));},1,1))->ua(0)->ib(0);
	(new WrapClose(L,"openInet",[](const struct WrapClose *arg) -> void {arg->i_(0)=openInet(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0);
	(new WrapClose(L,"forkExec",[](const struct WrapClose *arg) -> void {arg->i_(0)=forkExec(arg->u(0));},1,1))->ua(0)->ib(0);
	(new WrapClose(L,"pipeInit",[](const struct WrapClose *arg) -> void {arg->i_(0)=pipeInit(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0);
	(new WrapClose(L,"openFork",[](const struct WrapClose *arg) -> void {arg->i_(0)=openFork();},0,1))->ib(0);
	(new WrapClose(L,"openCheck",[](const struct WrapClose *arg) -> void {arg->i_(0)=openCheck(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose(L,"openRdfd",[](const struct WrapClose *arg) -> void {arg->i_(0)=openRdfd(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose(L,"openWrfd",[](const struct WrapClose *arg) -> void {arg->i_(0)=openWrfd(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose(L,"openExec",[](const struct WrapClose *arg) -> void {arg->i_(0)=openExec(arg->u(0),arg->u(1));},2,1))->ua(0)->ua(1)->ib(0);
	(new WrapClose(L,"rdwrInit",[](const struct WrapClose *arg) -> void {arg->i_(0)=rdwrInit(arg->i(0),arg->i(1));},2,1))->ia(0)->ia(1)->ib(0);
	(new WrapClose(L,"waitRead",[](const struct WrapClose *arg) -> void {arg->i_(0)=waitRead(arg->k(0),arg->i(1));},2,1))->ka(0)->ia(1)->ib(0);
	(new WrapClose(L,"waitExit",[](const struct WrapClose *arg) -> void {arg->i_(0)=waitExit();},0,1))->ib(0);
	(new WrapClose(L,"pollPipe",[](const struct WrapClose *arg) -> void {arg->i_(0)=pollPipe(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose(L,"pollFile",[](const struct WrapClose *arg) -> void {arg->i_(0)=pollFile(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose(L,"seekFile",[](const struct WrapClose *arg) -> void {seekFile(arg->k(0),arg->i(1));},2,0))->ia(0)->ka(1);
	(new WrapClose(L,"truncFile",[](const struct WrapClose *arg) -> void {truncFile(arg->i(0));},1,0))->ia(0);
	(new WrapClose(L,"checkFile",[](const struct WrapClose *arg) -> void {arg->k_(0)=checkFile(arg->i(0));},1,1))->ia(0)->kb(0);
	(new WrapClose(L,"rdlkFile",[](const struct WrapClose *arg) -> void {arg->i_(0)=rdlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,1))->ka(0)->ka(1)->ia(2)->ib(0);
	(new WrapClose(L,"wrlkFile",[](const struct WrapClose *arg) -> void {arg->i_(0)=wrlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,1))->ka(0)->ka(1)->ia(2)->ib(0);
	(new WrapClose(L,"unlkFile",[](const struct WrapClose *arg) -> void {unlkFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2);
	(new WrapClose(L,"rdlkwFile",[](const struct WrapClose *arg) -> void {rdlkwFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2);
	(new WrapClose(L,"wrlkwFile",[](const struct WrapClose *arg) -> void {wrlkwFile(arg->k_(0),arg->k_(1),arg->i_(2));},3,0))->ka(0)->ka(1)->ia(2);
	(new WrapClose(L,"checkRead",[](const struct WrapClose *arg) -> void {arg->i_(0)=checkRead(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose(L,"checkWrite",[](const struct WrapClose *arg) -> void {arg->i_(0)=checkWrite(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose(L,"sleepSec",[](const struct WrapClose *arg) -> void {sleepSec(arg->m(0));},1,0))->ma(0);
	(new WrapClose(L,"readEof",[](const struct WrapClose *arg) -> void {readEof(arg->i(0));},1,0))->ia(0);
	(new WrapClose(L,"readStr",[](const struct WrapClose *arg) -> void {readStr(&arg->v_(0),arg->i(0));},1,1))->ia(0)->vb(0);
	(new WrapClose(L,"preadStr",[](const struct WrapClose *arg) -> void {preadStr(&arg->v_(0),arg->k(0),arg->i(1));},2,1))->ka(0)->ia(1)->vb(0);
	(new WrapClose(L,"readDat",[](const struct WrapClose *arg) -> void {readDat(&arg->q_(0),arg->i(0));},1,1))->ia(0)->pb(0);
	(new WrapClose(L,"readChr",[](const struct WrapClose *arg) -> void {arg->w_(0)=readChr(arg->i(0));},1,1))->ia(0)->wb(0);
	(new WrapClose(L,"readInt",[](const struct WrapClose *arg) -> void {arg->i_(0)=readInt(arg->i(0));},1,1))->ia(0)->ib(0);
	(new WrapClose(L,"readInt32",[](const struct WrapClose *arg) -> void {arg->j_(0)=readInt32(arg->i(0));},1,1))->ia(0)->jb(0);
	(new WrapClose(L,"readNew",[](const struct WrapClose *arg) -> void {arg->k_(0)=readNew(arg->i(0));},1,1))->ia(0)->kb(0);
	(new WrapClose(L,"readNum",[](const struct WrapClose *arg) -> void {arg->m_(0)=readNum(arg->i(0));},1,1))->ia(0)->mb(0);
	(new WrapClose(L,"readOld",[](const struct WrapClose *arg) -> void {arg->n_(0)=readOld(arg->i(0));},1,1))->ia(0)->nb(0);
	(new WrapClose(L,"writeStr",[](const struct WrapClose *arg) -> void {writeStr(arg->u(0),arg->i(1));},2,0))->ua(0)->ia(1);
	(new WrapClose(L,"pwriteStr",[](const struct WrapClose *arg) -> void {pwriteStr(arg->u(0),arg->k(1),arg->i(2));},3,0))->ua(0)->ka(1)->ia(2);
	(new WrapClose(L,"writeDat",[](const struct WrapClose *arg) -> void {writeDat(arg->p(0),arg->i(1));},2,0))->pa(0)->ia(1);
	(new WrapClose(L,"writeChr",[](const struct WrapClose *arg) -> void {writeChr(arg->w(0),arg->i(1));},2,0))->wa(0)->ia(1);
	(new WrapClose(L,"writeInt",[](const struct WrapClose *arg) -> void {writeInt(arg->i(0),arg->i(1));},2,0))->ia(0)->ia(1);
	(new WrapClose(L,"writeInt32",[](const struct WrapClose *arg) -> void {writeInt32(arg->j(0),arg->i(1));},2,0))->ja(0)->ia(1);
	(new WrapClose(L,"writeNum",[](const struct WrapClose *arg) -> void {writeNum(arg->m(0),arg->i(1));},2,0))->ma(0)->ia(1);
	(new WrapClose(L,"writeNew",[](const struct WrapClose *arg) -> void {writeNew(arg->k(0),arg->i(1));},2,0))->ka(0)->ia(1);
	(new WrapClose(L,"writeOld",[](const struct WrapClose *arg) -> void {writeOld(arg->n(0),arg->i(1));},2,0))->na(0)->ia(1);
	(new WrapClose(L,"showStr",[](const struct WrapClose *arg) -> void {showStr(arg->u(0),&arg->s_(1));arg->s(0);},2,1))->ua(0)->ua(1)->vb(0);
	(new WrapClose(L,"showDat",[](const struct WrapClose *arg) -> void {showDat(arg->p(0),&arg->s_(1));arg->s(0);},2,1))->pa(0)->ua(1)->vb(0);
	(new WrapClose(L,"showChr",[](const struct WrapClose *arg) -> void {showChr(arg->w(0),&arg->s_(1));arg->s(0);},2,1))->wa(0)->ua(1)->vb(0);
	(new WrapClose(L,"showInt",[](const struct WrapClose *arg) -> void {showInt(arg->i(0),&arg->s_(1));arg->s(0);},2,1))->ia(0)->ua(1)->vb(0);
	(new WrapClose(L,"showInt32",[](const struct WrapClose *arg) -> void {showInt32(arg->j(0),&arg->s_(1));arg->s(0);},2,1))->ja(0)->ua(1)->vb(0);
	(new WrapClose(L,"showNew",[](const struct WrapClose *arg) -> void {showNew(arg->k(0),&arg->s_(1));arg->s(0);},2,1))->ka(0)->ua(1)->vb(0);
	(new WrapClose(L,"showNum",[](const struct WrapClose *arg) -> void {showNum(arg->m(0),&arg->s_(1));arg->s(0);},2,1))->ma(0)->ua(1)->vb(0);
	(new WrapClose(L,"showOld",[](const struct WrapClose *arg) -> void {showOld(arg->n(0),&arg->s_(1));arg->s(0);},2,1))->na(0)->ua(1)->vb(0);
	(new WrapClose(L,"showEnum",[](const struct WrapClose *arg) -> void {showEnum(arg->u(0),arg->u(1),&arg->s_(2));arg->s(0);},3,1))->ua(0)->ua(1)->ua(2)->vb(0);
	(new WrapClose(L,"showOpen",[](const struct WrapClose *arg) -> void {showOpen(arg->u(0),&arg->s_(1));arg->s(0);},2,1))->ua(0)->ua(1)->vb(0);
	(new WrapClose(L,"showClose",[](const struct WrapClose *arg) -> void {showClose(&arg->s_(0));arg->s(0);},1,1))->ua(0)->vb(0);
	(new WrapClose(L,"hideStr",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideStr(&arg->v_(0),arg->u(0),&arg->t_(1));arg->t(1);},2,2))->ua(0)->ia(1)->vb(0)->ib(1);
	(new WrapClose(L,"hideDat",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideDat(&arg->q_(0),arg->u(0),&arg->t_(1));arg->t(1);},2,2))->ua(0)->ia(1)->qb(0)->ib(1);
	(new WrapClose(L,"hideChr",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideChr(&arg->w_(0),arg->u(0),&arg->t_(1));arg->t(1);},2,2))->ua(0)->ia(1)->wb(0)->ib(1);
	(new WrapClose(L,"hideInt",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideInt(&arg->i_(0),arg->u(0),&arg->t_(1));arg->t(1);},2,2))->ua(0)->ia(1)->ib(0)->ib(1);
	(new WrapClose(L,"hideInt32",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideInt32(&arg->j_(0),arg->u(0),&arg->t_(1));arg->t(1);},2,2))->ua(0)->ia(1)->jb(0)->ib(1);
	(new WrapClose(L,"hideNew",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideNew(&arg->k_(0),arg->u(0),&arg->t_(1));arg->t(1);},2,2))->ua(0)->ia(1)->kb(0)->ib(1);
	(new WrapClose(L,"hideNum",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideNum(&arg->m_(0),arg->u(0),&arg->t_(1));arg->t(1);},2,2))->ua(0)->ia(1)->mb(0)->ib(1);
	(new WrapClose(L,"hideOld",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideOld(&arg->n_(0),arg->u(0),&arg->t_(1));arg->t(1);},2,2))->ua(0)->ia(1)->nb(0)->ib(1);
	(new WrapClose(L,"hideEnum",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideEnum(arg->u(0),arg->u(1),arg->u(2),&arg->t_(3));arg->i_(0)=1;arg->t(1);},4,2))->ua(0)->ua(1)->ua(2)->ia(3)->ib(0)->ib(1);
	(new WrapClose(L,"hideOpen",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideOpen(arg->u(0),arg->u(1),&arg->t_(2));arg->i_(0)=1;arg->t(1);},3,2))->ua(0)->ua(1)->ia(2)->ib(0)->ib(1);
	(new WrapClose(L,"hideClose",[](const struct WrapClose *arg) -> void {arg->r(0)=!hideClose(arg->u(0),&arg->t_(1));arg->i_(0)=1;arg->t(1);},2,2))->ua(0)->ia(1)->ib(0)->ib(1);
}
