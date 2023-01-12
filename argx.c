#include "argx.h"
#include "memx.h"
#include "type.h"
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>
#include <stdlib.h>

enum ArgxTag {
	FlowTag, // data flow
	FlagTag, // option flag
	JumpTag, // loop break nest
	NestTag, // nesting control
	NoopTag, // data container
	ArgxTags
};
enum ArgxOpc {
	TypeOpc, // operation type
	CallOpc, // change callback
	DfltOpc, // identity callback
	CompOpc, // compile callback
	InstOpc, // operation instance
	ArgxOpcs,
};
struct ArgxNest {
	enum ArgxTag tag; // type of data flow control step
	const char *opt; // commandline arguments
	void *str; // string after dash option
	void *use; // script or constant
	void *run; // result or copy
	struct Function fnc; // for str to use
	struct Function gnc; // for use to run
	int ref; // argument for callback
};
struct ArgxNest arg[ArgxOpcs][NUMARGX] = {0};
int len[ArgxOpcs] = {0};
struct ArgxNest *use = 0; // which fnc to use
char opt = 0; // which dash matched
int vld = 0; // whether dash matched
int idx = 0; // program counter

int addFunc(enum ArgxOpc opc, const char *opt, int ref, struct Function fnc)
{
	struct ArgxNest *prg = &arg[opc][len[opc]];
	prg->opt = opt;
	prg->fnc = fnc;
	prg->ref = ref;
	return len[opc]++;
}
int addGunc(enum ArgxOpc opc, const char *opt, struct Function use, struct Function run, enum ArgxTag tag)
{
	struct ArgxNest *prg = &arg[opc][len[opc]];
	prg->opt = opt;
	prg->tag = tag;
	prg->fnc = use;
	prg->gnc = run;
	return len[opc]++;
}
int nestFind(enum ArgxOpc opc, char opt, struct ArgxNest **use)
{
	for (int i = 0; i < len[opc]; i++) {
		struct ArgxNest *prg = &arg[opc][i];
		if (prg->opt[0] == 0 && opt == 0) {*use = &arg[opc][i]; return 1;}
		for (int j = 0; prg->opt[j]; j++) {
			if (opt == prg->opt[j]) {*use = &arg[opc][i]; return 1;}}}
	return 0;
}

int nestJumpF(int idx, int dir, int cnt, int cmp, int lvl)
{
	int dpt = 0;
	if (dir > 1 || dir < -1 || dir == 0) ERROR();
	if (cmp > 1 || cmp < -1 || cmp == 0) ERROR();
	while (idx >= 0 && idx < len[InstOpc] && cnt > 0) {
		int cnd = 0; // maybe count if flow or nest exit
		if (arg[InstOpc][idx].tag == NestTag) {
			int dif = memxInt(arg[InstOpc][idx].run);
			dpt += dir*dif;
			cnd = ((dir > 0) != (dif > 0));}
		if (arg[InstOpc][idx].tag == FlowTag) cnd = 1;
		// count if given level is given relation to nest level
		if (cnd && ((cmp > 0) == (dpt > lvl) || dpt == lvl)) cnt -= 1;
		idx += dir;}
	return idx;
}
int nestJump(int idx, void *jmp)
{
	int stp = 0;
	while (idx >= 0 && idx < len[InstOpc] && stp < memxSize(jmp)) {
		void *mem = memxSkip(jmp,stp);
		int cnt = memxInt(memxSkip(mem,1));
		switch ((enum Step)memxInt(memxSkip(mem,0))) {
		case (FwdSkp): idx = nestJumpF(idx,1,cnt,-1,0); break;
		case (RevSkp): idx = nestJumpF(idx,-1,cnt,-1,0); break;
		case (FwdEnt): idx = nestJumpF(idx,1,1,1,cnt); break;
		case (RevEnt): idx = nestJumpF(idx,-1,1,1,cnt); break;
		case (FwdExt): idx = nestJumpF(idx,1,1,-1,-cnt); break;
		case (RevExt): idx = nestJumpF(idx,-1,1,-1,-cnt); break;
		default: ERROR();}
	}
	return idx;
}
int argxJump(void *use)
{
	void *tmp = 0;
	const char *str = 0;
	int val = 0;
	memxCopy(&tmp,use);
	val = nestJump(idx,tmp);
	memxDone(&tmp);
	return val;
}
void argxCopy(void **run, void *use)
{
	memxCopy(run,arg[InstOpc][nestJump(idx,use)].run);
}
void argxKeep(void **run, void *use)
{
	memxKeep(run,arg[InstOpc][nestJump(idx,use)].run);
}
struct ArgxNest *argxGet(int idx)
{
	return arg[InstOpc]+idx;
}
int argxLen()
{
	return len[InstOpc];
}
void *argxUse(int idx)
{
	return arg[InstOpc][idx].use;
}
void *argxRun(int idx)
{
	return arg[InstOpc][idx].run;
}
int argxHere()
{
	return idx;
}
int getLocation()
{
	arg[InstOpc][len[InstOpc]].tag = NoopTag;
	return len[InstOpc]++;
}
int addOption(const char *opt, struct Function use, struct Function run, enum ArgxTag tag)
{
	return addGunc(TypeOpc,opt,use,run,tag);
}
int addFlow(const char *opt, struct Function use, struct Function run)
{
	return addOption(opt,use,run,FlowTag);
}
int addFlag(const char *opt, struct Function use, struct Function run)
{
	return addOption(opt,use,run,FlagTag);
}
int addJump(const char *opt, struct Function use, struct Function run)
{
	return addOption(opt,use,run,JumpTag);
}
int addNest(const char *opt, struct Function use, struct Function run)
{
	return addOption(opt,use,run,NestTag);
}
int mapCallback(const char *opt, int ref, struct Function fnc)
{
	return addFunc(CallOpc,opt,ref,fnc);
}
int mapDefault(const char *opt, int ref, struct Function fnc)
{
	return addFunc(DfltOpc,opt,ref,fnc);
}
int mapContext(const char *opt, int ref, struct Function fnc)
{
	return addFunc(CompOpc,opt,ref,fnc);
}
int useArgument(const char *giv)
{
	int ret = argxLen(); struct ArgxNest *ptr = argxGet(ret);
	struct ArgxNest *tmp = 0; struct ArgxNest *ref = 0;
	if (vld && giv[0] == '-') vld = 0;
	if (!vld && giv[0] == '-') opt = giv[1];
	if (!vld && giv[0] != '-') opt = 0;
	if (!vld && opt == '-') {vld = 0; return ret;}
	if (!vld && !nestFind(TypeOpc,opt,&use)) ERROR();
	if (!vld && opt && use->tag != FlagTag) {vld = 1; return ret;}
	*ptr = *use; len[InstOpc]++;
	printf("calling memxConst %d %s\n",ret,giv);
	memxConst(&ptr->str,MemxStr,giv);
	if (nestFind(CompOpc,opt,&tmp)) memxBack(&ptr->use,&argxGet(tmp->ref)->use,tmp->fnc);
	memxCall(&ptr->use,ptr->str,ptr->fnc);
	if (nestFind(CallOpc,opt,&tmp)) memxBack(&ptr->run,&argxGet(tmp->ref)->run,tmp->fnc);
	if (nestFind(DfltOpc,opt,&tmp)) memxDflt(&ptr->run,&argxGet(tmp->ref)->run,tmp->fnc);
	if (use->tag == FlagTag) return ret;
	vld = 1; return ret;
}
void runProgram()
{
	memxScan();
	while (idx >= 0 && idx < argxLen()) {
		if (argxGet(idx)->tag != NoopTag) memxCall(&argxGet(idx)->run,argxGet(idx)->use,argxGet(idx)->gnc);
		if (argxGet(idx)->tag == JumpTag) idx = memxInt(argxGet(idx)->run);
		else idx++;}
}