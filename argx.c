#include "argx.h"
#include "memx.h"
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>
#include <stdlib.h>

struct ArgxNest nst[NUMARGX] = {0}; // data flow control steps
int lst = 0; // number of steps
const char *str[NUMARGX] = {0}; // dash option types
struct ArgxNest fnc[NUMARGX] = {0}; // function to use
int ltr = 0; // number of types
int use = 0; // which fnc to use
int opt = 0; // which dash matched
int idx = 0; // program counter
void *map = 0; // format for jump

int nestJumpF(int idx, int dir, int cnt, int cmp, int lvl)
{
	int dpt = 0;
	if (dir > 1 || dir < -1 || dir == 0) ERROR(exitErr,0);
	if (cmp > 1 || cmp < -1 || cmp == 0) ERROR(exitErr,0);
	while (idx >= 0 && idx < lst && cnt > 0) {
		int cnd = 0; // maybe count if flow or nest exit
		if (nst[idx].opt == NestTag) {
			int dif = memxInt(nst[idx].run);
			dpt += dir*dif;
			cnd = ((dir > 0) != (dif > 0));}
		if (nst[idx].opt == FlowTag) cnd = 1;
		// count if given level is given relation to nest level
		if (cnd && ((cmp > 0) == (dpt > lvl) || dpt == lvl)) cnt -= 1;
		idx += dir;}
	return idx;
}
int nestJump(int idx, void *jmp)
{
	int stp = 0;
	while (idx >= 0 && idx < lst && stp < memxSize(jmp)) {
		enum ArgxStep dir = memxInt(memxSkip(memxSkip(jmp,stp),0));
		int cnt = memxInt(memxSkip(memxSkip(jmp,stp),1));
		switch (dir) {
		case (FwdSkpStep): idx = nestJumpF(idx,1,cnt,-1,0); break;
		case (RevSkpStep): idx = nestJumpF(idx,-1,cnt,-1,0); break;
		case (FwdEntStep): idx = nestJumpF(idx,1,1,1,cnt); break;
		case (RevEntStep): idx = nestJumpF(idx,-1,1,1,cnt); break;
		case (FwdExtStep): idx = nestJumpF(idx,1,1,-1,-cnt); break;
		case (RevExtStep): idx = nestJumpF(idx,-1,1,-1,-cnt); break;
		default: break;}
	}
	return idx;
}
int argxJump(void *use)
{
	void *tmp = 0;
	const char *str = 0;
	void *mem = 0;
	int val = 0;
	memxCopy(&tmp,use);
	str = memxStr(tmp);
	memxForm(&mem,str,map);
	val = nestJump(idx,mem);
	memxDone(&tmp);
	memxDone(&mem);
	return val;
}
void argxCopy(void **run, void *use)
{
	memxCopy(run,nst[nestJump(idx,use)].run);
}
void argxKeep(void **run, void *use)
{
	memxKeep(run,nst[nestJump(idx,use)].run);
}
struct ArgxNest *argxGet(int idx)
{
	return nst+idx;
}
int argxHere()
{
	return idx;
}
int getLocation()
{
	nst[lst].opt = NoopTag;
	return lst++;
}
int addOption(const char *opt, enum ArgxTag opc, struct Prototype use, struct Prototype run)
{
	str[ltr] = opt;
	fnc[ltr].opc = opc;
	fnc[ltr].fnc = use;
	fnc[ltr].gnc = run;
	return ltr++;
}
int mapCallback(const char *str, int ref, struct Prototype fnc)
{
	return 0; // TODO add globals
}
int mapDefault(const char *str, int ref, struct Prototype fnc)
{
	return 0; // TODO add globals
}
int useArgument(const char *arg)
{
	int result = lst;
	if (arg[0] == '-' && arg[1] != '-') {
		for (int i = 0; i < ltr; i++) {
			for (int j = 0; str[i][j]; j++) {
				if (arg[1] == str[i][j]) {
					use = i; opt = j;}}}}
	else {
		nst[lst] = fnc[use];
		nst[lst].opt = opt;
		memxInit(&nst[lst].str,arg);
		memxCall(&nst[lst].use,nst[lst].str,nst[lst].fnc);
		lst++;}
	return result;
}
void runProgram()
{
	// TODO initialize map, + - for forward backward, ( ) = for enter exit skip
	while (idx >= 0 && idx < lst) {
		if (nst[idx].opt != NoopTag) memxCall(&nst[idx].run,nst[idx].use,nst[idx].gnc);
		if (nst[idx].opt == JumpTag) idx = memxInt(nst[idx].run);
		else idx++;}
}