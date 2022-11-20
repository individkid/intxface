#include "argx.h"
#include "memx.h"
#include "face.h"
#include "type.h"
#include <string.h>

int face = 0;
int faces = 0;
int type = 0;
int field = 0;
const char *flow = "abcdefghilmnopq";

void useFlowF(int idx, struct ArgxNest *nst, fftype fnc)
{
	union ArgxValue val;
	const char *str;
	memxInit(&nst[idx].arg,nst[idx].str);
	str = memxStr(nst[idx].arg);
	val.vad = fnc(str);
	memxForm(&nst[idx].arg,ArgxInt,val);
}
void useFlowG(int idx, struct ArgxNest *nst, gftype fnc)
{
	union ArgxValue val;
	const char *one, *oth;
	void *fst, *nxt;
	memxInit(&nst[idx].arg,nst[idx].str);
	fst = memxFirst(nst[idx].arg);
	nxt = memxNext(fst);
	one = memxStr(fst);
	oth = memxStr(nxt);
	val.vad = fnc(one,oth);
	memxForm(&nst[idx].arg,ArgxInt,val);
}
void useFlow(int idx, struct ArgxNest *nst)
{
	switch (flow[nst[idx].idx]) {
	case ('a'): memxInit(&nst[idx].arg,nst[idx].str); break;
	case ('b'): memxInit(&nst[idx].arg,nst[idx].str); break;
	case ('c'): memxInit(&nst[idx].arg,nst[idx].str); break; 
	case ('d'): memxInit(&nst[idx].arg,nst[idx].str); break;
	case ('e'): useFlowF(idx,nst,forkExec); break;
	case ('f'): useFlowF(idx,nst,openFile); break;
	case ('g'): useFlowF(idx,nst,openFifo); break;
	case ('h'): useFlowG(idx,nst,openInet); break;
	default: break;}
}
void runFlowF(int idx, struct ArgxNest *nst)
{
		int tmp, typ;
		struct File file; // TODO use generic generated function
		tmp = memxInt(nst[face].arg);
		typ = memxInt(nst[idx].arg);
		readFile(&file,tmp);
}
void runFlowG(int idx, struct ArgxNest *nst)
{
	int dly = memxInt(nst[idx].arg);
	int msk = memxMsk(nst[faces].arg);
	if (dly == 0) waitMsk(msk);
	else pauseMsk(dly,msk);
}
void runFlow(int idx, struct ArgxNest *nst)
{
	switch (flow[nst[idx].idx]) {
	case ('a'): memxCopy(&nst[idx].arg,memxRun(nst[idx].arg)); break;
	case ('b'): memxCopy(&nst[idx].arg,memxRun(nst[idx].arg)); break;
	case ('c'): runFlowF(idx,nst); break;
	case ('d'): runFlowG(idx,nst); break;
	case ('e'): memxCopy(&nst[idx].arg,nst[idx].arg); break;
	case ('f'): memxCopy(&nst[idx].arg,nst[idx].arg); break;
	case ('g'): memxCopy(&nst[idx].arg,nst[idx].arg); break;
	case ('h'): memxCopy(&nst[idx].arg,nst[idx].arg); break;
	default: break;}
	
}
void useJump(int idx, struct ArgxNest *nst)
{
	// TODO initialize nst[idx].arg
}
void *runJump(int idx, struct ArgxNest *nst)
{
	return memxRun(nst[idx].arg);
}
void useNest(int idx, struct ArgxNest *nst)
{
	// TODO initialize nst[idx].arg
}
int runNest(int idx, struct ArgxNest *nst)
{
	return memxInt(nst[idx].arg);
}
int main(int argc, char **argv)
{
	addFlow(flow,runFlow,useFlow);
	addJump("j",runJump,useJump);
	addNest("k",runNest,useNest);
	face = useLocation("efghlpq",memxCopy);
	faces = useLocation("efghlpq",memxKeep);
	type = useLocation("a",memxCopy);
	field = useLocation("b",memxCopy);
	for (int i = 1; i < argc; i++) useArgument(argv[i]);
	runProgram();
	return 0;
}
