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

void useFlow(int idx, struct ArgxNest *nst)
{
	switch (flow[nst[idx].idx]) {
	case ('a'): memxInit(&nst[idx].arg,nst[idx].str); break;
	case ('b'): memxInit(&nst[idx].arg,nst[idx].str); break;
	case ('c'): memxInit(&nst[idx].arg,nst[idx].str); break; 
	case ('d'): memxInit(&nst[idx].arg,nst[idx].str); break; 
	default: break;}
}
void runFlow(int idx, struct ArgxNest *nst)
{
	switch (flow[nst[idx].idx]) {
	case ('a'): memxCopy(&nst[idx].arg,memxRun(nst[idx].arg)); break;
	case ('b'): memxCopy(&nst[idx].arg,memxRun(nst[idx].arg)); break;
	case ('c'): {
		int idx = memxInt(nst[face].arg);
		struct File file;
		readFile(&file,idx);
		break;} // TODO use generic generated function
	case ('d'): {
		int dly = memxInt(nst[idx].arg);
		int msk = memxMsk(nst[faces].arg);
		if (dly == 0) waitMsk(msk);
		else pauseMsk(dly,msk);
		break;}
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
