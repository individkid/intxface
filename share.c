#include "argx.h"
#include "memx.h"
#include "face.h"
#include "type.h"
#include <string.h>

int face = 0;
const char *flow = "abcdefghilmnopq";

void useFlowF(int *glb, int idx, struct ArgxNest *nst)
{
	if (!nst[idx].arg) {
		
		*glb = idx;}
}
void useFlow(int idx, struct ArgxNest *nst)
{
	switch (flow[nst[idx].idx]) {
	case ('a'): memxInit(&nst[idx].arg,nst[idx].str); break;
	case ('b'): memxInit(&nst[idx].arg,nst[idx].str); break;
	// TODO
	default: break;}
}
void runFlow(int idx, struct ArgxNest *nst)
{
	struct File file; readFile(&file,0); // TODO use generic generated functions
}
void useJump(int idx, struct ArgxNest *nst)
{
	// TODO initialize nst[idx].arg
}
void *runJump(int idx, struct ArgxNest *nst)
{
	return 0; // TODO run script or return nst[idx].arg
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
	face = useLocation("",memxKeep); // TODO identify dashes that get idx from face.h
	// TODO call useLocation for type, field, input, output
	for (int i = 1; i < argc; i++) useArgument(argv[i]);
	runProgram();
	return 0;
}
