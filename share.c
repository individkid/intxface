#include "argx.h"
#include "memx.h"
#include "face.h"
#include "type.h"
#include <string.h>

int face = 0;
int acum = 0;
int hist = 0;
const char *flow = "abcdefghilmnopq";

void shareFlow(int idx, struct ArgxNest *nst)
{
	switch (flow[nst[idx].idx]) {
	case ('a'): if (!nst[idx].arg) nst[idx].arg = memxInit(nst[idx].str); break;
	default: break;}
	{struct File file; readFile(&file,0);} // TODO use generic generated functions
}
void *shareJump(int idx, struct ArgxNest *nst)
{
	// TODO standard flow control
	return 0;
}
int shareNest(int idx, struct ArgxNest *nst)
{
	// TODO standard flow control
	return 0;
}
int main(int argc, char **argv)
{
	addFlow(flow,shareFlow);
	addJump("j",shareJump);
	addNest("k",shareNest);
	for (int i = 1; i < argc; i++) useArgument(argv[i]);
	face = useNoop(); // face idx
	acum = useAcum(face); // accumulated face idx
	hist = useHist(acum); // history of accumulated face idx
	runProgram();
	helloOkAgain();
	debugStr("hello ok again");
	return 0;
}
