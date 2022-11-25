#include "argx.h"
#include "memx.h"
#include "face.h"
#include "type.h"
#include <string.h>

int faces = 0;
int type = 0;
int field = 0;
int iface = 0;
int oface = 0;
int misc = 0;

void shareRunC(void **run, void *use)
{
	int tmp, typ;
	struct File file; // TODO use generic generated function
	tmp = memxInt(argxGet(iface)->run);
	typ = memxInt(use);
	readFile(&file,tmp); // TODO read to run instead
	// TODO write from misc
}
int shareRunD(void *use)
{
	int msk, dly, val;
	msk = memxMask(argxGet(faces)->run);
	dly = memxInt(use);
	if (dly == 0) val = waitMask(msk);
	else val = pauseMask(dly,msk);
	return val;
}
int shareUseLF(int idx, void *buf, int nbyte)
{
	// TODO read buf frim script at argxGet(idx)->run[1]
	return 0; // TODO number of bytes read
}
int shareUseLG(int idx, const void *buf, int nbyte)
{
	// TODO write buf to script at argxGet(idx)->run[2]
	return 0; // TODO number of bytes written
}
void shareUseL(void **use, const char *str)
{
	// TODO split str into read and write scripts
	int idx = puntInit(argxHere(),argxHere(),shareUseLF,shareUseLG);
	// TODO init use as tuple of idx, read script, write script
}
void shareRunL(void **run, void *use)
{
	memxCopy(run,memxSkip(use,0));
}
int shareUseP(const char *str)
{
	return rdfdInit(memxInt(memxTemp(str,0)),memxInt(argxGet(oface)->run));
}
int shareUseQ(const char *str)
{
	return wrfdInit(memxInt(memxTemp(str,0)),memxInt(argxGet(iface)->run));
}
int main(int argc, char **argv)
{
	faces = getLocation();
	type = getLocation();
	field = getLocation();
	iface = getLocation();
	oface = getLocation();
	misc = getLocation();
	// TODO add global memx to lua interpreter
	addOption("a",FlowTag,protoTypeN(memxInit),protoTypeM(memxCopy));
	addOption("b",FlowTag,protoTypeN(memxInit),protoTypeM(memxCopy));
	addOption("c",FlowTag,protoTypeN(memxInit),protoTypeM(shareRunC));
	addOption("d",FlowTag,protoTypeN(memxInit),protoTypeO(shareRunD));
	addOption("e",FlowTag,protoTypeF(forkExec),protoTypeM(memxCopy));
	addOption("f",FlowTag,protoTypeF(openFile),protoTypeM(memxCopy));
	addOption("g",FlowTag,protoTypeF(openFifo),protoTypeM(memxCopy));
	addOption("h",FlowTag,protoTypeG(openInet),protoTypeM(memxCopy));
	addOption("i",FlowTag,protoTypeN(memxInit),protoTypeM(argxCopy));
	addOption("j",JumpTag,protoTypeN(memxInit),protoTypeO(argxJump));
	addOption("k",NestTag,protoTypeN(memxInit),protoTypeM(memxCopy));
	addOption("l",FlowTag,protoTypeN(shareUseL),protoTypeM(shareRunL));
	addOption("m",FlowTag,protoTypeN(memxInit),protoTypeM(argxKeep));
	addOption("n",FlowTag,protoTypeN(memxInit),protoTypeM(argxCopy));
	addOption("o",FlowTag,protoTypeN(memxInit),protoTypeM(argxCopy));
	addOption("p",FlowTag,protoTypeF(shareUseP),protoTypeM(memxCopy));
	addOption("q",FlowTag,protoTypeF(shareUseQ),protoTypeM(memxCopy));
	mapCallback("a",type,protoTypeM(memxCopy));
	mapCallback("b",field,protoTypeM(memxCopy));
	mapCallback("c",misc,protoTypeM(memxList));
	mapCallback("diefghlq",iface,protoTypeM(memxCopy));
	mapCallback("efghlop",oface,protoTypeM(memxCopy));
	mapCallback("efghlpq",faces,protoTypeM(memxKeep));
	mapCallback("mn",faces,protoTypeM(memxCopy));
	for (int i = 1; i < argc; i++) useArgument(argv[i]);
	runProgram();
	return 0;
}
