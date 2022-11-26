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
int zero = 0;

void shareRunC(void **run, void *use)
{
	int tmp, opc;
	struct File file; // TODO use generic generated function
	tmp = memxInt(argxGet(iface)->run);
	opc = memxInt(use);
	// TODO write from misc
	readFile(&file,tmp); // TODO read to run instead
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
	zero = getLocation();
	// TODO add global locations to lua interpreter
	addOption("a",protoTypeN(memxInit),protoTypeM(memxCopy));
	addOption("b",protoTypeN(memxInit),protoTypeM(memxCopy));
	addOption("c",protoTypeN(memxInit),protoTypeM(shareRunC));
	addOption("d",protoTypeN(memxInit),protoTypeO(shareRunD));
	addOption("e",protoTypeF(forkExec),protoTypeM(memxCopy));
	addOption("f",protoTypeF(openFile),protoTypeM(memxCopy));
	addOption("g",protoTypeF(openFifo),protoTypeM(memxCopy));
	addOption("h",protoTypeG(openInet),protoTypeM(memxCopy));
	addOption("i",protoTypeN(memxInit),protoTypeM(argxCopy));
	addJump("j",protoTypeN(memxInit),protoTypeO(argxJump));
	addNest("k",protoTypeN(memxInit),protoTypeM(memxCopy));
	addOption("l",protoTypeN(shareUseL),protoTypeM(shareRunL));
	addOption("m",protoTypeN(memxInit),protoTypeM(argxKeep));
	addOption("n",protoTypeN(memxInit),protoTypeM(argxCopy));
	addOption("o",protoTypeN(memxInit),protoTypeM(argxCopy));
	addOption("p",protoTypeF(shareUseP),protoTypeM(memxCopy));
	addOption("q",protoTypeF(shareUseQ),protoTypeM(memxCopy));
	mapCallback("a",type,protoTypeM(memxCopy));
	mapCallback("b",field,protoTypeM(memxCopy));
	mapCallback("c",misc,protoTypeM(memxList));
	mapCallback("diefghlq",iface,protoTypeM(memxCopy));
	mapCallback("efghlop",oface,protoTypeM(memxCopy));
	mapCallback("efghlpq",faces,protoTypeM(memxKeep));
	mapCallback("mn",faces,protoTypeM(memxCopy));
	mapDefault("i",iface,protoTypeM(memxCopy));
	mapDefault("o",oface,protoTypeM(memxCopy));
	mapDefault("m",zero,protoTypeM(memxCopy));
	mapDefault("n",faces,protoTypeM(memxCopy));
	for (int i = 1; i < argc; i++) useArgument(argv[i]);
	runProgram();
	return 0;
}
