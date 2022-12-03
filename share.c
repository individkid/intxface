#include "argx.h"
#include "memx.h"
#include "luax.h"
#include "face.h"
#include "type.h"
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/errno.h>
#include <stdlib.h>
#include <libgen.h>

int faces = 0;
int type = 0;
int field = 0;
int iface = 0;
int oface = 0;
int misc = 0;
int zero = 0;

void shareRunCF(const char *str, int trm, int idx, void *arg)
{
	memxInit(&arg,str);
}
void shareRunC(void **run, void *use)
{
	int len = memxSize(use);
	void *acc = 0; memxSkip(&acc,run,0);
	void *aux = 0; memxSkip(&aux,run,1);
	int bcc = memxOpen(&acc);
	int bux = memxOpen(&aux);
	int ifd = memxInt(argxRun(iface));
	int ofd = memxInt(argxRun(oface));
	void *que = argxRun(misc);
	int typ = memxInt(argxRun(type));
	int fld = memxInt(argxRun(field));
	void *mem = memxTemp(0);
	void *tmp = memxTemp(1);
	for (memxFirst(&mem,use); memxTest(mem); memxNext(&mem,mem))
	switch ((enum Stream) memxInt(mem)) {
	case (RdTypP): loopStruct(typ,ifd,bcc); break;
	case (RdTypHd): memxSkip(&acc,que,0); memxDel(&que,0); break;
	case (RdTypTl): memxSkip(&acc,que,memxSize(que)-1); memxDel(&que,memxSize(que)-1); break;
	case (RdTypAux): loopStruct(typ,bux,bcc); break;
	case (RdFldP): break;
	case (RdFldHd): break;
	case (RdFldTl): break;
	case (RdFldAux): break;
	case (RdStrP): readStruct(shareRunCF,acc,typ,ifd); break; // read from raw to string
	case (RdStrHd): memxSkip(&acc,que,0); memxDel(&que,0); readStruct(shareRunCF,acc,typ,bcc); break;
	case (RdStrTl): memxSkip(&acc,que,memxSize(que)-1); memxDel(&que,memxSize(que)-1); readStruct(shareRunCF,acc,typ,bcc); break;
	case (RdStrAux): readStruct(shareRunCF,acc,typ,bux); break;
	// TODO case (RdRawP): break; // read to raw from string
	case (WrTypP): loopStruct(typ,bcc,ofd); break;
	case (WrTypHd): memxAdd(&acc,que,0); break;
	case (WrTypTl): memxAdd(&acc,que,memxSize(que)); break;
	case (WrTypAux): loopStruct(typ,bcc,bux); break;
	case (WrFldP): break;
	case (WrFldHd): break;
	case (WrFldTl): break;
	case (WrFldAux): break;
	case (WrStrP): writeStruct(memxStr(acc),typ,ofd); break; // write to raw from string
	case (WrStrHd): memxCopy(tmp,acc); writeStruct(memxStr(acc),typ,bcc); memxAdd(&acc,que,0); memxCopy(acc,tmp); break;
	case (WrStrTl): memxCopy(tmp,acc); writeStruct(memxStr(acc),typ,bcc); memxAdd(&acc,que,memxSize(que)); memxCopy(acc,tmp); break;
	case (WrStrAux): writeStruct(memxStr(acc),typ,bux); break;
	// TODO case (WrRawP): break; // write from raw to string
	default: ERROR(exitErr,0); break;}
}
int shareRunD(void *use)
{
	int msk, dly, val;
	msk = memxMask(argxRun(faces));
	dly = memxInt(use);
	if (dly == 0) val = waitRead(0.0,msk);
	else val = waitRead(dly,msk);
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
	void *mem = memxTemp(0); memxSkip(&mem,use,0); memxCopy(run,mem);
}
int shareUseP(const char *str)
{
	void *mem = memxTemp(0); memxInit(&mem,str); return rdfdInit(memxInt(mem),memxInt(argxRun(oface)));
}
int shareUseQ(const char *str)
{
	void *mem = memxTemp(0); memxInit(&mem,str); return wrfdInit(memxInt(mem),memxInt(argxRun(iface)));
}
int shareLuax(const char *str)
{
	if (strcmp(str,"faces") == 0) return faces;
	else if (strcmp(str,"type") == 0) return type;
	else if (strcmp(str,"field") == 0) return field;
	else if (strcmp(str,"iface") == 0) return iface;
	else if (strcmp(str,"oface") == 0) return oface;
	else if (strcmp(str,"misc") == 0) return misc;
	else if (strcmp(str,"zero") == 0) return zero;
	ERROR(exitErr,0);
	return 0;
}
int main(int argc, char **argv)
{
	if (luaxFile("type.lua") < 0) {protoErr("shareC: cannot load library: type.lua\n"); fprintf(stderr,"%s",protoMsg()); return -1;}
	luaxFunc("shareLuax",protoTypeF(shareLuax));
	luaxFunc("argxUse",protoTypeT(argxUse));
	luaxFunc("argxRun",protoTypeT(argxRun));
	memxLuax();
	faces = getLocation();
	type = getLocation();
	field = getLocation();
	iface = getLocation();
	oface = getLocation();
	misc = getLocation();
	zero = getLocation();
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
