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
void shareRunCG(void **run)
{
	void *que = argxRun(misc);
	memxSkip(run,que,0);
	memxDel(&que,0);
}
void shareRunCH(void **run)
{
	void *que = argxRun(misc);
	memxSkip(run,que,memxSize(que)-1);
	memxDel(&que,memxSize(que)-1);
}
void shareRunCI(void **run)
{
	void *que = argxRun(misc);
	memxAdd(&que,*run,0);
}
void shareRunCJ(void **run)
{
	void *que = argxRun(misc);
	memxAdd(&que,*run,memxSize(que));
}
void shareRunC(void **run, void *use)
{
	int len = memxSize(use);
	int rfd = memxOpen(run);
	int ifd = memxInt(argxRun(iface));
	int ofd = memxInt(argxRun(oface));
	int typ = memxInt(argxRun(type));
	void *mem = memxTemp(0);
	void *tmp = memxTemp(1);
	int tfd = memxOpen(tmp);
	for (memxFirst(&mem,use); memxTest(mem); memxNext(&mem,mem))
	switch ((enum Stream) memxInt(mem)) {
	// read from type to type
	case (RdTypP): loopStruct(typ,ifd,rfd); break;
	case (RdTypHd): shareRunCG(run); break;
	case (RdTypTl): shareRunCH(run); break;
	// read from raw to string
	case (RdStrP): readStruct(shareRunCF,*run,typ,ifd); break;
	case (RdStrHd): shareRunCG(run); readStruct(shareRunCF,*run,typ,rfd); break;
	case (RdStrTl): shareRunCH(run); readStruct(shareRunCF,*run,typ,rfd); break;
	// read to raw from string
	case (RdRawP): loopStruct(typ,ifd,rfd); writeStruct(memxStr(*run),typ,rfd); break;
	case (RdRawHd): shareRunCG(run); writeStruct(memxStr(*run),typ,rfd); break;
	case (RdRawTl): shareRunCH(run); writeStruct(memxStr(*run),typ,rfd); break;
	// write from type to type
	case (WrTypP): loopStruct(typ,rfd,ofd); break;
	case (WrTypHd): shareRunCI(run); break;
	case (WrTypTl): shareRunCJ(run); break;
	// write to raw from string
	case (WrStrP): writeStruct(memxStr(*run),typ,ofd); break;
	case (WrStrHd): writeStruct(memxStr(*run),typ,tfd); shareRunCI(tmp); break;
	case (WrStrTl): writeStruct(memxStr(*run),typ,tfd); shareRunCJ(tmp); break;
	// write from raw to string
	case (WrRawP): readStruct(shareRunCF,tmp,typ,rfd); writeStr(memxStr(tmp),1,ofd); break;
	case (WrRawHd): readStruct(shareRunCF,tmp,typ,rfd); shareRunCI(tmp); break;
	case (WrRawTl): readStruct(shareRunCF,tmp,typ,rfd); shareRunCJ(tmp); break;
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
	void *mem = 0;
	memxSkip(&mem,argxUse(idx),1);
	if (luaxCall(memxStr(mem),protoCloseP(idx,nbyte)) < 0) ERROR(exitErr,0);
	return protoResultP(buf);
}
int shareUseLG(int idx, const void *buf, int nbyte)
{
	void *mem = 0;
	memxSkip(&mem,argxUse(idx),2);
	if (luaxCall(memxStr(mem),protoCloseQ(idx,buf,nbyte)) < 0) ERROR(exitErr,0);
	return protoResultQ();
}
void shareUseL(void **use, const char *str)
{
	memxForm(use,":%d;%s",puntInit(argxHere(),argxHere(),shareUseLF,shareUseLG),str);
}
void shareRunL(void **run, void *use)
{
	memxSkip(run,use,0);
}
int shareUseP(const char *str)
{
	void *mem = memxTemp(0); memxInit(&mem,str); return rdfdInit(memxInt(mem),memxInt(argxUse(oface)));
}
int shareUseQ(const char *str)
{
	void *mem = memxTemp(0); memxInit(&mem,str); return wrfdInit(memxInt(mem),memxInt(argxUse(iface)));
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
	luaxAdd("shareLuax",protoTypeF(shareLuax));
	luaxAdd("argxUse",protoTypeT(argxUse));
	luaxAdd("argxRun",protoTypeT(argxRun));
	memxLuax();
	faces = getLocation();
	type = getLocation();
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
	mapCallback("diefghlp",iface,protoTypeM(memxCopy));
	mapCallback("efghloq",oface,protoTypeM(memxCopy));
	mapCallback("efghlpq",faces,protoTypeM(memxKeep));
	mapCallback("mn",faces,protoTypeM(memxCopy));
	mapDefault("i",iface,protoTypeM(memxCopy));
	mapDefault("o",oface,protoTypeM(memxCopy));
	mapDefault("m",zero,protoTypeM(memxCopy));
	mapDefault("n",faces,protoTypeM(memxCopy));
	mapContext("p",iface,protoTypeM(memxCopy));
	mapContext("q",oface,protoTypeM(memxCopy));
	for (int i = 1; i < argc; i++) useArgument(argv[i]);
	runProgram();
	return 0;
}
