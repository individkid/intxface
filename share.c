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

void shareRunA(void **run, void *use)
{
	memxInit(run,memxStr(use));
	nestScan();
}
void shareRunCF(const char *str, int trm, int idx, void *arg)
{
	void **mem = (void**)arg;
	memxInit(mem,str);
}
void shareRunCG(void **run)
{
	void *que = argxRun(misc);
	memxCopy(run,memxSkip(que,0));
	memxDel(&que,0);
}
void shareRunCH(void **run)
{
	void *que = argxRun(misc);
	memxCopy(run,memxSkip(que,memxSize(que)-1));
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
	int typ = memxInt(memxSkip(argxRun(type),0));
	int fld = memxInt(memxSkip(argxRun(type),1));
	int idx = memxInt(memxSkip(argxRun(type),2));
	void *tmp = memxTemp(0);
	int tfd = memxOpen(tmp);
	for (int i = 0; i < memxSize(use); i++) {
	void *mem = memxSkip(use,i);
	enum Stream fst = memxInt(memxSkip(mem,0));
	switch (fst) {
	case (Encode): break;
	case (Decode): break;
	case (Insert): break;
	case (Extract): break;
	case (Unique): break;
	case (Combine): break;
	case (Permute): break;
	case (Constant): break;
	case (Repeat): break;
	case (Delete): break;
	case (Follow): break;
	default: ERROR();}}
}
void shareRunD(void **run, void *use)
{
	int msk, dly, val;
	msk = memxMask(argxRun(faces));
	dly = memxInt(memxSkip(use,0));
	if (dly == 0 && memxSize(memxSkip(use,1)) == 0) val = waitRead(0.0,msk);
	else if (dly == 0) val = waitRead(-1.0,msk);
	else if (memxSize(memxSkip(use,1)) == 0) val = waitRead(dly,0);
	else val = waitRead(dly,msk);
	if (val < 0) memxForm(run,"(%d)(%d)",val,argxJump(memxSkip(use,1)));
	else memxForm(run,"(%d)(%d)",val,0);
}
void shareCopy(void **run, void *use)
{
	memxCopy(run,memxSkip(use,1));
}
int shareUseLF(int idx, void *buf, int nbyte)
{
	void *mem = memxSkip(argxUse(idx),1);
	if (luaxCall(memxStr(mem),protoClosePf(idx,nbyte)) < 0) ERROR();
	return protoResultPf(buf);
}
int shareUseLG(int idx, const void *buf, int nbyte)
{
	void *mem = memxSkip(argxUse(idx),2);
	if (luaxCall(memxStr(mem),protoCloseQf(idx,buf,nbyte)) < 0) ERROR();
	return protoResultQf();
}
void shareUseL(void **use, const char *str)
{
	void *tmp0 = 0; void *tmp1 = 0;
	memxInit(use,"");
	memxForm(&tmp0,"%d",puntInit(argxHere(),argxHere(),shareUseLF,shareUseLG)); memxInit(&tmp1,str);
	memxList(use,tmp0); memxList(use,tmp1);
	memxMake(use,tmp0); memxMake(use,tmp1);}
void shareRunL(void **run, void *use)
{
	memxCopy(run,memxSkip(use,0));
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
	ERROR();
	return 0;
}
int main(int argc, char **argv)
{
	if (luaxFile("type.lua") < 0) {protoErr("shareC: cannot load library: type.lua\n"); fprintf(stderr,"%s",protoMsg()); return -1;}
	luaxAdd("shareLuax",protoTypeFf(shareLuax));
	luaxAdd("argxUse",protoTypeTf(argxUse));
	luaxAdd("argxRun",protoTypeTf(argxRun));
	memxLuax();
	faces = getLocation();
	type = getLocation();
	iface = getLocation();
	oface = getLocation();
	misc = getLocation();
	zero = getLocation();
	addFlow("a",protoTypeNf(memxInit),protoTypeMf(shareRunA));
	addFlow("b",protoTypeNf(memxInit),protoTypeMf(memxCopy));
	addFlow("c",protoTypeNf(memxInit),protoTypeMf(shareRunC));
	addSide("d",protoTypeNf(memxInit),protoTypeMf(shareRunD));
	addFlow("e",protoTypeFf(forkExec),protoTypeMf(memxCopy));
	addFlow("f",protoTypeFf(openFile),protoTypeMf(memxCopy));
	addFlow("g",protoTypeFf(openFifo),protoTypeMf(memxCopy));
	addFlow("h",protoTypeGf(openInet),protoTypeMf(memxCopy));
	addFlow("i",protoTypeNf(memxInit),protoTypeMf(argxCopy));
	addJump("j",protoTypeNf(memxInit),protoTypeOf(argxJump));
	addNest("k",protoTypeNf(memxInit),protoTypeMf(memxCopy));
	addFlow("l",protoTypeNf(shareUseL),protoTypeMf(shareRunL));
	addFlow("m",protoTypeNf(memxInit),protoTypeMf(argxKeep));
	addFlow("n",protoTypeNf(memxInit),protoTypeMf(argxCopy));
	addFlow("o",protoTypeNf(memxInit),protoTypeMf(argxCopy));
	addFlow("p",protoTypeFf(shareUseP),protoTypeMf(memxCopy));
	addFlow("q",protoTypeFf(shareUseQ),protoTypeMf(memxCopy));
	mapCallback("b",type,protoTypeMf(memxCopy));
	mapCallback("d",iface,protoTypeMf(shareCopy));
	mapCallback("efghilp",iface,protoTypeMf(memxCopy));
	mapCallback("efghloq",oface,protoTypeMf(memxCopy));
	mapCallback("efghlpq",faces,protoTypeMf(memxList));
	mapCallback("mn",faces,protoTypeMf(memxCopy));
	mapDefault("i",iface,protoTypeMf(memxCopy));
	mapDefault("o",oface,protoTypeMf(memxCopy));
	mapDefault("m",zero,protoTypeMf(memxCopy));
	mapDefault("n",faces,protoTypeMf(memxCopy));
	mapContext("p",iface,protoTypeMf(memxCopy));
	mapContext("q",oface,protoTypeMf(memxCopy));
	for (int i = 1; i < argc; i++) useArgument(argv[i]);
	runProgram();
	return 0;
}
