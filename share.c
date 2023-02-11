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
int field = 0;
int iface = 0;
int oface = 0;
int misc = 0;
int zero = 0;
void *fdm = 0;
int mfd = 0;
int bfd = 0;

void shareRunA(void **run, void *use)
{
	memxInit(run,memxStr(use));
}
void shareRunB(void **run, void *use)
{
	memxInit(run,memxStr(use));
	nestScan();
}
int shareReadF(int fildes, void *buf, int nbyte)
{
	return 0;
}
int shareReadG(int fildes, const void *buf, int nbyte)
{
	void *dat = malloc(nbyte+sizeof(int));
	*(int*)dat = nbyte;
	memcpy((void*)(((int*)dat)+1),buf,nbyte);
	memxData(&fdm,dat);
	free(dat);
	return nbyte;
}
void shareRead(void **dat, int typ, int ifd)
{
	loopStruct(typ,ifd,bfd);
	flushBuf(bfd);
	memxCopy(dat,fdm);
}
void shareWrite(const void *dat, int typ, int ofd)
{
	memxData(&fdm,dat);
	loopStruct(typ,mfd,ofd);
}
void shareDecode(void **mem, void *giv, int typ)
{
	char *str;
	memxCopy(&fdm,giv);
	allocMark();
	readStruct(callStr,&str,typ,mfd);
	memxConst(mem,MemxStr,str);
	allocDrop();	
}
void shareEncode(void **mem, void *giv, int typ)
{
	writeStruct(memxStr(giv),typ,mfd);
	memxCopy(mem,fdm);
}
void shareCode(void **dst, void *src, int typ, igtype fnc)
{
	while (memxSize(src) > 0) {
	void *mem = 0;
	void *nxt = memxSkip(src,0);
	memxDel(&src,0);
	fnc(&mem,nxt,typ);
	memxDone(&nxt);
	memxAdd(dst,mem,memxSize(*dst));}
}
void shareRunC(void **run, void *use)
{
	int len = memxSize(use);
	int ifd = memxInt(argxRun(iface));
	int ofd = memxInt(argxRun(oface));
	int idx = len-1;
	int typ = -1;
	void *src = memxTemp(0);
	while (idx < len) {
		int val = 0;
		void *arg = memxSkip(use,idx);
		enum Stream tag = memxInt(memxSkip(arg,0));
		void *dst = memxSkip(*run,idx); // typ is still type of src
		switch (tag) {
		case (Encode):
			if (memxSize(src) == 0) {val = -1; break;}
			if (typ != -1) ERROR();
			typ = memxInt(memxSkip(arg,1));
			shareCode(&dst,src,typ,shareEncode);
			val = 1; break;
		case (Decode):
			if (memxSize(src) == 0) {val = -1; break;}
			if (typ != memxInt(memxSkip(arg,1))) ERROR();
			typ = -1;
			shareCode(&dst,src,typ,shareDecode);
			val = 1; break;
		case (Insert): break; // TODO with writeField use xfd/fdx nfd/fdn mfd/fdm initialized in main
		case (Extract): break; // TODO with readField use xfd/fdx nfd/fdn mfd/fdm initialized in main
		case (Unique): break;
		case (Permute): break;
		case (Constant): break;
		case (Repeat): break;
		case (Delete): break;
		case (Follow): break;
		default: ERROR();} // typ is now typ of dst
		if (idx == 0 && val > 0 && memxSize(src) != 0) ERROR();
		if (idx == 0 && val < 0) {
			void *mem = 0;
			if (typ < 0) {
				char *str = 0;
				readStr(callStr,&str,ifd);
				memxConst(&mem,MemxStr,str);}
			else {
				void *dat = 0;
				shareRead(&dat,typ,ifd);
				memxData(&mem,dat);}
			memxAdd(&src,mem,memxSize(src));
			val = 0;}
		if (val > 0) src = dst; // typ is now type of src
		if (val < 0) src = memxTemp(1);
		idx += val;}
	while (memxSize(src) > 0) {
		void *nxt = memxSkip(src,0);
		memxDel(&src,0);
		if (typ < 0) writeStr(memxStr(nxt),1,ofd);
		else shareWrite(memxDat(nxt),typ,ofd);
		memxDone(&nxt);}
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
	iface = getLocation();
	oface = getLocation();
	misc = getLocation();
	zero = getLocation();
	mfd = memxOpen(&fdm);
	bfd = buffInit(0,0,shareReadF,shareReadG);
	addFlow("a",protoTypeNf(memxInit),protoTypeMf(shareRunA));
	addFlow("b",protoTypeNf(memxInit),protoTypeMf(shareRunB));
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
