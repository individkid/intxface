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
void *fdm[3] = {0};
int mfd[3] = {0};

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
	const void *dat = memxDat(fdm[fildes]);
	int len = *(int*)dat;
	void *rem = 0;
	if (len > nbyte) {
	rem = malloc(len-nbyte+sizeof(int));
	*(int*)rem = len-nbyte;
	memcpy((void*)(((int*)rem)+1),(void*)(((char*)(((int*)dat)+1))+nbyte),len-nbyte);
	len = nbyte;}
	memcpy(buf,(void*)(((int*)dat)+1),len);
	if (rem) memxData(&fdm[fildes],rem);
	return len;
}
int shareWriteF(int fildes, const void *buf, int nbyte)
{
	int len = *(int*)memxDat(fdm[fildes]);
	void *dat = malloc(len+nbyte+sizeof(int));
	*(int*)dat = len+nbyte;
	memcpy((void*)(((char*)(((int*)dat)+1))+len),buf,len+nbyte);
	memxData(&fdm[fildes],dat);
	free(dat);
	return nbyte;
}
void shareRead(void **mem, int typ, int ifd)
{
	loopStruct(typ,ifd,mfd[0]);
	flushBuf(mfd[0]);
	memxCopy(mem,fdm[0]);
	memxDone(&fdm[0]);
}
void shareWrite(void *mem, int typ, int ofd)
{
	memxCopy(&fdm[0],mem);
	loopStruct(typ,mfd[0],ofd);
	memxDone(&fdm[0]);
}
void shareDecode(void *mem, void *giv, int typ)
{
	char *str;
	memxCopy(&fdm[0],giv);
	allocMark();
	readStruct(callStr,&str,typ,mfd[0]);
	if (mem == 0) ERROR();
	memxConst(&mem,MemxStr,str);
	allocDrop();
	memxDone(&fdm[0]);
}
void shareEncode(void *mem, void *giv, int typ)
{
	writeStruct(memxStr(giv),typ,mfd[0]);
	if (mem == 0) ERROR();
	memxCopy(&mem,fdm[0]);
	memxDone(&fdm[0]);
}
void shareInsert(void *mem, void *giv, void *fld, int sid, int fid, int idx)
{
	memxCopy(&fdm[0],giv);
	memxCopy(&fdm[1],fld);
	readField(sid,fid,idx,mfd[0],mfd[1],mfd[2]);
	if (mem == 0) ERROR();
	memxCopy(&mem,fdm[2]);
	for (int i = 0; i < 3; i++) memxDone(&fdm[i]); 
}
void shareExtract(void **mem, void *giv, int sid, int fid, int idx)
{
	memxCopy(&fdm[0],giv);
	writeField(sid,fid,idx,mfd[0],mfd[1]);
	memxCopy(mem,fdm[1]);
	for (int i = 0; i < 2; i++) memxDone(&fdm[i]); 
}
void *sharePop(void *src, int idx, int mod)
{
	int sub = 0;
	int min = 0;
	void *nxt = 0;
	void *mem = 0;
	min = (mod-memxSize(src)%mod)%mod;
	sub = idx - min;
	if (sub < 0) ERROR();
	nxt = memxSkip(src,sub);
	mem = memxTemp(1+idx);
	memxCopy(&mem,nxt);
	memxConst(&nxt,MemxNul,"");
	while (memxNul(memxSkip(src,0))) {nxt = memxSkip(src,0); memxDel(&src,0); memxDone(&nxt);}
	return mem;
}
void *sharePush(void **dst)
{
	void *mem = 0;
	memxConst(&mem,MemxNul,"");
	memxAdd(dst,mem,memxSize(*dst));
	return mem;
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
			while (memxSize(src) > 0) shareEncode(sharePush(&dst),sharePop(src,0,1),typ);
			val = 1; break;
		case (Decode):
			if (memxSize(src) == 0) {val = -1; break;}
			if (typ != memxInt(memxSkip(arg,1))) ERROR();
			while (memxSize(src) > 0) shareDecode(sharePush(&dst),sharePop(src,0,1),typ);
			typ = -1; val = 1; break;
		case (Insert): {
			int fld = memxInt(memxSkip(arg,2));
			int idx = memxInt(memxSkip(arg,3));
			if (memxSize(src) < 2 || memxSize(src)%2 != 0) {val = -1; break;}
			typ = memxInt(memxSkip(arg,1));
			while (memxSize(src) > 0) shareInsert(sharePush(&dst),sharePop(src,0,2),sharePop(src,1,2),typ,fld,idx);
			val = 1; break;}
		case (Extract): {
			int fld = memxInt(memxSkip(arg,2));
			int idx = memxInt(memxSkip(arg,3));
			if (memxSize(src) == 0) {val = -1; break;}
			if (typ != memxInt(memxSkip(arg,1))) ERROR();
			while (memxSize(src) > 0) shareExtract(sharePush(&dst),sharePop(src,0,1),typ,fld,idx);
			// TODO typ = identType(typ,fld);
			val = 1; break;}
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
				shareRead(&mem,typ,ifd);}
			memxAdd(&src,mem,memxSize(src));
			val = 0;}
		if (val > 0) src = dst; // typ is now type of src
		if (val < 0) src = memxTemp(1);
		idx += val;}
	while (memxSize(src) > 0) {
		void *nxt = memxSkip(src,0);
		memxDel(&src,0);
		if (typ < 0) writeStr(memxStr(nxt),1,ofd);
		else shareWrite(nxt,typ,ofd);
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
	for (int i = 0; i < 3; i++) mfd[i] = buffInit(i,i,shareReadF,shareWriteF);
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
