#include "face.h"
#include "type.h"
#include "datx.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <stdint.h>

int idx0 = 0;
int idx1 = 0;
void *dat0 = 0;
void *dat1 = 0;
int note = 0;
struct Wrap {
	enum Stream tag;
	int nxt; // next if this is on wake list
	int sub; // dst subscript
	int siz; // dst size
	struct Wrap **dst; // where to write for tag==Fanout,Combine
	int idx; // tag==Fanout,Buffer reads from here
	int inp; // type to read from here
	int out; // type to write to here
	struct Express *exp; // for tag==Combine
	const char *str; // for tag==Buffer
} *wrap = 0; // per argv that is a Stage
int args = 0;
int **back = 0; // per value lists to add to wake
int *refs = 0; // per value list length
int vals = 0; // number of values in expressions
int wake = 0; // first to process before waiting for readable pipe

int shareReadFp(int fildes, void *buf, int nbyte)
{
	void **dat = (fildes == idx0 ? &dat0 : &dat1);
	void *pre = 0;
	void *suf = 0;
	datxSplit(&pre,&suf,*dat,nbyte);
	if (*(int*)pre != nbyte) return 0;
	memcpy(buf,datxData(pre),nbyte);
	assignDat(dat,suf);
	free(pre);
	free(suf);
	return nbyte;
}
int shareWriteFp(int fildes, const void *buf, int nbyte)
{
	void **dat = (fildes == idx0 ? &dat0 : &dat1);
	void *suf = malloc(sizeof(int)+nbyte);
	void *pre = 0;
	assignDat(&pre,*dat);
	datxJoin(dat,pre,suf);
	free(pre);
	free(suf);
	return nbyte;
}
void shareNote(int idx)
{
	note = 1;
}

void shareCallback(void *key)
{
	void *dat = 0; int sub = 0;
	datxPrefix("V"); datxFind(&dat,key); sub = *datxIntz(0,dat);
	if (wrap[sub].nxt == args) {wrap[sub].nxt = wake; wake = sub;}
}
int shareExec(const char *exe)
{
	int idx = openFork();
	struct Argument arg = {0};
	char *str = 0;
	int len = 0;
	struct stat new;
	if (openCheck(idx) == -1) return idx;
	arg.typ = Processs;
	if (stat(exe,&new) == 0) for (enum Process i = 0; i < Processs; i++) {
		struct stat old;
		if (stat(Execname__Process__Str(i),&old) != 0) continue;
		if (new.st_dev == old.st_dev && new.st_ino == old.st_ino) {arg.typ = i; break;}}
	arg.inp = openRdfd(idx);
	arg.out = openWrfd(idx);
	datxStr(&dat1,""); writeArgument(&arg,idx1);
	readType(&str,&len,identType("Argument"),idx1);
	return openExec(exe,str);
}
void shareLoop(int src, int dst, int stp, int dtp)
{
	// TODO convert from any type to Str Dat or Generic
	// TODO or convert from Str Dat or Generic to any type
}
void shareWrap(struct Wrap *ptr)
{
	switch (ptr->tag) {
	case (Fanout): {
		shareLoop(ptr->idx,ptr->dst[ptr->sub]->idx,ptr->inp,ptr->dst[ptr->sub]->out);
		if (++ptr->sub == ptr->siz) ptr->sub = 0;
		break;}
	case (Combine): {
		datxEval(&dat0,ptr->exp,ptr->dst[ptr->sub]->out);
		shareLoop(idx0,ptr->dst[ptr->sub]->idx,identType("Dat"),ptr->dst[ptr->sub]->out);
		if (++ptr->sub == ptr->siz) ptr->sub = 0;
		break;}
	case (Buffer): {
		datxStr(&dat0,""); loopType(ptr->inp,ptr->idx,idx0);
		datxStr(&dat1,ptr->str); datxInsert(dat1,dat0);
		break;}
	case (Execute): ERROR();
	default: ERROR();}
}

void shareArgs(int sub, const char *str)
{
	args++;
}
void shareVals(int sub, const char *str)
{
	struct Wrap *ptr = &wrap[sub];
	struct Stage arg = {0}; int len = 0; hideStage(&arg,str,&len);
	// TODO open pipe in idx
	// TODO map str to sub
	// TODO increment vals
}
void shareRefs(int sub, const char *str)
{
	struct Wrap *ptr = &wrap[sub];
	struct Stage arg = {0}; int len = 0; hideStage(&arg,str,&len);
	// TODO increment refs
}
void shareBack(int sub, const char *str)
{
	struct Wrap *ptr = &wrap[sub];
	struct Stage arg = {0}; int len = 0; hideStage(&arg,str,&len);
	// TODO initialize back[refs]
}
void shareNone(int typ, const char *str)
{
}
void shareConst(int typ, const char *str)
{
	int len = 0;
	writeType(str,&len,typ,wrap[0].idx);
}
void shareMessage(int len, const char *str)
{
	fprintf(stderr,"ERROR: invalid type: %s\n",str);
	fprintf(stderr,"---------------------");
	for (int j = 0; j < len; j++) fprintf(stderr,"-");
	fprintf(stderr,"^\n");
	exit(-1);
}
void shareError(int len, const char *str)
{
	ERROR();
}

int sharePeek(const char *str, int *len)
{
	for (int typ = 0; identSubtype(typ,0)!=-1; typ++) {
		int tmp = 0;
		note = 0;
		writeType(str,&tmp,typ,idx0);
		flushBuf(idx0);
		if (tmp > *len) *len = tmp;
		if (note == 0) return typ;}
	return -1;
}
void shareParse(int argc, char **argv, sftype err, sftype arg, sftype stg)
{
	for (int i = 1, dne = 0, sub = 0; i < argc; i++) {
		int len = 0; int typ = 0;
		if (strcmp(argv[i],"--") == 0) dne = 1;
		else if ((typ = sharePeek(argv[i],&len)) < 0) err(len,argv[i]);
		else if (dne || typ != identType("Stage")) arg(typ,argv[i]);
		else stg(sub,argv[i]); sub += 1;}
}

int main(int argc, char **argv)
{
	// prefix "P" for pipe name to wrap index
	// prefix "V" for value name to back index
	noteFunc(shareNote);
	idx0 = puntInit(0,0,shareReadFp,shareWriteFp);
	idx1 = puntInit(0,0,shareReadFp,shareWriteFp);
	shareParse(argc,argv,shareMessage,shareNone,shareArgs);
	wrap = malloc(args*sizeof(struct Wrap)+1);
	shareParse(argc,argv,shareError,shareNone,shareVals);
	back = malloc(vals*sizeof(int*)); refs = malloc(vals*sizeof(int));
	shareParse(argc,argv,shareError,shareConst,shareRefs);
	for (int i = 0; i < vals; i++) {back[i] = malloc(refs[i]*sizeof(int)); refs[i] = 0;}
	shareParse(argc,argv,shareError,shareNone,shareBack);
	// TODO initialize wrap[args]
	datxPrefix("V"); datxCallback(shareCallback);
	wake = args; for (int i = 0; i < args; i++) wrap[i].nxt = args;
	while (1) { // TODO think of way to terminate
		int idx = 0;
		if (wake < args) {idx = wake; wake = wrap[idx].nxt; wrap[idx].nxt = args;}
		else {idx = (int)(intptr_t)*userIdent(waitRead(0,-1));}
		shareWrap(&wrap[idx]);}
	// TODO print each from each wrap[i].idx and wrap[args].idx
	return 0;
}
