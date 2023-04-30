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
int unique = 0;
struct Wrap {
	int idx; // arg reads from here and writes to idx of the next
	int nxt; // next if this is on wake list
	struct Stage arg;
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

int shareFind(const char *dim, const char *str)
{
	void *pre = 0; void *dat = 0; int val = 0;
	datxStr(&dat0,""); writeStr(str,idx0);
	datxStr(&pre,dim);
	datxJoin(&dat,pre,dat0);
	val = *datxIntz(0,datxFind(dat));
	free(pre);
	free(dat);
	return val;
}
void shareInsert(const char *dim, const char *str, int val)
{
	void *pre = 0; void *dat = 0;
	void *tmp = malloc(sizeof(int)+sizeof(int));
	datxStr(&dat0,""); writeStr(str,idx0);
	*(int*)tmp = sizeof(int);
	*datxIntz(0,tmp) = val;
	datxStr(&pre,dim);
	datxJoin(&dat,pre,dat0);
	datxInsert(dat,tmp);
	free(pre);
	free(dat);
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
void shareLoop(int src, int dst, int typ, int oth)
{
	// TODO convert from any type to Str Dat or Generic
	// TODO or convert from Str Dat or Generic to any type
}
void shareStage(struct Stage *ptr, int src, int dst, int typ)
{
	// TODO process stage, writing typ to dst, writing "P" arg.typ to idx, writing ptr->typ from src to "V"
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
void shareCallback(void *dat)
{
	int sub = (datxPrefix("V"),*datxIntz(0,datxFind(dat)));
	if (wrap[sub].nxt == args) {wrap[sub].nxt = wake; wake = sub;}
}

int main(int argc, char **argv)
{
	// prefix "P" for pipe name to wrap index
	// prefix "V" for value name to back index
	// prefix "U" for Dat to unique++
	noteFunc(shareNote);
	idx0 = puntInit(0,0,shareReadFp,shareWriteFp);
	idx1 = puntInit(0,0,shareReadFp,shareWriteFp);
	for (int i = 1, dne = 0; i < argc; i++) {
		int len = 0; int typ = 0;
		if (strcmp(argv[i],"--") == 0) dne = 1;
		else if ((typ = sharePeek(argv[i],&len)) < 0);
		else if (dne || typ != identType("Stage"));
		else args++;}
	wrap = malloc(args*sizeof(struct Wrap)+1);
	for (int i = 1, dne = 0, sub = 0; i < argc; i++) {
		int len = 0; int typ = 0;
		if (strcmp(argv[i],"--") == 0) dne = 1;
		else if ((typ = sharePeek(argv[i],&len)) < 0) {
			fprintf(stderr,"ERROR: invalid type: %s\n",argv[i]);
			fprintf(stderr,"---------------------");
			for (int j = 0; j < len; j++) fprintf(stderr,"-");
			fprintf(stderr,"^\n");
			exit(-1);}
		else if (dne || typ != identType("Stage")) {
			len = 0; writeType(argv[i],&len,typ,wrap[0].idx);}
		else {
			len = 0; hideStage(&wrap[sub].arg,argv[i],&len); sub += 1;}}
	// TODO initialize wrap[args]
	for (int i = 0; i < args; i++) {
	void *dat0 = 0; void *dat1 = 0;
	datxStr(&dat0,wrap[i].arg.str); datxInt(&dat1,i);
	datxPrefix("P"); datxInsert(dat0,dat1);}
	for (int i = 0; i < args; i++) if (wrap[i].arg.tag == Combine) for (int j = 0; j < wrap[i].arg.num; j++) {
	void *dat0 = 0; datxStr(&dat0,wrap[i].arg.dep[j]); datxPrefix("V");
	if (datxFind(dat0) == 0) {void *dat1 = 0; datxInt(&dat1,vals++); datxInsert(dat0,dat1);}}
	back = malloc(vals*sizeof(int*)); refs = malloc(vals*sizeof(int));
	for (int i = 0; i < vals; i++) {back[i] = 0; refs[i] = 0;}
	for (int i = 0; i < args; i++) if (wrap[i].arg.tag == Combine) for (int j = 0; j < wrap[i].arg.num; j++) {
	void *dat0 = 0; datxStr(&dat0,wrap[i].arg.dep[j]); datxPrefix("V");
	refs[*datxIntz(0,datxFind(dat0))]++;}
	for (int i = 0; i < vals; i++) {back[i] = malloc(refs[i]*sizeof(int)); refs[i] = 0;}
	for (int i = 0; i < args; i++) if (wrap[i].arg.tag == Combine) for (int j = 0; j < wrap[i].arg.num; j++) {
	void *dat0 = 0; datxStr(&dat0,wrap[i].arg.dep[j]); datxPrefix("V");
	int sub = *datxIntz(0,datxFind(dat0)); back[refs[sub]++][sub] = i;}
	datxCallback(shareCallback);
	wake = args; for (int i = 0; i < args; i++) wrap[i].nxt = args;
	while (1) { // TODO think of way to terminate
		if (wake < args) {
		if (wrap[wake].arg.tag != Combine) ERROR();
		void *dat = (datxPrefix("V"),datxEval(wrap[wake].arg.exp,wrap[wake+1].arg.typ));
		int next = wrap[wake].nxt; wrap[wake].nxt = args; wake = next;
		assignDat(&dat0,dat); shareLoop(idx0,wrap[wake+1].idx,identType("Dat"),wrap[wake+1].arg.typ);} else {
		int idx = (int)(intptr_t)*userIdent(waitRead(0,-1));
		shareStage(&wrap[idx].arg,wrap[idx].idx,wrap[idx+1].idx,wrap[idx+1].arg.typ);}}
	// TODO print each from each wrap[i].idx and wrap[args].idx
	return 0;
}
