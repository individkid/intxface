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
	int src;
	int dst;
	int psub;
	int psiz;
	int bsiz;
	struct Wrap **plst; // those that depend on pipe indicated by src
	struct Wrap **blst; // those that depend on buffer indicated by arg 
	struct Stage arg;
};
int btodo = 0;

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
void shareStage(struct Stage *ptr)
{
	// TODO process stage, possibly setting btodo
}
void shareEval(struct Express *exp, const char *str)
{
	// TODO evaluate and insert Dat to str
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

int main(int argc, char **argv)
{
	struct Wrap *array = 0;
	int limit = 0;
	int index = 0;
	noteFunc(shareNote);
	idx0 = puntInit(0,0,shareReadFp,shareWriteFp);
	idx1 = puntInit(0,0,shareReadFp,shareWriteFp);
	for (int i = 1, dne = 0; i < argc; i++) {
		int len = 0; int typ = 0;
		if (strcmp(argv[i],"--") == 0) dne = 1;
		else if ((typ = sharePeek(argv[i],&len)) < 0);
		else if (dne || typ != identType("Stage"));
		else limit++;}
	array = malloc(limit*sizeof(struct Wrap));
	array[0].src = openPipe();
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
			len = 0; writeType(argv[i],&len,typ,array[0].src);}
		else {
			len = 0; hideStage(&array[sub].arg,argv[i],&len); sub += 1;}}
	// TODO count psiz and bsiz for each of array
	for (int i = 0; i < limit; i++) {
		array[i].plst = malloc(array[i].psiz*sizeof(struct Wrap *)); array[i].psiz = 0;
		array[i].blst = malloc(array[i].bsiz*sizeof(struct Wrap *)); array[i].bsiz = 0;}
	// TODO put each of array in plst or blst of others in array
	while (1) { // TODO think of way to terminate
		for (int i = 0; btodo && i < array[index].bsiz; i++, btodo--) {
			struct Stage *ptr = &array[index].blst[i]->arg;
			if (ptr->tag != Combine) ERROR();
			for (int j = 0; j < ptr->num; j++) {
				shareEval(&ptr->exp[j],ptr->val[j]);}}
		index = (int)(intptr_t)*userIdent(waitRead(0,-1));
		shareStage(&array[index].arg);}
	return 0;
}
