#include "face.h"
#include "type.h"
#include "datx.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int idx = 0;
void *dat = 0;
int note = 0;

int shareRead(int fildes, void *buf, int nbyte)
{
	memcpy(buf,datxData(dat),*(int*)dat);
	return 0;
}
int shareWrite(int fildes, const void *buf, int nbyte)
{
	dat = realloc(dat,nbyte+sizeof(int));
	*(int*)dat = nbyte;
	memcpy(datxData(dat),buf,nbyte);
	return 0;
}
void shareNote(int idx)
{
	note = 1;
}
int sharePeek(const char *str, int *len)
{
	for (int typ = 0; identSubtype(typ,0)!=-1; typ++) {
		int tmp = 0;
		note = 0;
		writeType(str,typ,idx);
		// TODO add &tmp to writeType to indicate how much of str consumed
		if (tmp > *len) *len = tmp;
		if (note == 0) return typ;}
	return -1;
}
void **shareEntry(struct Queue *que, int idx)
{
	if (idx >= que->lim) return 0;
	return &que->dat[(que->fst+idx) % que->siz];
}
int *shareType(struct Queue *que, int idx)
{
	if (idx >= que->lim) return 0;
	return &que->typ[(que->fst+idx) % que->siz];
}
void shareAlloc(struct Queue *que, int idx)
{
	while (idx >= que->lim) {
	if (que->lim == que->siz) {
	void **dat = 0;
	int *typ = 0;
	int siz = que->siz ? que->siz*2 : 1;
	allocDat(&dat,siz);
	allocInt(&typ,siz);
	for (int j = 0; j < que->lim; j++) {
	dat[j] = *shareEntry(que,j);
	typ[j] = *shareType(que,j);}
	for (int j = que->lim; j < siz; j++) {
	dat[j] = 0; typ[j] = -1;}
	allocDat(&que->dat,0);
	allocInt(&que->typ,0);
	que->dat = dat;
	que->typ = typ;
	que->fst = 0;
	que->siz = siz;}
	que->lim += 1;}
}
void shareEnque(struct Queue *que, void *dat, int typ)
{
	int sub = que->lim + 1;
	shareAlloc(que,sub);
	assignDat(shareEntry(que,sub),dat);
	*shareType(que,sub) = typ;
}
void shareDeque(struct Queue *que, void **dat, int *typ)
{
	if (que->lim == 0) ERROR();
	assignDat(dat,*shareEntry(que,0));
	*typ = *shareType(que,0);
	que->lim -= 1;
	que->fst = (que->fst+1) % que->siz;
}
int shareStage(struct Queue *que, struct Stage *ptr);
void sharePipe(struct Queue *que, struct Pipe *ptr)
{
	for (int sub = 0, val = 1; sub < ptr->siz && sub >= 0; sub += val) {
	if (val == 1) while (que->lim > 0) {
	void *dat = 0; int typ = 0;
	shareDeque(que,&dat,&typ);
	shareEnque(&ptr->que[sub],dat,typ);}
	val = shareStage(que,&ptr->stg[sub]);}
}
int shareStage(struct Queue *que, struct Stage *ptr)
{
	// TODO deque as many from ptr->que as needed to enque the required to que
	return 0; // TODO return -1 for more in ptr->que; return 1 for stage done
}

int main(int argc, char **argv)
{
	struct Pipe pipe = {0};
	struct Queue queue = {0};
	int done = 0; int sub = 0;
	int len = 0; int typ = 0;
	char *str = 0;
	noteFunc(shareNote);
	idx = buntInit(0,0,shareRead,shareWrite);
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i],"--") == 0) done = 1;
		else if ((len = 0, typ = sharePeek(argv[i],&len)) < 0);
		else if (done || typ != identType("Stage"));
		else pipe.siz++;}
	allocStage(&pipe.stg,pipe.siz);
	allocQueue(&pipe.que,pipe.siz);
	for (int i = 1; i < argc; i++) {
		if (strcmp(argv[i],"--") == 0) done = 1;
		else if ((len = 0, typ = sharePeek(argv[i],&len)) < 0) {
			fprintf(stderr,"ERROR: invalid type: %s\n",argv[i]);
			fprintf(stderr,"---------------------");
			for (int j = 0; j < len; j++) fprintf(stderr,"-");
			fprintf(stderr,"^\n");
			exit(-1);}
		else if (done || typ != identType("Stage")) {
			shareEnque(&queue,dat,typ);}
		else {
			len = 0; hideStage(&pipe.stg[sub],argv[i],&len); sub += 1;}}
	sharePipe(&queue,&pipe);
	for (int i = 0; i < queue.siz; i++) {
		assignDat(dat,queue.dat[i]);
		readType(&str,queue.typ[i],idx);
		printf("%s\n",str);}
	return 0;
}
