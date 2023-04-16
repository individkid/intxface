#include "face.h"
#include "type.h"
#include "datx.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

struct Queue {
	int msb;
	int siz;
	int fst;
	int *typ;
	void **dat;
};

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
int shareType(const char *str, int *len)
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
void **planeEntry(struct Queue *que, int idx)
{
	if (idx >= que->siz) return 0;
	return &que->dat[(que->fst+idx) % (1<<que->msb)];
}
int *planeType(struct Queue *que, int idx)
{
	if (idx >= que->siz) return 0;
	return &que->typ[(que->fst+idx) % (1<<que->msb)];
}
void planeAlloc(struct Queue *que, int idx)
{
	while (idx >= que->siz)
	if (que->siz == 1<<que->msb) {
	void **dat = 0;
	int *typ = 0;
	int msb = que->msb+1;
	allocDat(&dat,1<<msb);
	allocInt(&typ,1<<msb);
	for (int j = 0; j < que->siz; j++) {
	dat[j] = *planeEntry(que,j);
	typ[j] = *planeType(que,j);}
	for (int j = que->siz; j < 1<<msb; j++) {
	dat[j] = 0; typ[j] = -1;}
	allocDat(&que->dat,0);
	allocInt(&que->typ,0);
	que->dat = dat;
	que->typ = typ;
	que->fst = 0;
	que->msb = msb;
	que->siz += 1;}
}
void planeEnque(struct Queue *que, void *dat, int typ)
{
	int sub = que->siz++;
	planeAlloc(que,sub);
	assignDat(planeEntry(que,sub),dat);
	*planeType(que,sub) = typ;
}
void planeDeque(struct Queue *que, void **dat, int *typ)
{
	if (que->siz == 0) ERROR();
	assignDat(dat,*planeEntry(que,0));
	*typ = *planeType(que,0);
	que->siz -= 1;
	que->fst = (que->fst+1) % (1<<que->msb);
}
int runStage(struct Queue *dst, struct Queue *src, const struct Stage *ptr)
{
	// TODO deque as many from src as needed to enque the required to dst
	return 0; // TODO return -1 for more in src; return 1 for stage done
}
void runPipe(struct Queue *dst, struct Queue *src, const struct Stage *ptr)
{
	int sub = 0;
	int siz = 0;
	const struct Stage **lst = 0;
	struct Queue *que = 0;
	for (const struct Stage *tmp = ptr; tmp; tmp = tmp->nxt) siz++;
	lst = malloc(siz*sizeof(struct Stage*));
	que = malloc(siz*sizeof(struct Queue));
	for (const struct Stage *tmp = ptr; tmp; tmp = tmp->nxt) lst[sub++] = tmp;
	planeAlloc(&que[0],src->siz);
	sub -= 1; while (sub < siz) {
	int val = runStage(dst,&que[sub],lst[sub]);
	if (val == 1) while (dst->siz > 0) {
	void *dat = 0;
	int typ = 0;
	planeDeque(dst,&dat,&typ);
	planeEnque(&que[sub+1],dat,typ);}
	sub += val;}
}

int main(int argc, char **argv)
{
	struct Stage *start = 0;
	struct Stage **next = &start;
	struct Queue dst = {0};
	struct Queue src = {0};
	int done = 0;
	noteFunc(shareNote);
	idx = buntInit(0,0,shareRead,shareWrite);
	for (int i = 1; i < argc; i++) {
		int len = 0;
		int typ = 0;
		if (strcmp(argv[i],"--") == 0) {done = 1; continue;}
		typ = shareType(argv[i],&len);
		if (typ < 0) {
			fprintf(stderr,"ERROR: invalid type: %s\n",argv[i]);
			fprintf(stderr,"---------------------");
			for (int j = 0; j < len; j++) fprintf(stderr,"-");
			fprintf(stderr,"^\n");
			exit(-1);}
		else if (done || typ != identType("Stage")) {
			planeEnque(&src,dat,typ);}
		else {
			allocStage(next,1);
			len = 0; hideStage(*next,argv[i],&len);
			next = &(*next)->nxt;}}
	runPipe(&dst,&src,start);
	for (int i = 0; i < dst.siz; i++) {
		char *str = 0;
		assignDat(dat,dst.dat[i]);
		readType(&str,dst.typ[i],idx);
		printf("%s\n",str);}
	return 0;
}
